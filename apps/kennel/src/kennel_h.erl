-module(kennel_h).

-export([init/2]).

-ignore_xref([behaviour_info/1, init/2]).

-type state() :: #{
             vm => map(),
             uuid => binary(),
             method => method(),
             ctx => list(),
             user => binary(),
             scope => binary()
            }.

-type method_reply() ::
        {ok, term(), cowboy:req(), state()} |
        {ok, cowboy:req(), state()} |
        {stop, cowboy:req()} |
        {module(), cowboy:req(), state()}.


-type method() :: get |
                  put |
                  post |
                  delete |
                  update |
                  trace |
                  options |
                  head |
                  connect |
                  undefined.

-callback permission(state()) ->
    true | [binary()].

-callback get(cowboy:req(), state()) ->
    method_reply().

-callback put(cowboy:req(), state()) ->
    method_reply().

-callback post(cowboy:req(), state()) ->
    method_reply().

-callback delete(cowboy:req(), state()) ->
    method_reply().

-callback trace(cowboy:req(), state()) ->
    method_reply().

-callback update(cowboy:req(), state()) ->
    method_reply().

-optional_callbacks([get/2, put/2, post/2, delete/2, trace/2, update/2,
                     permission/1]).


init(Req, State) ->
    %% TODO propper error
    case kennel:context(Req) of
        {ok, Context} ->
            {ok, Scope} = jsxd:get([<<"scope">>], Context),
            {ok, User} = jsxd:get([<<"resource_owner">>], Context),
            State1 = State#{ctx => Context, user => User, scope => Scope},
            State2 = case cowboy_req:binding(id, Req) of
                         undefined ->
                             State1;
                         ID ->
                             State1#{id => ID}
                     end,
            State3 = State2#{method => method_to_atom(cowboy_req:method(Req))},
            validate_id(Req, State3);
        _ ->
            Req2 = cowboy_req:reply(
                     401, #{<<"WWW-Authenticate">> => <<"ssl-client-cert">>}
                          , "", Req),
            {ok, Req2, State}
    end.

method_to_atom(<<"GET">>) ->
    get;
method_to_atom(<<"PUT">>) ->
    put;
method_to_atom(<<"POST">>) ->
    post;
method_to_atom(<<"DELETE">>) ->
    delete;
method_to_atom(<<"UPDATE">>) ->
    update;
method_to_atom(<<"TRACE">>) ->
    trace;
method_to_atom(<<"OPTIONS">>) ->
    options;
method_to_atom(<<"HEAD">>) ->
    head;
method_to_atom(<<"CONNECT">>) ->
    connect;
method_to_atom(_) ->
    undefined.


validate_id(Req, State = #{id := ID}) ->
    case ls_vm:get_docker(ID) of
        {ok, Obj} ->
            UUID = ft_vm:uuid(Obj),
            State1 = State#{vm => Obj, uuid => UUID},
            check_permission(Req, State1);
        _ ->
            Req2 = cowboy_req:reply(404, #{}, "", Req),
            {ok, Req2, State}
    end;

validate_id(Req, State) ->
    check_permission(Req, State).




check_permission(Req, State = #{user := User, handler := H, scope := Scope}) ->
    case erlang:function_exported(H, permission, 1) of
        true ->
            P = H:permission(State),
            case has_permission(User, Scope, P) of
                true ->
                    check_method(Req, State);
                false ->
                    Req2 = cowboy_req:reply(403, #{}, "", Req),
                    {ok, Req2, State}
            end;
        false ->
            check_method(Req, State)
    end.

has_permission(User, Scope, Permission) ->
    libsnarl:allowed(User, Permission) andalso
        scope_allowed(Scope, Permission).

scope_allowed(Scope, Permission) ->
    {ok, Scopes} = ls_oauth:scope(Scope),
    ScopeP = scope_perms(Scopes, []),
    libsnarlmatch:test_perms(Permission, ScopeP).

scope_perms([], Acc) ->
    lists:usort(Acc);

scope_perms([#{permissions := Perms} | R], Acc) ->
    scope_perms(R, Acc ++ Perms).

check_method(Req, State = #{method := Verb, handler := H}) ->
    case erlang:function_exported(H, Verb, 2) of
        true ->
            run(Req, State);
        false ->
            lager:warning("Method ~s not allowed in handler ~s", [Verb, H]),
            Req2 = cowboy_req:reply(
                     405, #{<<"Allowed">> => methods(H)} , "", Req),
            {ok, Req2, State}
    end.

run(Req, State = #{method := Verb, handler := H}) ->
    lager:info("Call: ~s:~s", [H, Verb]),
    case H:Verb(Req, State) of
        {ok, JSON, Req2, State1} ->
            Req3 = cowboy_req:reply(
                     200, #{<<"content-type">> => <<"application/json">>} ,
                     jsone:encode(JSON), Req2),
            {ok, Req3, State1};
        {no_content, Req2, State1} ->
            Req3 = cowboy_req:reply(204, #{}, "", Req2),
            {ok, Req3, State1};
        {error, not_found, Req2, State1} ->
            Req3 = cowboy_req:reply(404, #{}, "", Req2),
            {ok, Req3, State1};
        {error, E, Req2, State1} ->
            Req3 = cowboy_req:reply(503, #{}, atom_to_list(E), Req2),
            {ok, Req3, State1};
        E ->
            E
    end.

methods(H) ->
    <<",", R/binary>> = methods(H, [get, post, put, delete], <<>>),
    R.

methods(H, [Verb | Rest], Acc) ->
    case erlang:function_exported(H, Verb, 2) of
        true ->
            V = list_to_binary(string:to_upper(atom_to_list(Verb))),
            methods(H, Rest, <<Acc/binary, ",", V/binary>>);
        _ ->
            methods(H, Rest, Acc)
    end;

methods(_, [], Acc) ->
    Acc.
