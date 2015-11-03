-module(kennel_h).

-export([init/2]).

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
            State3 = case cowboy_req:method(Req) of
                         <<"POST">> ->
                             State2#{method => post};
                         <<"PUT">> ->
                             State2#{method => put};
                         <<"GET">> ->
                             State2#{method => get}
                     end,
            validate_id(Req, State3);
        _ ->
            Req2 = cowboy_req:reply(
                     401, [
                           {<<"WWW-Authenticate">>, <<"ssl-client-cert">>}
                          ], "", Req),
            {ok, Req2, State}
    end.

validate_id(Req, State = #{id := ID}) ->
    case ls_vm:get_docker(ID) of
        {ok, Obj} ->
            UUID = ft_vm:uuid(Obj),
            State1 = State#{vm => Obj, uuid => UUID},
            check_permission(Req, State1);
        _ ->
            Req2 = cowboy_req:reply(404, [], "", Req),
            {ok, Req2, State}
    end;

validate_id(Req, State) ->
    check_permission(Req, State).


check_permission(Req, State) ->
    lager:warning("[TODO] Check permissions"),
    check_method(Req, State).

check_method(Req, State = #{method := Verb, handler := H}) ->
    case erlang:function_exported(H, Verb, 2) of
        true ->
            run(Req, State);
        false ->
            lager:warning("Method ~s not allowed in handler ~s", [Verb, H]),
            Req2 = cowboy_req:reply(
                     405, [
                           {<<"Allowed">>, methods(H)}
                          ], "", Req),
            {ok, Req2, State}
    end.

run(Req, State = #{method := Verb, handler := H}) ->
    lager:info("Call: ~s:~s", [H, Verb]),
    case H:Verb(Req, State) of
        {ok, JSON, Req2, State1} ->
            Req3 = cowboy_req:reply(
                     200, [
                           {<<"content-type">>, <<"application/json">>}
                          ], jsone:encode(JSON), Req2),
            {ok, Req3, State1};
        E ->
            E
    end.

methods(H) ->
    <<",", R/binary>> = methods(H, [get, post, put], <<>>),
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
