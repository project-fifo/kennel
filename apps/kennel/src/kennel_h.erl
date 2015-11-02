-module(kennel_h).

-export([init/2]).

init(Req, State = #{handler := H}) ->
    %% TODO propper error
    case kennel:context(Req) of
        {ok, Context} ->
            State1 = State#{ctx => Context},
            case cowboy_req:method(Req) of
                <<"POST">> ->
                    run(post, H, Req, State1);
                <<"PUT">> ->
                    run(put, H, Req, State1);
                <<"GET">> ->
                    run(get, H, Req, State1)
            end;
        _ ->
            Req2 = cowboy_req:reply(
                     401, [
                           {<<"WWW-Authenticate">>, <<"ssl-client-cert">>}
                          ], "", Req),
            {ok, Req2, State}

    end.


run(Verb, H, Req, State) ->
    case erlang:function_exported(H, Verb, 2) of
        true ->
            case H:Verb(Req, State) of
                {ok, JSON, Req2, State2} ->
                    Req3 = cowboy_req:reply(
                             200, [
                                   {<<"content-type">>, <<"application/json">>}
                                  ], jsone:encode(JSON), Req2),
                    {ok, Req3, State2};
                E ->
                    E
            end;
        false ->
            lager:warning("Method ~s not allowed in handler ~s", [Verb, H]),
            Req2 = cowboy_req:reply(
                     405, [
                           {<<"Allowed">>, methods(H)}
                          ], "", Req),
            {ok, Req2, State}
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
