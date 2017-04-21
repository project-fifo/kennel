%% This is going to become messy, somehow we need to convert
%% a HTTP connection to a plain TCP one, that might work
%% but there will be ugy hackery involved.
%%
%% I want to go on the record and say it's a horrible idea.
-module(kennel_attach_h).
-behaviour(kennel_h).

-export([permission/1, post/2, upgrade/6]).

-ignore_xref([upgrade/6]).

-define(TRIES, 60).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"console">>].

post(Req, State) ->
    %%lets grab the socket, so we can do stuff wiht it.
    {kennel_attach_h, Req, State}.

upgrade(Req, _Env, _Handler, State = #{uuid := UUID}, infinity, run) ->
    Socket = cowboy_req:get(socket, Req),
    Reply = <<"HTTP/1.1 101 UPGRADED\n"
              "Content-Type: application/vnd.docker.raw-stream\n"
              "Connection: Upgrade\n"
              "Upgrade: tcp\n\n">>,
    ssl:send(Socket, Reply),
    case ensure_running(UUID, Socket, ?TRIES) of
        {ok, VM} ->
            HID = ft_vm:hypervisor(VM),
            {ok, H} = ls_hypervisor:get(HID),
            {Host, Port} = ft_hypervisor:endpoint(H),
            {ok, Console} = libchunter:console_open(Host, Port, UUID, self()),
            ssl:setopts(Socket, [{active, once}]),
            loop(Socket, Console, Req);
        {error, E} ->
            {error, E, Req, State}
    end.

is_running(V) ->
    (ft_vm:state(V) =:= <<"running">>)
        and (ft_vm:creating(V) =:= false).

ensure_running(UUID, _Socket, 1) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            case is_running(V) of
                true ->
                    {ok, V};
                _ ->
                    {error, timeout}
            end;
        not_found ->
            {error, not_found}
    end;

ensure_running(UUID, Socket, T) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            case is_running(V) of
                true ->
                    {ok, V};
                _ ->
                    timer:sleep(1000),
                    ensure_running(UUID, Socket, T - 1)
            end;
        not_found ->
            timer:sleep(1000),
            ensure_running(UUID, Socket, T - 1)
    end.

loop(Socket, Console, Req) ->
    receive
        %% We seem to need to strip this, not sure why it's
        %% ugly.
        {data, <<27, Data/binary>>} ->
            ssl:send(Socket, stdout(Data)),
            loop(Socket, Console, Req);
        {data, Data} ->
            ssl:send(Socket, stdout(Data)),
            loop(Socket, Console, Req);
        {ssl, Socket, Data} ->
            libchunter_console_server:send(Console, Data),
            ssl:setopts(Socket, [{active, once}]),
            loop(Socket, Console, Req);
        {ssl_closed, Socket} ->
            lager:info("Socket closed"),
            {stop, Req};
        {ssl_error, Socket, Reason} ->
            lager:error("Socket Error: ~p", [Reason]),
            {stop, Req}
    end.

stdout(Data) ->
    Size = byte_size(Data),
    <<1, 0, 0, 0, Size:32/big-unsigned-integer, Data/binary>>.
