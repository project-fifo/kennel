%% This is going to become messy, somehow we need to convert
%% a HTTP connection to a plain TCP one, that might work
%% but there will be ugy hackery involved.
%%
%% I want to go on the record and say it's a horrible idea.
-module(kennel_attach_h).

-export([permission/1, post/2, upgrade/6]).

permission(#{uuid := VM}) ->
    [<<"vms">>, VM, <<"console">>].

post(Req, State) ->
    %%lets grab the socket, so we can do stuff wiht it.
    {kennel_attach_h, Req, State}.

upgrade(Req, _Env, _Handler, #{uuid := UUID}, infinity, run) ->
    Socket = cowboy_req:get(socket, Req),
    Reply = <<"HTTP/1.1 101 UPGRADED\n"
              "Content-Type: application/vnd.docker.raw-stream\n"
              "Connection: Upgrade\n"
              "Upgrade: tcp\n\n">>,
    ssl:send(Socket, Reply),
    {ok, VM} = ensure_running(UUID, Socket),
    HID = ft_vm:hypervisor(VM),
    {ok, H} = ls_hypervisor:get(HID),
    {Host, Port} = ft_hypervisor:endpoint(H),
    {ok, Console} = libchunter:console_open(Host, Port, UUID, self()),
    ssl:setopts(Socket, [{active, once}]),
    loop(Socket, Console, Req).

ensure_running(UUID, Socket) ->
    {ok, V} = ls_vm:get(UUID),
    Running = (ft_vm:state(V) =:= <<"running">>)
        and (ft_vm:creating(V) =:= false),
    case Running of
        true ->
            ssl:send(Socket, stdout(<<" done\r\n">>)),
            {ok, V};
        _ ->
            ssl:send(Socket, stdout(<<".">>)),
            timer:sleep(1000),
            ensure_running(UUID,Socket)
    end.

loop(Socket, Console, Req) ->
    receive
        %% We seem to need to strip this, not sure why it's
        %% ugly.
        {data, <<27, Data/binary>>} ->
            io:format(">~p~n", [Data]),
            ssl:send(Socket, stdout(Data)),
            loop(Socket, Console, Req);
        {data, Data} ->
            io:format(">~p~n", [Data]),
            ssl:send(Socket, stdout(Data)),
            loop(Socket, Console, Req);
        {ssl, Socket, Data} ->
            io:format("<~p~n", [Data]),
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
