-module(maxSpeakers).
-compile(export_all).

benchmark([N]) ->
    process_flag(priority, max),
    Start = erlang:system_time(millisecond),
    Monitor = spawn(?MODULE, aggregator, [0, erlang:system_time(millisecond)]),
    NumOfServers = element(1, string:to_integer(N)),
    Servers = serverSpawner(Monitor, NumOfServers, bytes_generate(500)),
    End = erlang:system_time(millisecond),
    io:format("Time taken to create servers: ~f seconds~n", [(End-Start)/1000]),
    Start2 = erlang:system_time(millisecond),

    [Pid ! "Start" || Pid <- Servers],
    %wait for the processes to finish
    timer:sleep(1000*6),
    Monitor ! "Show",
    receive
        "Done" ->
            End2 = erlang:system_time(millisecond),
            io:format("Time taken to do the work: ~f seconds~n", [(End2-Start2)/1000])
    end.

serverSpawner(Monitor, 0, D) -> [];

serverSpawner(Monitor, N, D) when N>0 ->
    [spawn(?MODULE, server, [Monitor, clientSpawner(1), 0, D]) | serverSpawner(Monitor, N-1, D) ].    

clientSpawner(0) -> [];

clientSpawner(N) when N>0 ->
    [spawn(?MODULE, client, []) | clientSpawner(N-1) ].  

server(Monitor, Clients, 100, Data) ->
    Monitor ! 100,
    server(Monitor, Clients, 0, Data);

server(Monitor, Clients, MessagesCount, Data) ->
    receive
        "Start" ->
            [Pid ! {"Hello",self(), Data} || Pid <- Clients],
            server(Monitor, Clients, MessagesCount, Data);
        {"Done", Pid, Data} ->
            Pid ! {"Hello",self(), Data},
            server(Monitor, Clients, MessagesCount + 1, Data);
        _ ->
            io:format("I don't understand.~n")
    end.

client() ->
    receive
        {"Hello", Pid, Data} ->
            Pid ! {"Done", self(), Data},
            client();
        _ ->
            io:format("I don't understand.~n")
    end.

aggregator(N, StartTime) ->
    process_flag(priority, max),
    receive
        "Show" ->
            io:format("Time: ~f || Count: ~w.~n", [(erlang:system_time(millisecond) - StartTime)/1000,N]);
        MessagesCount ->
            aggregator(N+MessagesCount, StartTime)
    end.

bytes_generate(Size) ->
    bytes_generate(Size, []).

bytes_generate(0, Bytes) ->
    list_to_binary(Bytes);
bytes_generate(Size, Bytes) ->
    bytes_generate(Size - 1, [1 | Bytes]).