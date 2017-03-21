-module(pingping).
-export([benchmark/2]).

benchmark(R, D) ->
    P1 = spawn(fun() -> pingping(bytes_generate(D)) end),
    P2 = spawn(fun() -> pingping(bytes_generate(D)) end),
    Start = erlang:system_time(millisecond),
    P1 ! {init, self(), P2, R},
    P2 ! {init, self(), P1, R},
    imb:finalize(P1),
    imb:finalize(P2),
    End = erlang:system_time(millisecond),
    io:format("Total time taken: ~f seconds~n", [(End-Start)/1000]).

pingping(Data) ->
    receive
        {init, _From, Dest, R} ->
            Dest ! {self(), R - 1, Data, _From},
            pingping(Data);
        {_From, R, Data, Parent} when R =:= 0 ->
            Parent ! {done, self()};
        {_From, R, Data, Parent} ->
            _From ! {self(), R - 1, Data, Parent},
            pingping(Data)
    end.

bytes_generate(Size) ->
    bytes_generate(Size, []).

bytes_generate(0, Bytes) ->
    list_to_binary(Bytes);
bytes_generate(Size, Bytes) ->
    bytes_generate(Size - 1, [1 | Bytes]).