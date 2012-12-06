-module(blade).
-author("Carlos Eduardo").

%-export().
-compile(export_all).

start() ->
    L1 = logger:start(l1),
    Q1 = bill_queue:start(q1),
    R1 = rating:start_instance(rating1, Q1, [moc]),
    register(l1, L1),
    register(r1, R1),
    register(q1, Q1).

stop() ->
    rating:stop_instance(r1),
    q1 ! {self(), exit},
    l1 ! {self(), exit},
    unregister(q1),
    unregister(l1).

restart() ->
    stop(),
    start().
    
%% Test Functions
bill_ticket(A, B, Dur) ->
    bill_ticket(q1, A, B, Dur).

bill_ticket(Q, A, B, Dur) ->
    Q ! {self(), {put, {l1, {moc, A, B, Dur}}}},
    receive
        {ok, T} ->
            T
    end.

bench(N) ->
    Q1 = bill_queue:start(q1),
    lists:foreach(fun(X) -> bill_ticket(Q1, "1185236565", "1185236565", X) end, lists:seq(1,N)),
    R1 = rating:bench_rating(rating1, Q1, [moc], N).   
    
populate_queue(N) ->
    lists:foreach(fun(X) -> bill_ticket(q1, "1185236565", "1185236565", X) end, lists:seq(1,N)).
