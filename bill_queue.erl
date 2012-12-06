-module(bill_queue).
-author("Carlos Eduardo").

-export([start/1, billing_queue/1]).
%-compile(export_all).

start(Id) ->
    spawn(?MODULE, billing_queue, [Id]).

billing_queue(Id) ->
    File = "QueueDump_" ++ atom_to_list(Id),
    Q = restore_queue(File),
    error_logger:info_msg("Started billing queue: ~p~n", [Id]),
    billing_queue(Id, Q).
billing_queue(Id, Q) ->
    receive
        {From, {put, Data}} ->
            Q1 = queue:in(Data, Q),
            From ! {ok, received},
            billing_queue(Id, Q1);
        {From, {get}} ->
            case queue:out(Q) of
                {{value, Data}, Q1} ->
                    From ! {ok, Data};
                {empty, _} ->
                    From ! {error, empty},
                    Q1 = Q
            end,
            billing_queue(Id, Q1);
        {From, len} ->
            L = queue:len(Q),
            error_logger:info_msg("Queue length: ~p~n", [L]),
            From ! {ok, L},
            billing_queue(Id, Q);
        {_, exit} ->
            error_logger:info_msg("Billing Queue ~p exiting.~n", [Id]),
            File = "QueueDump_" ++ atom_to_list(Id),
            dump_queue(Q, File),
            {ok, exit}
    end.

unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
    file:close(S).

dump_queue(Q, File) ->
    L = queue:to_list(Q),
    case queue:is_empty(Q) of
        false ->
            unconsult(File, L);
        true ->
            ok
    end.

restore_queue(File) ->
    case file:consult(File) of
        {ok, Data} ->
            Q = queue:from_list(Data),
            error_logger:info_msg("Started queue loading ~p entries.~n", [length(Data)]),
            file:delete(File);
        {error, enoent} ->
            Q = queue:new();
        {error, Why} ->
            error_logger:info_msg("Queue load error: ~p~n", [Why]),
            Q = queue:new()
    end,
    Q.