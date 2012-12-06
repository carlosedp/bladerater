-module(logger).
-author("Carlos Eduardo").

-export([start/1, logger/1]).

start(Id) ->
    spawn(logger, logger, [Id]).
    
logger(Id) ->    
    receive
        {ok, T} ->
            error_logger:info_msg("Received ticked ~p.~n", [T]),
            logger(Id);
        {_, exit} ->
            error_logger:info_msg("Logger ~p exiting.~n", [Id]),
            {ok, exit}
    end.
    