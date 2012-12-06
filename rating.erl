-module(rating).
-author("Carlos Eduardo").
-import(re).

-include("blade.hrl").

% Exports for management functions
-export([start_instance/3, stop_instance/1, reload/3]).

% Exports for benchmark functions
-export([bench_rating/4]).

% Exports for spawned/timed functions
-export([bench_loop/5, rate_call/3, rating_loop/4, load_tariffs/1]).

% For Debug
%-compile(export_all).

%%====================================================================
%% Management functions
%%====================================================================

start_instance(Name, Queue, Calltypes) ->
    ?INFO("Starting instance " ++ atom_to_list(Name)),
    start_rating(Name, Queue, Calltypes).

stop_instance(Name) ->
    Name ! {self(), exit, "Shutdown"}.

reload(Instance, Queue, Calltypes) ->
    Instance ! {self(), exit, "Shutdown"},
    start_rating(Instance, Queue, Calltypes).

%%====================================================================
%% Internal functions
%%====================================================================

load_tariffs([C|T]) ->
    TableZone = ets:new(zone, [ordered_set]),
    TableMap = ets:new(map, [ordered_set]),
    %% Read database of zones
    case file:read_file(atom_to_list(C)++"ZoneMap") of
        {ok, Data} ->
            compile_zones(0, parse_files:parse_tsv(Data), TableZone);
            %error_logger:info_msg("Loaded Zones ~p~n", [CompData]);
        {error, Why} ->
            error_logger:error_msg(Why)
    end,
    %% Read database of maps
    case file:read_file(atom_to_list(C)++"TariffMap") of
        {ok, Data2} ->
            compile_maps(0, parse_files:parse_tsv(Data2), TableMap);
            %error_logger:info_msg("Loaded Zones ~p~n", [Data2]);
        {error, Why2} ->
            error_logger:error_msg(Why2)
    end,
    [{C, {TableZone, TableMap}}|load_tariffs(T)];
load_tariffs([]) ->
    [].

compile_zones(N, [{Reg, Z}|T], Table) ->
    {ok, Re} = re:compile(Reg),
    ets:insert(Table, {N, Z, Re}),
    compile_zones(N+1, T, Table);
compile_zones(_, [],_) ->
    [];
compile_zones(_, [{[]}], _) ->
    [].

compile_maps(N, [{Z1, _, Z2, _, Cost}|T], Table) ->
    {ok, Re1} = re:compile(Z1, [anchored]),
    {ok, Re2} = re:compile(Z2, [anchored]),
    ets:insert(Table, {N, Re1, Re2, Cost}),
    compile_maps(N+1, T, Table);
compile_maps(_, [], _) ->
    [];
compile_maps(_, [{[]}], _) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Benchmark functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
bench_rating(Instance, Q, Calltypes, Qtd) ->    
    Tariffs = load_tariffs(Calltypes),
    error_logger:info_msg("Starting rating engine ~p~n", [{Instance, Calltypes}]),
    Start = now(),
    M = self(),
    spawn(?MODULE, bench_loop, [M, Q, Instance, Calltypes, Tariffs]),
    receive
        over ->
            T = timer:now_diff(now(), Start)/1000000,
            X = Qtd/T,
            Y = T/Qtd,
            error_logger:info_msg("End processing ~p requests in: ~p secs. ~p reqs/sec. ~p secs/req.~n", [Qtd, T, X, Y]),
            Q ! {self(), exit}
    end.

bench_loop(M, Q, Instance, Calltypes, Tariffs) ->
    S = self(),
    Q ! {S, {get}},
    receive
        {ok, {_From, {Calltype, Anum, Bnum, Duration}}} ->
            spawn(?MODULE, rate_call, [S, Tariffs, {Calltype, Anum, Bnum, Duration}]),
            %rate_call(S, Tariffs, {Calltype, Anum, Bnum, Duration}),
            bench_loop(M, Q, Instance, Calltypes, Tariffs);
        {error, empty} ->
            M ! over,
            Q ! {self(), exit}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
start_rating(Instance, Q, Calltypes) ->
    error_logger:info_msg("Loading tariffs ~p~n", [Calltypes]),
    {Time, Tariffs} = timer:tc(?MODULE, load_tariffs, [Calltypes]),
    error_logger:info_msg("Time to load tariffs: ~p~n", [Time]),
    error_logger:info_msg("Starting rating engine ~p~n", [{Instance, Calltypes}]),
    spawn(?MODULE, rating_loop, [Instance, Q, Calltypes, Tariffs]).

rating_loop(Instance, Q, Calltypes, Tariffs) ->
    Q ! {self(), get},
    receive
        {ok, {From, {Calltype, Anum, Bnum, Duration}}} ->
            spawn(?MODULE, rate_call, [From, Tariffs, {Calltype, Anum, Bnum, Duration}]),
            rating_loop(Instance, Q, Calltypes, Tariffs);
        {error, empty} ->
            timer:sleep(100),
            rating_loop(Instance, Q, Calltypes, Tariffs);
        {_, exit, Why} ->
            error_logger:info_msg("~p exiting, ~p~n", [Instance, Why]);
        X ->
            error_logger:error_msg("Unknown message received ~p~n", [X]),
            rating_loop(Instance, Q, Calltypes, Tariffs)
    end.
%%--------------------------------------------------------------------
%% Function: rate_call(From, [Calltypes], {CallInfo}) ->
%% Description:
%%
%%--------------------------------------------------------------------
rate_call(From, [{Calltype, {Zone, Map}}|_], {Calltype, A, B, _Dur}) ->
    case match_zone(A, Zone) of
        {ok, Azone} ->
            case match_zone(B, Zone) of
                {ok, Bzone} ->
                    case match_map(Map, Azone, Bzone) of
                        {ok, Tariff} ->
                            %TODO Callcost = Tariff/60 * Dur,
                            %error_logger:info_msg("Debug: ~p~n", [{{A, Azone}, {B, Bzone}, Tariff}]);
                            From ! {ok, Tariff};
                        {error, Err} ->
                            error_logger:info_msg("Debug: ~p~n", [{rating_error, Err}])
                    end;
                {error, Err} ->
                    error_logger:info_msg("Debug: ~p~n", [{rating_error, Err}])
            end;
        {error, Err} ->
            error_logger:info_msg("Debug: ~p~n", [{rating_error, Err}])
    end;
rate_call(From, [_|T], Calldata) ->
    rate_call(From, T, Calldata).

%%--------------------------------------------------------------------
%% Function: match_zone([ZoneList], Anumber) -> Zone | no_zone_match
%% Description: Searches the ZoneList for the first match of Anumber.
%% Returns the first matching Zone.
%%--------------------------------------------------------------------
match_zone(Num, TableZone) ->
    match_zone(Num, TableZone, ets:lookup(TableZone, 0)).

match_zone(Num, TableZone, [{N, Zone, Pattern}]) ->
    case re:run(Num, Pattern) of
        {match, _} ->
            {ok, Zone};
        _ ->
            match_zone(Num, TableZone, ets:lookup(TableZone, N+1))
    end;
match_zone(_, _, []) ->
    {error, no_zone_match}.


%%--------------------------------------------------------------------
%% Function: match_map([MapList], AnumZone, BnumZone) -> Tariff | no_map_match
%% Description: Searches the MapList for a combination of Azone and Bzone.
%% Returns the tariff for the combination
%%--------------------------------------------------------------------
match_map(TableMap, A, B) ->
    match_map(TableMap, A, B, ets:lookup(TableMap, 0)).

match_map(TableMap, A, B, [{N, Azone, Bzone, Tar}]) ->
    case re:run(A, Azone) of
        {match, _} ->
            case re:run(B, Bzone) of
                {match, _} ->
                    %error_logger:info_msg("Debug: ~p~n", [{A, B, Tar}]),
                    {ok, Tar};
                _ ->
                    match_map(TableMap, A, B, ets:lookup(TableMap, N+1))
            end;
        _ ->
            match_map(TableMap, A, B, ets:lookup(TableMap, N+1))
    end;
match_map(_, A, B, []) ->
    {error, {no_map_match, {A, B}}}.
