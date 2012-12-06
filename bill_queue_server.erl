%%%-------------------------------------------------------------------
%%% File    : bill_queue_server.erl
%%% Author  : my name <yourname@localhost.localdomain>
%%% Description : 
%%%
%%% Created :  2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(bill_queue_server).
-author("Carlos Eduardo").

-behaviour(gen_server).

%% API
-export([start_link/1, billing_queue/2, put/2, get/1, len/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {id = none}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Id], []).

put(Who, Data) ->
    gen_server:call(?MODULE, {put, {Who, Data}}).

get(Who) ->
    gen_server:call(?MODULE, {get, Who}).

len(Who) ->
    gen_server:call(?MODULE, {len, Who}).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Id]) ->
    process_flag(trap_exit, true),
    File = "QueueDump_" ++ atom_to_list(Id),
    Q = restore_queue(File),
    error_logger:info_msg("Started billing queue: ~p~n", [Id]),
    register(Id, spawn(?MODULE, billing_queue, [Id, Q])),
    {ok, #state{id = Id}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({put, {Who, Data}}, _From, State) ->
    State#state.id ! {Who, {put, Data}},
    receive
        X ->
            Reply = X
    end,
    {reply, Reply, State}.

handle_call({get, Who}, _From, State) ->
    State#state.id ! {Who, get},
    receive
        X ->
            Reply = X
    end,
    {reply, Reply, State}.

handle_call({len, Who}, _From, State) ->
    State#state.id ! {Who, {put, Data}},
    receive
        X ->
            Reply = X
    end,
    {reply, Reply, State}.
    
    
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    error_logger:info_msg("Billing Queue ~p exiting.~n", [State#state.id]),
    File = "QueueDump_" ++ atom_to_list(State#state.id),
    State#state.id ! {exit, File},
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

billing_queue(Id, Q) ->
    receive
        {From, {put, Data}} ->
            Q1 = queue:in(Data, Q),
            From ! {ok, received},
            billing_queue(Id, Q1);
        {From, get} ->
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
        {exit, File} ->
            dump_queue(Q, File)
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
            error_logger:error_msg("Queue load error: ~p~n", [Why]),
            Q = queue:new()
    end,
    Q.
