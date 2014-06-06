%%%-------------------------------------------------------------------
%%% File    : website_scanner_server 
%%% Author  : Roy Goldstein Almaliach <roy.almaliach@sumup.com>
%%% Description : 
%%%
%%% Created :  8.4.2014 by Roy Goldstein Almaliach
%%%-------------------------------------------------------------------

-module(website_scanner_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

%% supplement functions
-export([get_website_source_code/1, transfer_info_to_dictionary/1, stop/0,dump_link_list/0]).


-define (SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    ibrowse:start(),
    dictionary_server:start_link(),
	TableId = ets:new(link_list, [ordered_set]),
    SearchLevel = 0,
    Scanned_links = [],
    State = {TableId, SearchLevel, Scanned_links},
    {ok, State}.

%%--------------------------------------------------------------------
%%% supplement functions
%%--------------------------------------------------------------------
stop()      ->  gen_server:call(?MODULE, stop).

%get_website_source_code(Link_list)     			  ->  gen_server:call(?MODULE, {get_website_source_code, Link_list}).
transfer_info_to_dictionary({Host, Link_list, Word_list})  ->  gen_server:cast(?MODULE, {transfer_info_to_dictionary, Host, Link_list, Word_list}).
get_website_source_code(Link_list)                ->  gen_server:cast(?MODULE, {get_website_source_code, Link_list}).
dump_link_list()                                  ->  gen_server:cast(?MODULE, {dump_link_list}).
    
%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------






handle_call(stop, _From, State) ->
    Reply = stoped,
	{stop, normal, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({transfer_info_to_dictionary, Host, Link_list, Word_list}, State) ->
    NewState = store_data_EST(Host, Word_list,State),
    {TableId, SearchLevel, _} = NewState,
    dictionary_server:add_ETS_list_to_mnesia(ets:tab2list(TableId)),
      if
       SearchLevel < 5 ->
            get_website_source_code(Link_list);
        SearchLevel > 4 -> ok          

        end,

    
%    ets:delete(TableId),
%    TableId2 = ets:new(link_list, [ordered_set]),
%    NewState2= {TableId2, SearchLevel, Scanned_links},
    {noreply, NewState};



 handle_cast({get_website_source_code, Link_list}, State) ->
    {TableId, SearchLevel, Scanned_links} = State,
    Socket_Controller = spawn (socket_controller,start,[]),
%    Link_list2 = Link_list --Scanned_links,
    Link_list2 = Link_list,
    case length(Link_list2) of
                    0   ->   fetching_website([], Socket_Controller);
                    _ ->    lists:foreach(fun(Link) -> Socket_Controller ! {get_url, self(), Link} end , Link_list2)
             end,
     NewState = {TableId, SearchLevel+1, [Link_list2|Scanned_links]}, 
    {noreply, NewState};

handle_cast({dump_link_list}, State) ->
    {TableId, SearchLevel, _} = State,
    NewState = {TableId, SearchLevel, []},
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({example}, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ibrowse:stop(),
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

fetching_website (Link, Socket_Controller) ->
    Socket_Controller ! {get_url, self(), Link},
    ok.



store_data_EST(Host, Word_list,State) ->
    {TableId, SearchLevel, Scanned_links} = State,
    NewTableId = insert_to_EST(TableId, Word_list,Host, State),
%  lists:map(fun nice_print/1 ,ets:tab2list(NewTableId)),
%    {NewTableId, NewSearchLevel}.
    {NewTableId, SearchLevel, Scanned_links}.


insert_to_EST(TableId, [], _, _State) ->
        TableId;

insert_to_EST(TableId, Word_list, Host, State) ->
        [Word|Rest_of_words] = Word_list,
        List = ets:lookup(TableId, Word),       
        case  List of
             [] -> ets:insert(TableId, {Word, Host});
             [{_,List2}] -> case lists:member(Host, List2) of  
                 false  -> ets:insert(TableId, {Word, [Host|List2]});
                 true   -> ets:insert(TableId, {Word, List2});
                 _ ->ok
             end 
        end,
        insert_to_EST(TableId, Rest_of_words, Host, State).


%nice_print(Tuple) ->
%{Word, Host_list} = Tuple,
%io:format("   ~-15s => ~p~n",[Word,Host_list]),
%ok.
