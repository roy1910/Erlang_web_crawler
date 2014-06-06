%%%-------------------------------------------------------------------
%%% File    : dictionary_server
%%% Author  : Roy Goldstein Almaliach <roy.almaliach@gmail.com>
%%% Description : 
%%%
%%% Created :  16.04.14 by Roy
%%%-------------------------------------------------------------------
-module(dictionary_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).


-export ([add_ETS_list_to_mnesia/1, stop/0, 
		delete_DB/0,show_DB/0,select_word/1,
		remove_word/1,word_count/0,links_scanned_count/0]).

-record(state, {}).
-define (SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%%% supplement functions
%%--------------------------------------------------------------------
stop()      ->  gen_server:call(?MODULE, stop).

add_ETS_list_to_mnesia(ETS_list)    ->  gen_server:call(?MODULE, {add_items, ETS_list}).



show_DB()							->	gen_server:call(?MODULE, {show_DB}).
select_word(Word)					->	gen_server:call(?MODULE, {select_word, Word}).
remove_word(Word)					->	gen_server:call(?MODULE, {remove_word, Word}).
delete_DB()							->	gen_server:call(?MODULE, {delete_DB}).
word_count()						->	gen_server:call(?MODULE, {word_count}).
links_scanned_count()				->	gen_server:call(?MODULE, {links_scanned_count}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	application:start(mnesia),
	do_this_once(),
	start(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_items, ETS_list}, _From, State) ->
    lists:foreach(fun add_item/1, ETS_list),
    Reply = ok,
    {reply, Reply, State};

handle_call({show_DB}, _From, State) ->
    Reply = show_DB,
	List = select_all(word_dictionary),
    nice_print(List),	
	{reply, Reply, State};

handle_call({select_word, Word}, _From, State) ->
    Reply = word_selected,
	List = select_item(Word),
	nice_print(List),
	{reply, Reply, State};

handle_call({remove_word, Word}, _From, State) ->
    Reply = remove_item(Word),
	{reply, Reply, State};


handle_call({delete_DB}, _From, State) ->
    Reply = reset_tables(),
	{reply, Reply, State};

handle_call({word_count}, _From, State) ->
    Reply = count_unique_words(),
	{reply, Reply, State};

handle_call({links_scanned_count}, _From, State) ->
    Reply = count_unique_links(),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
    Reply = stoped,
    mnesia:stop(),
	{stop, normal, Reply, State}.

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
terminate(_Reason, _State) ->
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

%% IMPORTANT: The next line must be included
%%            if we want to call qlc:q(...)

-include_lib("stdlib/include/qlc.hrl").


-record(word_dictionary, {word, list_of_links}).

do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(word_dictionary,   [{attributes, record_info(fields, word_dictionary)}]),
    mnesia:stop().


start() ->
    mnesia:start(),
    mnesia:wait_for_tables([word_dictionary], 2000).



%% SQL equivalent
%%  SELECT * FROM shop;

select_all(Table) ->
    do(qlc:q([X || X <- mnesia:table(Table)])).


nice_print([]) ->
io:format("not found~n");
nice_print(List) ->
TableId = ets:new(test, [ordered_set]),
    lists:foreach(fun ({_,Word,Links}) -> ets:insert(TableId, {Word, Links}) end, List),
		F = fun (Tuple) -> {Word, Host_list} = Tuple,
				io:format("   ~-15s => ~p~n",[Word,Host_list]) end,
    lists:foreach(F, ets:tab2list(TableId)),
   	ets:delete(TableId).


%% SQL equivalent
%%  SELECT item, quantity FROM shop;

select_item(Word) ->
    do(qlc:q([X || X <- mnesia:table(word_dictionary), X#word_dictionary.word == Word])).

 count_unique_words() ->
 TableId = ets:new(words, [set]),
 List = select_all(word_dictionary),
    lists:foreach(fun ({_,Word,_}) -> ets:insert(TableId, {Word, ""}) end, List),
   	List_of_words = ets:tab2list(TableId),
   	Word_count = length(List_of_words),
   	ets:delete(TableId),
   	Word_count.

count_unique_links() ->
TableId = ets:new(links, [set]),
 List = select_all(word_dictionary),
	
	F1 =  fun (Link)		->	ets:insert(TableId, {Link, ""}) end,    
	F2 =  fun ({_,_,Links})	->	lists:foreach(F1, Links) end,

    lists:foreach(F2,List),
   	Link_counts = length(ets:tab2list(TableId)),
   	ets:delete(TableId),
   	Link_counts.

%% SQL equivalent
%%   SELECT shop.item FROM shop
%%   WHERE  shop.quantity < 250;

%select_itmes_with_conditions(Table,Conditions) ->
%    do(qlc:q([X#shop.item || X <- mnesia:table(shop),X#shop.quantity < 250	]));
%    do(qlc:q([X#shop.item || X <- mnesia:table(Table), Conditions])).

%% SQL equivalent
%%   SELECT shop.item
%%   FROM shop, cost 
%%   WHERE shop.item = cost.name 
%%     AND cost.price < 2
%%     AND shop.quantity < 250

%demo(join) ->
%    do(qlc:q([X#word_dictionary.item || X <- mnesia:table(word_dictionary),
%			     X#shop.quantity < 250,
%			     Y <- mnesia:table(cost),
%			     X#shop.item =:= Y#cost.name,
%			     Y#cost.price < 2
%				])).



do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.


add_item({Word, List_of_links}) ->
    Row = #word_dictionary{word = Word, list_of_links = List_of_links},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).



remove_item(Word) ->
    Oid = {word_dictionary, Word},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F),
    word_deleted.


reset_tables() ->
    mnesia:clear_table(word_dictionary),
    mnesia_DB_reset.