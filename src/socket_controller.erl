%%%-------------------------------------------------------------------
%%% File    : socket_controller
%%% Author  : Roy Goldstein Almaliach <roy.almaliach@sumup.com>
%%% Description : creating a tcp socket to recive a web site.
%%%
%%% Created :  8.4.2014 by Roy Goldstein Almaliach
%%%-------------------------------------------------------------------

-module (socket_controller).
-export ([start/0]).



start() ->
	receive
	{get_url, From, Link} -> get_url(From,Link), start();
	stop -> terminate_ok
	end.



get_url(From,Link)->
	application:ensure_started(ibrowse),
	{Host, Bin_website} = case Link of 
				[] -> fetch_binary("http://www.songlyrics.com");
				_ ->fetch_binary(Link)
				end,
	send_binary(From, Host, Bin_website).



fetch_binary(Host)->
				case ibrowse:send_req(Host, [], get) of
					{ok,_,_,Bin_website} -> {Host, list_to_binary(Bin_website)};
										_-> exit(this_link_cannot_be_scanned)
				end.

send_binary(From, Host, Bin_website) ->
	{Host2, Link_list, Word_list} = term_scanner:extract_links_and_words(Host, Bin_website),
	gen_server:cast(From, {transfer_info_to_dictionary, Host2, Link_list, Word_list}).
