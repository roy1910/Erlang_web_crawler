-module(term_scanner).
-compile(export_all).

%% API
-export([extract_links_and_words/2]).

%% Supervisor callbacks

extract_links_and_words(Host, Binary_website) ->
	Words = extract_words(Binary_website),
	Links = extract_links(Host, Binary_website),
	{Host, Links, Words}.



extract_links(Host, Binary_website) ->
	List_of_raw_links = re:split(Binary_website, <<"<a\\s+href=\"([^\"]*)">>,[{return,list}]),
	case length(List_of_raw_links) of 
		0	-> List_of_raw_links;
		_	-> arrange_list(Host, List_of_raw_links,[])
	end.


arrange_list(Host, List_of_raw_links,List_of_links) ->
if
	length(List_of_raw_links) =:= 0 -> List_of_links;
	length(List_of_raw_links) > 0 ->
				[H|T] = List_of_raw_links,
case is_string(H) of
	true ->  case string:str(H,"http://") of
						1 -> arrange_list(Host, T,[H|List_of_links]);
						_ -> case string:str(H,"/") of
								1	-> H2 = string:concat(Host,H),
										 	arrange_list(Host,T,[H2|List_of_links]);
								_	-> 	arrange_list(Host,T,List_of_links)
								end
				end;
false ->arrange_list(Host, T,List_of_links)	
end
end.


isprint(X) when X >= 32, X < 127 -> true;
isprint(_) -> false.
is_string(List) when is_list(List) -> lists:all(fun isprint/1, List);
is_string(_) -> false.

extract_words(Binary_website) ->
	List_of_words = re:split(Binary_website, <<"\s|,|\\.">>,[{return,list}]),
	List_of_words2 = lists:filter (fun is_word/1, List_of_words),
	List_of_words3 = lists:filter (fun  is_empty_list/1, List_of_words2),
	List_of_words3.

isword(X) when X >= 65, X < 123, X/=91, X/=92,X/=93,X/=94,X/=95,X/=96-> true;
isword(X) when X == 45-> true;

isword(_) -> false.
is_word(List) when is_list(List) -> lists:all(fun isword/1, List);
is_word(_) -> false.

is_empty_list(X) when length(X) =/= 0 ->true;
is_empty_list(X) when length(X) =:= 0 ->false.

