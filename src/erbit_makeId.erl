%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Farrokh Tavakoli Banizi  <farrokh-t-b@hotmail.com>
%% @module erbit_makeId.erl
%% @doc <html>The purpose of this module is to make application peer id and transaction
%% id for udp trackers</html>
%% @version 0.1

%% to make unique peer Id and unique transaction Id
-module(erbit_makeId).
-export([makePeerId/0,makeTransId/0]).

%% peer id

%% @doc <html> A function to add 0 at the end if one number is missing </html>
%% @spec zero(integer(), list()) -> ok
zero(0,Lista) -> 
	Lista;
zero(Num,Lista)-> 
	zero(Num-1,[48|Lista]).  

%% a function that reverses the list 
add([],Num) -> 
	Num;     
add([H|T],Num) ->
	add(T,[H|Num]).

%% a function to count the amount of numbers
count([]) -> 
	0;
count([_|T]) -> 
	1 + count(T).

%% a function that checks everything and put the word and integers together
makePeerId()->
	Num = integer_to_list(round(random:uniform()* 100000000000000)),
	Erbit = "Erbit-",
	ID = add(lists:reverse(Erbit),Num),
	case count(ID) < 20 of 
		false -> 
			ID;
		true -> 
			Antal = 20 - count(ID),
			_Final = lists:reverse(zero(Antal,lists:reverse(ID)))
%% 			io:format("~p~n", [Final])
	end.

%% transaction id
makeTransId() ->
	makeTransId(4,[]).
makeTransId(0,Lista) ->
	Lista;
makeTransId(Num,Lista)->
	makeTransId(Num-1,[random:uniform(9)|Lista]). 
