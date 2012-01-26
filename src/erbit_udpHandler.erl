%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @module erbit_udpHandler.erl
%% @doc <html> This module is for connecting to udp trackers </html>

-module(erbit_udpHandler).
-export([start/6]).
 
start(Pid,UrlUn, Reply,Trans_ID,Info_Hash, Ip)->
	spawn(fun() -> udptrack(Pid, UrlUn, Reply,Trans_ID,Info_Hash, Ip) end).

udptrack(Pid, UrlUn, Reply,Trans_ID,Info_Hash, Ip) -> % Amir should give info_hash as binary
 {ok,Url,TrackerPort} = split(UrlUn,[],[],false),
	{ok,Socket} = gen_udp:open(5555,[binary]),%%io:format("Sending.~n"),
 
%%=================================================================================
%% Fix this  
%%  {9, Value}
%% ================================================================================
 
	{PeerId, Port, Uploaded, Downloaded, Left, _Event, Numwant, _No_peer_id, _Compact} = Reply,
 gen_udp:send(Socket,Url,TrackerPort,<<4497486125440:64,0,0,0,0,Trans_ID/binary>>),
io:format("Sent~n"),
receive
{udp, Socket, _, _,<<0,0,0,0,Trans_ID:4/binary,Connection_ID:8/binary>>} -> 
%% {ok,{Ip,_}} = inet:sockname(Socket),
%% <<ConnectionID/binary, 1:32, TransactionId/binary, Hash/binary, BinaryPeerId/binary, Downloaded:64, Left:64, Uploaded:64, 2:32, IpAddress:32, Key:32, Numwant:32, Port:16>>
BinaryPeerId = list_to_binary(PeerId),

%% 	io:format("Info_Hash ~p ~n",[Info_Hash]),
%% 	
%% 	io:format("Ip ~p~n", [Ip]),
%% 	io:format("Connection_ID ~p~n", [Connection_ID]),
%% 	io:format("Trans_ID ~p~n", [Trans_ID]),
%% 	io:format("BinaryPeerId ~p~n", [BinaryPeerId]),
%% 	io:format("PeerId ~p~n", [PeerId]),
%% 	io:format("Downloaded ~p~n", [Downloaded]),
%% 	io:format("Left ~p~n", [Left]),
%% 	io:format("Uploaded ~p~n", [Uploaded]),
%% 	io:format("Event ~p~n", [Event]),
%% 	io:format("Numwant ~p~n", [Numwant]),
%% 	io:format("Port ~p~n", [Port]),
%% 	io:format("____~p~n",[<<Connection_ID/binary,1:32,Trans_ID/binary,Info_Hash/binary,BinaryPeerId/binary,Downloaded:64,Left:64,Uploaded:64, 2:32,0:32,0:32,Numwant:32,Port:16>>]),



Announce = <<Connection_ID/binary,1:32,Trans_ID/binary,Info_Hash/binary,BinaryPeerId/binary,Downloaded:64,Left:64,Uploaded:64, 2:32, Ip/binary,0:32,Numwant:32,Port:16>>,  %% AMIR PUT ACTION WHERE IT IS 1:32
gen_udp:send(Socket, Url, TrackerPort, Announce),
receive
{udp,Socket,_,_,<<Action:4/binary,TransID:4/binary,Intervall:4/binary,Leechers:4/binary,Seeders:4/binary,Daata/binary>>} ->
<<NewAction:32>> = Action,<<NewTransID:32>> = TransID,<<NewIntervall:32>> = Intervall,<<NewLeechers:32>> = Leechers,<<NewSeeders:32>> = Seeders,
 io:format("Got:~n Action:~w~n TransID:~w~n Intervall:~w~n Leechers:~w~n Seeders:~w~n Rest:~w~n",[NewAction,NewTransID,NewIntervall,NewLeechers,NewSeeders,Daata]),
Pid ! {upd_answer, Daata}
after 3000 -> no_response_udp_announce
end;
{udp,Socket,_,_,Bin} -> io:format("Got unhandeled UDP packet:~w~n",[Bin])
after 2000 -> no_response
end,gen_udp:close(Socket).




split([],URL,PORT,_) -> {ok ,lists:reverse(URL),list_to_integer(lists:reverse(PORT))};
split([H|T],URL,PORT,BOOL) ->
case H of 
$: -> split(T,URL,PORT,true);
_ when BOOL =:= false -> split(T,[H|URL],PORT,BOOL);
_ -> split(T,URL,[H|PORT],BOOL)
end.