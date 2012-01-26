%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @module sender.erl
%% @doc <html> This module is for send the binary messages</html>
-module(sender).
-export([init/3,init/5]).


%%----------------------------------------------------------------------
%% Function: init/3
%% Purpose: This init will initialize when peer connect to client. Only sequental stuff.
%% Args:  Pid of the peerHandeler, Socket of the peer, Client peer ID .
%% Returns: Nothing
%%----------------------------------------------------------------------

init(Pid,Socket,Peer_ID) ->
timer:sleep(500),
{ok, <<Var:8>>} = gen_tcp:recv(Socket,1),
{ok, <<_Pstr:19/binary, _Reserved:8/binary, InfoHash:20/binary, _PeerID:20/binary>>} = gen_tcp:recv(Socket,(Var + 48)),
{ok,{Address,Port}} = inet:peername(Socket),
erbit_handChecker ! {hand, self(), Address, Port, InfoHash},
receive 
{ok,Sibling} -> Pid ! {checker,Sibling},BinaryPeerId = list_to_binary(Peer_ID),
Response =  <<19,
"BitTorrent protocol",
0,0,0,0,0,0,0,0,
InfoHash/binary,
BinaryPeerId/binary>>,
case gen_tcp:send(Socket,Response) of
ok -> sent;
{error,_} ->  exit({crash,send_fail,Address})
end,
inet:setopts(Socket,[binary,{packet, 4},{active,true}]),
case erbit_messageProvider:getBitfield(dict:fetch(erbit_messageProvider,Sibling)) of
{ok,Bit} ->  
	case gen_tcp:send(Socket,<<5,Bit/binary>>) of
        ok-> sent;
        {error,_} -> io:format("Send failed"), exit({crash,send_fail,Address})
         end;
{error,_} -> no_bitfield
end,
case gen_tcp:send(Socket,<<1>>) of
ok -> sent;
{error,_} -> exit({crash,send_fail,Address})
end,
receive 
{dpid,DownPid} ->
receive 
{uppid,UpPid} ->
send(Socket,DownPid,UpPid,Address)
end
end;
{error,Reason} -> exit(self(),Reason)
end.
 
%%----------------------------------------------------------------------
%% Function: init/5
%% Purpose: This init will initialize when client connect to the peer. Only sequental stuff.
%% Args:  IP number to peer, Peer listening port, Client peer ID, Our bitfield .
%% Returns: Nothing
%%----------------------------------------------------------------------
init(Host,Port,Info_hash,Peer_id,Bitfield) ->
PeerId_binary = list_to_binary(Peer_id),
 Data = <<19,
"BitTorrent protocol",
0,0,0,0,0,0,0,0,
Info_hash/binary,PeerId_binary/binary>>,
case gen_tcp:connect(Host,Port,[binary,{packet,raw},{active,false}]) of 
{ok,Socket} ->
case gen_tcp:send(Socket,Data) of
ok -> sent;
{error,_} -> io:format("Send failed"), exit({crash,send_fail,Host})
end,
timer:sleep(500),
case  gen_tcp:recv(Socket,1) of
{ok, <<Var:8>>} ->
{ok, <<_Pstr:19/binary, _Reserved:8/binary, _InfoHash:20/binary, _PeerID:20/binary>>} = gen_tcp:recv(Socket,(Var + 48)),
case inet:setopts(Socket,[binary,{packet, 4},{active,true}]) of
ok -> opt_set;
{error,Res} -> exit({crash,Res,Host})
end,
case Bitfield of 
{ok,Bit} -> case gen_tcp:send(Socket,<<5,Bit/binary>>) of
            ok -> sent;
            {error,_} -> exit({crash,send_fail,Host})
            end;
{error, _} -> no_bitfield
end,

case gen_tcp:send(Socket,<<1>>) of
ok -> ok;
{error,_} -> exit({crash,send_fail,Host})
end,
receive 
{dpid,DownPid} ->
receive 
{uppid,UpPid} ->
send(Socket,DownPid,UpPid,Host)
end
end;
{error,_} -> exit({crash,handshake_response,Host})
end;

{error,Reason} -> exit({crash,Reason,Host})
end.


%%----------------------------------------------------------------------
%% Function: send/4
%% Purpose: This function is only to work with the socket and the only way to communicate 
%% with the peer will just loop and patter match its inbox messages. Commented messages was not
%% implemented by Amir.
%% Args:  Peer socket, Pid of download process, Pid of upload process, IP of peer .
%% Returns: Nothing
%%----------------------------------------------------------------------

send(Socket,DownPid,UpPid,Host) -> 
receive 

{tcp,Socket,<<>>} -> gen_tcp:send(Socket,<<>>),%%io:format("Was pinged~n"),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<0>>} -> %% io:format("Was choked~n"),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<1>>} -> %%io:format("Was unchoked~n"),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<2>>} -> %% io:format("Peer is interested~n"),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<3>>} -> %% io:format("Peer is not interested~n"),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<4,_Have:4/binary>>} -> %% io:format("Got a have message: ~w~n",[_Have]),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<5,_Bitfield/binary>>} -> %% io:format("Got a bitfield message: ~w~n",[Bitfield]),
send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<6,Request/binary>>} -> %% io:format("Got a request message: ~w~n",[Request]),
UpPid ! {request,Request},send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<7,_Index:4/binary,_Offset:4/binary,Piece/binary>>} -> DownPid ! {subblock,Piece} ,send(Socket,DownPid,UpPid,Host);
{tcp,Socket,<<8,_Cancel/binary>>} -> %% io:format("Got a cancel message: ~w~n",[Cancel]),
gen_tcp:close(Socket), exit({crash,was_canceled,Host});
{tcp,Socket,<<9,_Port/binary>>} -> %% io:format("Got a port message: ~w~n",[Port]),
send(Socket,DownPid,UpPid,Host);
{tcp_closed, Socket} -> %% io:format("Socket was closed by peer~n"),
exit({crash,socket_closed,Host});
{tcp_error, Socket, Reason} -> %% io:format("There was an error on the socket:~w~n",[Reason]),
gen_tcp:close(Socket),exit({crash,Reason,Host});
{tcp,Socket,_ERROR} ->  %% io:format("ERROR: got unwanted message: ~w~n",[ERROR]),
send(Socket,DownPid,UpPid,Host);
{upload,Message} ->  gen_tcp:send(Socket,Message),send(Socket,DownPid,UpPid,Host);
Error -> io:format("GOT NOT HANDELED DATA:~w~n",[Error])

after 120000 -> 
 case gen_tcp:send(Socket,<<>>) of 
ok -> send;
{error,_} ->gen_tcp:close(Socket),exit({crash,send_fail,Host})
end,
receive
{tcp,Socket,<<>>} -> send(Socket,DownPid,UpPid,Host)
after 1000 -> gen_tcp:close(Socket), exit(no_response)
end
end. 
