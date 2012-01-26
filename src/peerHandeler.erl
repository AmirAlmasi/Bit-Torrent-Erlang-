%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @module peerHandeler.erl
%% @doc <html> This module is for connecting to peers </html>
-module(peerHandeler).
-export([peerHandeler/5,peerHandeler/2,peerSpawner/5]).


%%%By:Mikael Arvidsson

%%----------------------------------------------------------------------
%% Function: peerHandeler/5
%% Purpose: Starts a peer core process for each peer.
%% Args:   Peer IP,Peer port,Info_Hash of the torrent,Peer Id,OTP Siblings.
%% Returns: Nothing
%%----------------------------------------------------------------------
peerSpawner(Ip,Port,Info_hash,Peer_Id,Sibling) ->
spawn(fun() -> peerHandeler(Ip,Port,Info_hash,Peer_Id,Sibling) end).

peerHandeler(Ip,Port,Info_hash,Peer_Id,Sibling) ->
process_flag(trap_exit,true),
Bitfield = erbit_messageProvider:getBitfield(dict:fetch(erbit_messageProvider,Sibling)),
PidSend = spawn_link(sender,init,[Ip,Port,Info_hash,Peer_Id,Bitfield]),
Dpid = spawn_link(download,init,[PidSend,Sibling]),
Upid = spawn_link(upload,uploader,[PidSend, Sibling]),
coreLoop(Sibling, Ip,Dpid,Upid).

%%----------------------------------------------------------------------
%% Function: peerHandeler/2
%% Purpose: Starts a peer core process for listened peers.
%% Args:   Socket for the new peer, peerID.
%% Returns: Nothing
%%----------------------------------------------------------------------
peerHandeler(Socket,Peer_ID) ->
{ok,{Address,_}} = inet:peername(Socket),
PidSend = spawn_link(sender,init,[self(),Socket,Peer_ID]),
gen_tcp:controlling_process(Socket,PidSend),
receive
{checker,Sibling} -> 
Dpid = spawn_link(download,init,[PidSend,Sibling]),
Upid = spawn_link(upload,uploader,[PidSend,Sibling]),
coreLoop(Sibling,Address,Dpid,Upid)
end.


%%----------------------------------------------------------------------
%% Function: coreLoop/4
%% Purpose: Keep track of children.
%% Args:   Sibling,Ip,Dpid,Upid.
%% Returns: Nothing
%%----------------------------------------------------------------------
coreLoop(Sibling, Ip,Dpid,Upid) ->
receive
{'EXIT',_Pid,_Reason} -> 
erbit_socketHandler:deleteSocket(dict:fetch(erbit_socketHandler, Sibling), Ip),
exit(Dpid,"handeler"),
exit(Upid,"handeler")
end.
