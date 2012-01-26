%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @module upload.erl
%% @doc <html> This module is for uploading blocks </html>

-module(upload).
-export([uploader/2]).

%%----------------------------------------------------------------------
%% Function: uploader/2
%% Purpose: This was only put in a seperate module to match the design provided 
%% by our system designer. It only parse the information to different formats 
%% before sending it to filemanager/sender:send.
%% Args:  Pid of the sender, OTP sibling .
%% Returns: Nothing
%%----------------------------------------------------------------------

uploader(SendPid, Sibling) ->
SendPid ! {uppid,self()},
loop(SendPid , Sibling).

loop(SendPid , Sibling) ->
receive
{request,<<Piece:4/binary,Offset:4/binary,Size:4/binary>>} ->  
<<TotalSize:32>> = Size,  <<OffsetNr:32>> = Offset, <<PieceNr:32>> = Piece,
{send,Block,OffsetRep,{ok,DataRep}} = erbit_fileManager:readPart(dict:fetch(erbit_fileManager, Sibling), PieceNr, OffsetNr, TotalSize),
SendPid ! {upload,<<7:8,Block:32,OffsetRep:32,DataRep/binary>>},
loop(SendPid , Sibling)

end.
