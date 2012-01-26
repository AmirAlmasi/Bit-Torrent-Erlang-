%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @doc <html> A module to handle the downloading the blocks and subblocks </html>

-module(download).
-export([init/2]).


%%----------------------------------------------------------------------
%% Function: init/2
%% Purpose: Sends pids and starts loop.
%% Args:   Sender pid, OTP sibling.
%% Returns: Nothing
%%----------------------------------------------------------------------

init(Pid,Sibling) ->
Pid ! {dpid,self()},
erbit_peerConnector:subscribe(dict:fetch(erbit_peerConnector, Sibling), self()),
downloadloop(Pid,Sibling).


%%----------------------------------------------------------------------
%% Function: downloadloop/2
%% Purpose: Waits for new block to download.
%% Args:   Sender pid, OTP sibling.
%% Returns: Nothing
%%----------------------------------------------------------------------

downloadloop(Pid,Sibling) ->
receive
{download,PieceNr,TorrentSize,PieceSize} -> %% io:format("Download started ~p~n",[PieceNr]),
 collectData(PieceNr,TorrentSize,PieceSize,Pid,Sibling)
end.

%%----------------------------------------------------------------------
%% Function: collectsData/5
%% Purpose: Inits the download request.
%% Args:   Block number,Torrent size,Block size,Sender pid,OTP Sibling.
%% Returns: Nothing
%%----------------------------------------------------------------------

collectData(Nr,TorrentSize,PieceSize,Pid,Sibling) ->
IndexValue = TorrentSize - (Nr * PieceSize),

%% io:format("Sending interested and request~n"),
Pid ! {upload,<<2:8>>},
Pid ! {upload,<<6:8,Nr:32,0:32,16384:32>>},
case IndexValue < PieceSize of
true -> SizeL = roundUp(IndexValue),download(Nr,<<>>,SizeL,0,Pid,0,Sibling);
false -> download(Nr,<<>>,PieceSize,0,Pid,0,Sibling)
end.
 

%%----------------------------------------------------------------------
%% Function: download/7
%% Purpose: Download subblocks until the correct size is achived .
%% Args:   Block number,Total amount of data downloaded,
%% Total size of the torrent,Offset,Sender pid,Counter, OTP sibling.
%% Returns: Sends data to file writer.
%%----------------------------------------------------------------------

download(Nr,Total,TotalSize,TotalSize,Pid,_Counter,Sibling) -> 
%% io:format("Got the block nr ~w ! ~nSize:~w~n",[Nr,size(Total)]),
%% nfile:locateAndwrite(Total,Nr,0),

erbit_pieceChecker:checkData(dict:fetch(erbit_pieceChecker, Sibling), Total, Nr),
erbit_peerConnector:subscribe(dict:fetch(erbit_peerConnector, Sibling), self()),
downloadloop(Pid,Sibling);
 
download(Nr,Total,TotalSize,Offset,Pid,Counter,Sibling) ->
receive 
{subblock,Data} -> %%io:format("Got subblock number:~w~n",[Counter]),
				   Off = Offset + size(Data),
case Off == TotalSize of
true -> download(Nr,<<Total/binary,Data/binary>>,TotalSize,Off,Pid,Counter+1,Sibling);
false -> Bin = <<6:8,Nr:32,Off:32,16384:32>>, Pid ! {upload,Bin},
download(Nr,<<Total/binary,Data/binary>>,TotalSize,Off,Pid,Counter+1,Sibling)
end
end.




%%----------------------------------------------------------------------
%% Function: round up/1
%% Purpose: Rounds up to the next integer.
%% Args:   Integer.
%% Returns: Rounded int
%%----------------------------------------------------------------------
roundUp(X) when X < 0 ->
trunc(X);
roundUp(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
