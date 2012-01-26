%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_torrentManager_sup.erl
%% @doc <html> This module is a supervisor that it's responsibility is to 
%% spawn the processes realted to each torrent for example parsing , connecting 
%% to tracker </html>
%% @version 1.0.1
%% @date Created: Nov 18, 2011
-module(erbit_torrentManager_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/3]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Reference,Pid, DirectoryPath) ->
	supervisor:start_link(?SERVER, [Reference,Pid, DirectoryPath]).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([Reference,Pid, DirectoryPath]) ->
%% 	io:format("erbit_torrentManager_sup:start_link/0 : ~p working Reference ~p~n", [self(), Reference]),
	MessageProviderChild = {erbit_messageProvider,{erbit_messageProvider, start_link,[Reference,Pid]},
	      permanent, brutal_kill,worker,[erbit_messageProvider]},
	PeerConnectorChild = {erbit_peerConnector,{erbit_peerConnector, start_link,[Reference,Pid]},
	      permanent, brutal_kill,worker,[erbit_peerConnector]},
	TrackerConnectorChild = {erbit_trackerConnector,{erbit_trackerConnector, start_link,[Reference,Pid]},
	      permanent, brutal_kill,worker,[erbit_trackerConnector]},
	PieceCheckerChild = {erbit_pieceChecker,{erbit_pieceChecker, start_link,[Reference,Pid]},
	      permanent, brutal_kill,worker,[erbit_pieceChecker]},
	FileManagerChild = {erbit_fileManager,{erbit_fileManager, start_link,[Reference,Pid]},
	      permanent, brutal_kill,worker,[erbit_fileManager]},
	TorrentInfoManagerChild = {erbit_torrentInfoManager,{erbit_torrentInfoManager, start_link,[Reference,Pid, DirectoryPath]},
	      permanent, brutal_kill,worker,[erbit_torrentInfoManager]},
	SocketHandler = {erbit_socketHandler,{erbit_socketHandler, start_link,[Reference,Pid]},
	      permanent, brutal_kill,worker,[erbit_socketHandler]},
    {ok,{{one_for_all,0,1}, [FileManagerChild, MessageProviderChild, PeerConnectorChild, 
							 TrackerConnectorChild,TorrentInfoManagerChild, PieceCheckerChild,
							 SocketHandler]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

