%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_torrent_sup.erl
%% @doc <html> This module is a supervisor that it's responsibility is to 
%% spawn the torrent process per each torrent </html>
%% @version 1.0.1
%% @date Created: Nov 18, 2011

-module(erbit_torrent_sup).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/5]).

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
start_link(Reference,FileAddress, DirectoryPath, PeerId, Port) ->
	supervisor:start_link(?SERVER, [Reference, FileAddress, DirectoryPath, PeerId, Port]),
	ok.


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([Reference, FileAddress, DirectoryPath, PeerId, Port]) ->
%% 	io:format("erbit_torrent_sup:start_link/1 : ~p working Reference ~p ~n", [self(), Reference]),
     TorrentManager = {erbit_torrentManager,{erbit_torrentManager, start_link,[Reference,FileAddress, DirectoryPath, PeerId, Port]},
	      permanent, brutal_kill,worker,[gen_server]},
    {ok,{{one_for_all,5,1}, [TorrentManager]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

