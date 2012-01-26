%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_sup.erl
%% @doc <html> This module is the main supervisor that it's responsibility is to 
%% spawn the main modules of the system </html>
%% @version 1.0.1
%% @date Created: Nov 18, 2011
-module(erbit_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]).

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
start_link(StartArgs) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [StartArgs]).



%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([_StartArgs]) ->
	PeerId = erbit_makeId:makePeerId(),
	erbit_guiConnector:start(),
%% 	io:format("erBit_sup:start_link : ~p working ~n", [self()]),
	Core = {erbit_core,{erbit_core,start_link,[PeerId]},
			permanent, brutal_kill,worker,[erbit_core]},
	%% 	PidHandler = {pidHandler,{pidHandler,start_link,[]},
	%% 	      permanent, brutal_kill,worker,[pidHandler]},
	
	PortListener = {erbit_portListener,{erbit_portListener,start_link,[PeerId, 6087]},
					permanent, brutal_kill,worker,[erbit_portListener]},
	HandChecker = {erbit_handChecker,{erbit_handChecker,start_link,[]},
				   permanent, brutal_kill,worker,[erbit_handChecker]},
	%% 	GUI = {core,{core,start_link,[]},
	%% 	      permanent, brutal_kill,worker,[core]},
	{ok,{{one_for_all,2,1}, [Core, PortListener, HandChecker]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

