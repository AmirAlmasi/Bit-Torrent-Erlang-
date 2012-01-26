%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_peerConnector.erl
%% @doc <html> A module to keep track of the information related to peers and their 
%% connection </html>
%% @version 0.1.0.3
-module(erbit_peerConnector).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, keepInfo/7, subscribe/2, connectSocket/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {reference = undefined,  torrentPid = undefined, sibling = dict:new(),
				peersList = false, fileSize = undefined, pieceSize = undefined,
				peerId = undefined , hash = undefined, peers = dict:new()}).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> A function to get the torrent information </html>
%% @spec keepInfo(pid(), reference(), integer(), integer(), string(), binary(), list()) -> ok
keepInfo(Pid, Reference, FileSize, PieceSize, PeerId, HashBinary, Sibling) ->
	gen_server:cast(Pid, {keep_info, Reference, FileSize, PieceSize, PeerId, HashBinary, Sibling}),
	ok.

%% @doc <html> a function for other processes to subscribe </html>
%% @spec subscribe(pid(), pid()) -> ok
subscribe(Pid, DownloaderPid)->
	gen_server:cast(Pid, {downloader_pid, DownloaderPid}),
	ok.

	
%% @doc <html> a function to start downloading as soon as we get peer list </html>
%% @spec connectSocket(pid(), reference(), tuple()) -> ok
connectSocket(Pid, Reference, {Ip, Port})->
	gen_server:cast(Pid , {connect_sockets, Reference, {Ip, Port}}),
	ok.


%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> a function to start gen_server </html>
%% @spec start_link(reference(), pid()) -> {ok, pid}
start_link(Reference,Pid) ->
	gen_server:start_link(?MODULE, [Reference,Pid], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Reference,Pid]) ->
%% 	io:format("erbit_peerConnector:init/1 -> ~p working Reference ~p~n", [self(), Reference]),
	erbit_torrentManager:subscribe(Pid, ?MODULE, Reference, self()),
	{ok, #state{reference = Reference, torrentPid = Pid}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% to get the fileSize and piceSize and ... from infoTrackerManager
handle_cast({keep_info, Reference, FileSize, PieceSize, PeerId, HashBinary, Sibling}, State) when Reference =:= State#state.reference->
			NewState=State#state{fileSize = FileSize, pieceSize = PieceSize, peerId = PeerId , hash = HashBinary, sibling= Sibling},
%% 			io:format("erbit_peerConnector:handle_cast, keepInfo -> FileSize ~p and PieceSize ~p are set ~p~n",[FileSize, PieceSize, HashBinary]),
	{noreply, NewState};

%% a function to start downloading
handle_cast({connect_sockets, Reference, {Ip, Port}}, State) when Reference =:= State#state.reference ->
%% 			io:format("erbit_peerConnector:handle_cast, connectSocket-> working ~p~n", [State#state.peersList]),
%% 			ReadyPeers = erbit_socketHandler:makeQuery(dict:fetch(erbit_socketHandler, State#state.sibling), Reference),
			spawnPeers({Ip, Port}, State),
			io:format("erbit_peerConnector:handle_cast, connectSocket -> spawn all the peers ~n"),
	{noreply, State};



%% a functiont to start downloading by getting a number based on bitfield
handle_cast({downloader_pid, DownloaderPid}, State) ->
	%% I should fix the refrence
	case erbit_messageProvider:getAblock(dict:fetch(erbit_messageProvider, State#state.sibling)) of
		{ok, Number} ->
			DownloaderPid ! {download, Number, State#state.fileSize, State#state.pieceSize};
		{done, downloaded} ->
			%% send the done message to the gui
			updateGUI(State#state.reference),
			erbit_torrentManager:doneTorrent(State#state.torrentPid, State#state.reference),
			erbit_socketHandler:changeStatus(dict:fetch(erbit_socketHandler, State#state.sibling),
											 State#state.reference, false),
			ok
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(_Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	io:format("erbit_peerConnector:terminate/2 -> working ~p~n", [Reason]),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra)->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% spawning the peers 
spawnPeers({Ip, Port}, State)->
	Hash = State#state.hash,
	PeerId = State#state.peerId,
%% 	io:format("erbit_peerConnector:spawnPeers -> working spawning process ip ~p port ~p~n", [Ip, Port]),
	Pid = peerHandeler:peerSpawner(Ip, Port, Hash, PeerId, State#state.sibling),
	erbit_socketHandler:updatePid(dict:fetch(erbit_socketHandler, State#state.sibling), State#state.reference, Ip, Pid),
%% 	peer_sup:start_link({State#state.reference, State#state.sibling}, {FileSize, PiceSize, Hash, PeerId}, {Ip, Port}),
	ok.

updateGUI(Reference) ->
	erbit_core:doneTorrent(Reference).
