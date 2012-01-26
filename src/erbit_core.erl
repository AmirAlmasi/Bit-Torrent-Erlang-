%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi   <amir.fireflame@gmail.com>
%% @module erbit_core.erl
%% @doc <html>The purpose of the module is to spawn new processes per each torrent
%% this module is responsible for updating the gui as well
%% <br/> module has gen_server behavior </html>
%% @version 1.0.2
%% @date Created: Nov 04, 2011
%% @date Edited: Nov 04, 2011 - implement start/0, startApp/0, readTorrent/0,showDataManager/0
%% @date Edited: Nov 06, 2011 - implement showElements/2, updateState/3, parseTrackerResponse/1
%% @date Edited: Nov 06, 2011 - implement seperateIpsPorts/2, editAddress/2,writeToFile/2
%% @date Edited: Nov 19, 2011 - change in some function and basically make new version of module

-module(erbit_core).
-behaviour(gen_server).
-define(PORT , 6087).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, updateName/2, readFile/2,
		 subscribe/2, updateSize/2, updateTracker/2, updatePeerList/2,
		 updateDownload/2, updateSeeder/3,
		 exit/1, doneTorrent/1, updateUpload/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {torrentManager = dict:new(), identifier = dict:new(), refIdentifier = dict:new(),
		peerId = undefined , port = ?PORT, fileIdentifier = 0}).

%% ====================================================================
%% External functions
%% ====================================================================


%% @doc <html> A function to be called at first when a torrent file is chosen on GUI </html>
%% @spec readFile(string(), {string(), integer(), integer(), integer()}) -> ok
readFile(TorrentfilePath, {DirectoryPath, NumWant, GlobalNumWant, Port})->
	gen_server:cast(?MODULE, {file, TorrentfilePath, {DirectoryPath, NumWant, GlobalNumWant, Port}}),
	ok.

%% @doc <html> A function for each torrentManager to subscribe to the erbit_core to get
%% identifier and refrence </html>
%% @spec subscribe(reference(), pid()) -> ok
subscribe(Reference, Pid)->
	gen_server:cast(?MODULE, {torrentManager, Reference, Pid}),
	ok.

%% @doc <html> A function to update the torrent name on GUI base on each
%% erbit_torrentInfoManager </html>
%% @spec updateName(reference(), string()) -> ok
updateName(Reference, Name)->
	gen_server:cast(?MODULE, {update_name, Reference, Name}),
	ok.

%% @doc <html> A function to update the torrent file size 
%% on GUI based on each erbit_torrentInfoManager </html>
%% @spec updateSize(reference(), pid()) -> ok
updateSize(Reference, Size)->
	gen_server:cast(?MODULE, {update_size, Reference, Size}),
	ok.

%% @doc <html> A function to update the trackers name on GUI based on each
%% erbit_torrentInfoManager </html>
%% @spec updateTracker(reference(), list()) -> ok
updateTracker(Reference, TrackerName)->
	gen_server:cast(?MODULE, {update_tracker, Reference, TrackerName}),
	ok.


%% @doc <html>  A function to update the peers ip and port
%% on GUI based on each erbit_torrentInfoManager </html>
%% @spec updatePeerList(reference(), list()) -> ok
updatePeerList(Reference, PeerList)->
	gen_server:cast(?MODULE, {update_peer_list, Reference, PeerList}),
	ok.

%% @doc <html>  A function to update the downloaded rate on GUI based on 
%% each erbit_torrentInfoManager which is called in piece checker component,
%% Downloaded should be in byte </html>
%% @spec updateDownload(reference(), integer()) -> ok
updateDownload(Reference, Downloaded) ->
	gen_server:cast(?MODULE, {downloaded, Reference, Downloaded}),
	ok.

%% @doc <html> A function to update the seeders and leechers on GUI based on 
%% each erbit_torrentInfoManager </html>
%% @spec updateSeeder(reference(), integer(), integer()) -> ok
updateSeeder(Reference, Seeders, Leechers) ->
	gen_server:cast(?MODULE, {seeders_lechers, Reference, Seeders, Leechers}),
	ok.

%% @doc <html> A function to exit all the processes when gui is closed </html> 
%% @spec exit(any()) -> ok
exit(Reason) ->
	gen_server:cast(?MODULE, {exit, Reason}),
	ok.

%% @doc <html> A function to send done message to gui per each torrent file </html> 
%% @spec doneTorrent(reference()) -> ok
doneTorrent(Reference) ->
	gen_server:cast(?MODULE, {done, Reference}),
	ok.

%% @doc <html> A function to send done message to gui per each torrent file </html> 
%% @spec updateUpload(reference(), byte()) -> ok
updateUpload(Reference, Byte) ->
	gen_server:cast(?MODULE, {upload, Reference, Byte}),
	ok.


%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html>An external function to start the erbit_core module
%% it gets peer Id as an argument </html>
%% @spec start_link(string())-> {ok, pid()}
start_link(PeerId) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [PeerId], []).


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([PeerId])->
%% 	io:format("erbit_core:init/0 -> ~p working ~n", [self()]),
%% 	io:format("erbit_core:init/1 -> PeerId ~p ~n", [PeerId]),
	case erlang:length(PeerId) of 
		20 ->
			ok;
		Unecpected ->
			io:format("erbit_core:init-> Wrong! peerId is ~p~n", [Unecpected])
	end,
	%% this one should be removed later on
%% 	tracker:start(),
    {ok, #state{peerId = PeerId}}.


handle_call(_Request, _From, State) ->
	io:format("erbit_core:handle_call was called An unecpected call has been occured~n"),
    Reply = ok,
    {reply, Reply, State}.



%% a function to start torrent manager per each torrent, and make ref
handle_cast({file, TorrentAddress, {DirectoryPath, _NumWant, _GlobalNumWant, Port}}, State)->
	%%  So far NumWant, GlobalNumWant, Port are unused
	Reference = make_ref(),
	erbit_torrent_sup:start_link(Reference, TorrentAddress, DirectoryPath, State#state.peerId, Port),
 {noreply, State#state{refIdentifier = dict:store(Reference, State#state.fileIdentifier, State#state.refIdentifier),
					identifier = dict:store(State#state.fileIdentifier, Reference, State#state.identifier),
					fileIdentifier = State#state.fileIdentifier + 1, port = Port}};

%% keep each torrent with a refrence
handle_cast({torrentManager, Reference, Pid}, State)->
%% 	io:format("torrentmanager is subscribing ref= ~p , pid= ~p ~n", [Reference,Pid]),
	{noreply, State#state{torrentManager = dict:store(Reference, Pid, State#state.torrentManager)}};


%% to update torrent name on gui
handle_cast({update_name, Reference, Name}, State)->
%% 	io:format("erbit_core:handle_cast, updateName -> working name ~p ~n", [Name]),
	erbit_guiConnector:showname(dict:fetch(Reference, State#state.refIdentifier), binary_to_list(Name)),
 {noreply, State};


%% to update file size on gui
handle_cast({update_size, Reference, Size}, State)->
%% 	io:format("erbit_core:handle_cast, updateSize -> working name ~p ~n", [Size]),
	erbit_guiConnector:showsize(dict:fetch(Reference, State#state.refIdentifier), Size),
 {noreply, State};


%% to update trackers which are in used on gui
handle_cast({update_tracker, Reference, TrackerName}, State)->
%% 	io:format("erbit_core:handle_cast, update tracker -> working ~p ~n", [TrackerName]),
	erbit_guiConnector:showtrackers(dict:fetch(Reference, State#state.refIdentifier), TrackerName),
 {noreply, State};


%% to update peer lists from which we are downloading
handle_cast({update_peer_list, Reference, PeerList}, State)->
%% 	io:format("erbit_core:handle_cast, update peer list -> working ~p ~n", [PeerList]),
	erbit_guiConnector:showip(dict:fetch(Reference, State#state.refIdentifier), PeerList),
 {noreply, State};

%% to update downloaded amount
handle_cast({downloaded, Reference, Downloaded}, State)->
%% 	io:format("erbit_core:handle_cast, update Download -> working downloaded ~p ~n", [Downloaded]),
	erbit_guiConnector:showdownloaded(dict:fetch(Reference, State#state.refIdentifier), Downloaded),
 {noreply, State};


%% to update seeders and leechers on gui
handle_cast({seeders_lechers, Reference, Seeders, Leechers}, State)->
	erbit_guiConnector:showseeders(dict:fetch(Reference, State#state.refIdentifier), Seeders),
	erbit_guiConnector:showleechers(dict:fetch(Reference, State#state.refIdentifier), Leechers),
    {noreply, State};

%% when gui is closed and we should kill the processes
handle_cast({exit, _Reason}, State)->
	erlang:exit(kill),
	erlang:halt(),
    {noreply, State};

%% when a torrent is done to send a message to gui
handle_cast({done, Reference}, State)->
	erbit_guiConnector:doneTorrent(dict:fetch(Reference, State#state.refIdentifier)),
    {noreply, State};

%% to update uploaded amount 
handle_cast({upload, Reference, Byte}, State)->
	erbit_guiConnector:showuploaded(dict:fetch(Reference, State#state.refIdentifier), Byte),
    {noreply, State};

handle_cast(_Msg, State)->
	io:format("erbit_core:handle_cast(Msg, State) -> working ~n"),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
	io:format("erbit_core:handle_info -> got a unecpected message ~p~n",[_Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	io:format("erbit_core:terminate/2 -> working ~p ~n",[Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% A test function to show all current torrents that are running

%% currentTorrents([], _)->
%% 	ok;
%% currentTorrents([Key|T], DictList) ->
%% 	io:format("~p ~p~n" ,[Key , dict:fetch(Key, DictList)]),
%% 	currentTorrents(T, DictList).