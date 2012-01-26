%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_torrentManager.erl
%% @doc <html> This module will be per each torrent and it's main responsibility is to 
%% monitoring the torrent </html>
-module(erbit_torrentManager).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/5,subscribe/4, gotBlock/3, doneTorrent/2, trackerValues/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {refernce = undefined, peerId=undefined, port=undefined,
				fileAddress = undefined , subscriber = dict:new(), downloaded = 0, uploaded=0,
				event = started}).

-define(LEFT, 727955456).
-define(NUMWANT, 50).
-define(NO_PEER_ID, 1).
-define(COMPACT, 1).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> A function for related processes to subscribe to the torrentManager
%% to keep track of thier pids </html>
%% @spec subscribe(pid(), atom(), reference(), pid()) -> ok
subscribe(TorManagerPid, Type, Reference, Pid) ->
	gen_server:cast(TorManagerPid, {torrentManager, Type, Reference, Pid}),
	ok.


%% @doc <html> A function for updating the current downloaded amount of trackers </html>
%% @spec gotBlock(pid(), reference(), byte()) -> ok
gotBlock(Pid, Reference, Byte) ->
	gen_server:cast(Pid, {got_block, Reference, Byte}),
	ok.

%% @doc <html> A function for updating the current status of event to send to tracker </html>
%% @spec doneTorrent(pid(), reference()) -> ok
doneTorrent(Pid, Reference) ->
	gen_server:cast(Pid, {done_torrent, Reference}),
	ok.


%% @doc <html> A function for updating the tracker </html>
%% @spec trackerValues(pid(), reference()) -> tuple()
trackerValues(Pid, Reference) ->
	gen_server:call(Pid, {tracker_value, Reference}).
	

%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> A function for starting the gen_server </html>
%% @spec start_link(reference(), string(), string(), string(), port()) -> ok
start_link(Reference, FileAddress, DirectoryPath,  PeerId, Port) ->
	gen_server:start_link(?MODULE, [Reference, FileAddress, DirectoryPath, PeerId, Port], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Reference, FileAddress, DirectoryPath,  PeerId, Port]) ->
%% 	io:format("erbit_torrentManager:init/1 -> ~p working Reference ~p~n", [self(), Reference]),
	ProcessedFileAddr = editAddress(FileAddress,[]),
	ProcessedDirePath = editAddress(DirectoryPath,[]) ++ "/",

	%% testing of information 
	%% 	io:format("erbit_torrentManager:init/1 -> working FileAddress ~p~n", [ProcessedFileAddr]),
	%% 	io:format("erbit_torrentManager:init/1 -> working PeerId ~p~n", [PeerId]),
	%% 	io:format("erbit_torrentManager:init/1 -> working Port ~p~n", [Port]),
 
	erbit_core:subscribe(Reference, self()),
	erbit_torrentManager_sup:start_link(Reference, self(), ProcessedDirePath),
	{ok, #state{refernce = Reference, fileAddress = ProcessedFileAddr, 
				peerId = PeerId , port = Port, downloaded = 0, uploaded = 0}}.

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

handle_call({tracker_value, Reference}, _From, State) when Reference =:= State#state.refernce ->
	case State#state.event of
		normal ->
			Value = {State#state.peerId, State#state.port, State#state.uploaded,
					 State#state.downloaded, ?LEFT, ?NUMWANT, ?NO_PEER_ID, ?COMPACT},
			Reply = {size(Value), Value};
		Event ->
			Value = {State#state.peerId, State#state.port, State#state.uploaded,
					 State#state.downloaded, ?LEFT, Event, ?NUMWANT, ?NO_PEER_ID, ?COMPACT},
			Reply = {size(Value), Value}
		
	end,
	{reply, Reply, State#state{event = normal}};

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

%% a function for the relevant processes to torrentManager to subscribe for keeping track of the pid 
handle_cast({torrentManager, Type, Reference, Pid}, State) ->
%% 	io:format("A subscriber ~p has been registered ~n", [Type]),
	case Reference =:= State#state.refernce of 
		true ->
			NewState = State#state{subscriber = dict:store(Type, Pid, State#state.subscriber)},
			io:format("dict:size is ~p~n", [dict:size(NewState#state.subscriber)]),
			checkSiblingSize(dict:size(NewState#state.subscriber), Reference, NewState);
		false -> 
			NewState = State,
			io:format("erbit_torrentManager:subscribe, handle_cast -> something is wrong, refrences are not match! ~n")
	end,
	{noreply, NewState};


%% a message to update downloaded amount status to the tracker
handle_cast({got_block, Reference, Byte}, State) when Reference =:= State#state.refernce ->
	{noreply, State#state{downloaded = State#state.downloaded + Byte}};

%% a message to send the complete message to the tracker
handle_cast({done_torrent, Reference}, State) when Reference =:= State#state.refernce->
	{noreply, State#state{event = completed}};

handle_cast(_Msg, State)->
	{noreply, State}.

%% ----------------------------------------------------
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
	io:format("erbit_torrentManager:terminate/2 -> working ~p~n",[Reason]),
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

%% A function to check all relevant process that have subscribed to the torrent manager or not
%% if they all have subscribed, then we start the erbit_torrentInfoManager with parsing
checkSiblingSize(7 , Reference, State)->
	io:format("erbit_torrentManager:checkSiblingSize -> is 8 and message to erbit_torrentInfoManager pid: ~p~n",
			  [dict:fetch(erbit_torrentInfoManager, State#state.subscriber)]),
	erbit_torrentInfoManager:parseTorrentFile(parse, Reference, dict:fetch(erbit_torrentInfoManager, State#state.subscriber),
										      State#state.fileAddress, State#state.subscriber, State#state.peerId);
checkSiblingSize(_ ,_,_)->
	ok.

%% A function to edit the address of the file to make it in a erlang way </html>
editAddress([], Acc) ->
	Acc;
editAddress ([H|T], Acc) ->
	case H of 
		$\\ ->
			editAddress(T, Acc ++ [$/]);
		_ -> 
			editAddress(T, Acc ++ [H])
	end.
