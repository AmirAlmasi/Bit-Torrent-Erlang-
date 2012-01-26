%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_torrentInfoManager.erl
%% @doc <html> The purpose of the module is to get the file in binary and parse all
%% data, also communicate with erbit_parser.erl, erbit_hashMaker.erl, and erbit_pieceHash.erl 
%% <br/>module has gen_server behavior </html>
%% @version 1.0.2
%% @date Created: Oct 23, 2011
%% @date Edited: Oct 23, 2011 - implement readFile/0 - start/1 functions
%% @date Edited: Nov 7, 2011 - change the way to get a path and open a torrent file and match it with tracker response
%% @date Edited: Nov 7, 2011 - change the way to get a path and open a torrent file and match it with tracker response
%% @date Edited: Nov 19, 2011 - change module name from dataManager to erbit_torrentInfoManager
%% @date Edited: Nov 19, 2011 - 
-module(erbit_torrentInfoManager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3, parseTorrentFile/6,showAnnounce/3, showAnnounceList/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {reference = undefined, torrentPid = undefined, sibling = dict:new(),
				bencodedData = undefined, hashUrl = undefined, hashBinary = undefined,
				directoryPath=undefined}).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> a function to start parsing torrent file after all processes are finished </html>
%% @spec parseTorrentFile(atom(), reference(), pid(), string(), list(), string()) -> ok
parseTorrentFile(parse, Reference, Pid, FileAddress, Sunscriber, PeerId) ->
	gen_server:cast(Pid, {parse, Reference, FileAddress, Sunscriber, PeerId}),
	ok.

%% @doc <html> a function to get the announce url of the torrent data </html>
%% @spec showAnnounce(atom(), reference(), pid()) -> ok
showAnnounce(announce, Reference, Pid) ->
	gen_server:call(Pid, {show_announce, Reference}).

%% @doc <html> a function to get the announce list urls of the torrent data </html>
%% @spec showAnnounceList(atom(), reference(), pid()) -> ok
showAnnounceList(announce_list, Reference, Pid)->
	gen_server:call(Pid, {show_announce_list, Reference}).


%% ====================================================================
%% Server functions
%% ====================================================================
%% @doc <html> a function to start the gen_server module  </html>
%% @spec start_link(reference(), pid(), string()) -> ok
start_link(Reference,Pid, DirectoryPath) ->
	gen_server:start_link(?MODULE, [Reference,Pid, DirectoryPath], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Reference,Pid, DirectoryPath]) ->
	io:format("erbit_torrentInfoManager:init/1 -> ~p working Reference ~p ~n", [self(), Reference]),
	erbit_torrentManager:subscribe(Pid, ?MODULE, Reference, self()),
 {ok, #state{reference = Reference, torrentPid = Pid, directoryPath = DirectoryPath}}.

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

%% get announce list url there is api function for this purpose
handle_call({show_announce_list, Reference}, _From, State) when Reference =:= State#state.reference ->
			%% announce list is an optional field so we should check if there is any
			case lists:member(<<"announce-list">>, dict:fetch_keys(State#state.bencodedData)) of
				true ->
					AnnounceList = dict:fetch(<<"announce-list">>, State#state.bencodedData),
%% 					io:format("erbit_torrentInfoManager:handle_call, showAnnounceList -> ~p announceList is: ~p~n", [self(), AnnounceList]),
					Reply = {ok, AnnounceList};
				false ->
					io:format("erbit_torrentInfoManager:handle_call, showAnnounceList -> ~p announceList there is NOT announce-list~n", [self()]),
					Reply = {error, not_announceList}
			end,
    {reply, Reply, State};



%% get announce list url there is api function for this purpose
%% {ok, Announce} | {error, wrong}
handle_call({show_announce, Reference}, _From, State) when Reference =:= State#state.reference->
			Announce = dict:fetch(<<"announce">>, State#state.bencodedData),
			io:format("erbit_torrentInfoManager:handle_call, showAnnounce -> ~p announce is: ~p~n", [self(), Announce]),
			Reply = {ok, Announce},
    {reply, Reply, State};


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

%% a function to start parsing the torrent file
handle_cast({parse, Reference, FileAddress, Sunscriber, PeerId}, State) when Reference =:= State#state.reference ->
			%% this one should be added at the end of the module
			
			case erbit_fileManager:readFile(Reference, dict:fetch(erbit_fileManager, Sunscriber), FileAddress) of
				{ok, Data} ->
					%% now I should bencode the data
					Bencoded = bencodeTorrent(Data),
					io:format("erbit_torrentInfoManager:handle_cast, parseTorrentFile -> Parse done~n"),
					{Hash, HashUrl} = erbit_hashMaker:getHash(Data),
					PieceDict = erbit_pieceHash:start(Bencoded),
					
					NewState= State#state{sibling = Sunscriber, bencodedData = Bencoded,
										  hashBinary = Hash , hashUrl = HashUrl},
					
					%% make the empty original file
					erbit_fileManager:makeEmptyFile(Reference, dict:fetch(erbit_fileManager, Sunscriber),
											        State#state.directoryPath ++ binary_to_list(dict:fetch(<<"name">>, dict:fetch(<<"info">>, Bencoded))),
                     dict:fetch(<<"length">>, dict:fetch(<<"info">>, Bencoded)), dict:fetch(<<"piece length">>, dict:fetch(<<"info">>, Bencoded)), Sunscriber),
					
					%% test reading file, it works
%% 					io:format("hahahahahahahha ~p~n",[erbit_fileManager:readPart(State#state.reference, dict:fetch(erbit_fileManager, Sunscriber),
%% 										 4, 63488, 4)]),
					
					
					%% A function to update the GUI
					updateGUI(Reference, Bencoded),
					
					%% send all pieces to piece checker
					%% erbit_pieceChecker
%% 					piecesHash(send_piece, send_sibling, Reference, Pid, PieceHashDict, SiblingDict),
					erbit_pieceChecker:piecesHash(Reference, dict:fetch(erbit_pieceChecker, NewState#state.sibling),
										           PieceDict, Sunscriber),
					
					erbit_trackerConnector:connectAnnounce(dict:fetch(erbit_trackerConnector, NewState#state.sibling),
													       Reference, dict:fetch(<<"announce">>, Bencoded), HashUrl, Hash, NewState#state.sibling),
					
				 erbit_peerConnector:keepInfo(dict:fetch(erbit_peerConnector, NewState#state.sibling), Reference,
									      dict:fetch(<<"length">>, dict:fetch(<<"info">>, Bencoded)),
										      dict:fetch(<<"piece length">>, dict:fetch(<<"info">>, Bencoded)),
										      PeerId, NewState#state.hashBinary, NewState#state.sibling),
					
%% 					io:format("dadadada adaddadada dadadadadada dadadadaddada ~n~p~n",[erlang:trunc(dict:fetch(<<"length">>, dict:fetch(<<"info">>, Bencoded)) / dict:fetch(<<"piece length">>, dict:fetch(<<"info">>, Bencoded)))]);
					
%% 					torrentManager:setInitBitField(NewState#state.torrentPid,
%% 												   Reference, erlang:round(dict:fetch(<<"length">>, dict:fetch(<<"info">>, Bencoded)) / dict:fetch(<<"piece length">>, dict:fetch(<<"info">>, Bencoded)))),
					erbit_messageProvider:setInitBitField(dict:fetch(erbit_messageProvider, NewState#state.sibling),
												         Reference, erlang:round(dict:fetch(<<"length">>, dict:fetch(<<"info">>, Bencoded)) / dict:fetch(<<"piece length">>, dict:fetch(<<"info">>, Bencoded))) + 1),

					erbit_handChecker:saveInfo(NewState#state.hashBinary, NewState#state.sibling),
					ok;
				
				{error, _} -> 
					io:format("torrentInfoManage:handle_cast, parseTorrentFile 
						there was a problem reading the torrent file, not contineing the process ~n"),
					NewState= State
			end,
    {noreply, NewState};

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
	io:format("erbit_torrentInfoManager:terminate/2 -> working ~p~n",[Reason]),
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

updateGUI (Reference, Bencoded)->
	erbit_core:updateName(Reference, dict:fetch(<<"name">>, dict:fetch(<<"info">>, Bencoded))),
	erbit_core:updateSize(Reference, dict:fetch(<<"length">>, dict:fetch(<<"info">>, Bencoded))).


bencodeTorrent(Data)->
	case erbit_parser:bencodeData(Data) of
		{ok, NewData} ->
			io:format("erbit_torrentInfoManager:bencodeTorrent/1 -> working ~n"),
			NewData;
		{error, unparsed} ->
			io:format("erbit_torrentInfoManager:bencodeTorrent/1 -> error, 
					not continuing the process ~n" , []),
			{error, unparsed};
		{error , Error, Reason} ->
			io:format("erbit_torrentInfoManager:bencodeTorrent/1 -> error, 
					not continuing the process , error ~p , reason ~p ~n" , [Error, Reason]),
			{error, Error, Reason};
		X -> 
			io:format(" ~n it did not pattern match ~p ~n", [X])
	end.
