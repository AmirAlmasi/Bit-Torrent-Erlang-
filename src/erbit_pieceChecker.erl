%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_pieceChecker.erl
%% @doc <html> A module for keeping pieces hash and compare them with the downloaded data </html>
-module(erbit_pieceChecker).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, piecesHash/4, checkData/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {reference = undefined, torrentPid = undefined, sibling = dict:new(),
				piecesHash = dict:new()}).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> A function to get the all piece's hash </html>
%% @spec piecesHash(reference(), pid(), list(), list()) -> ok
piecesHash(Reference, Pid, PieceHashDict, SiblingDict) ->
	gen_server:cast(Pid, {Reference, pieces , PieceHashDict , sibling, SiblingDict}),
	ok.

%% @doc <html> A function to check the data based on it's piece number </html>
%% @spec checkData(pid(), binary(), integer()) -> ok
checkData(Pid, Data, PieceNumber) ->
	gen_server:cast(Pid, {check_data, Data, PieceNumber}),
	ok.


%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> A function to start gen_server </html>
%% @spec start_link(refernce(), pid()) -> {ok, pid()}
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
%% 	io:format("erbit_pieceChecker:init/1 -> ~p working Reference ~p~n", [Pid, Reference]),
	erbit_torrentManager:subscribe(Pid, ?MODULE, Reference, self()),
	{ok, #state{reference = Reference, torrentPid= Pid}}.

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


%% to get the all piece's hash and keep them
handle_cast({Reference, pieces , PieceHashDict , sibling, SiblingDict}, State) ->
	case Reference =:= State#state.reference of 
		true ->
%% 			io:format("erbit_pieceChecker:handle_cast, piecesHash-> got all piece hashes and siblings ~n"),
			NewState = State#state{sibling = SiblingDict, piecesHash = PieceHashDict},
			%% 			?MODULE:checkData(self(), 1, 0),
			ok;
		false ->
			NewState = State,
%% 			io:format("erbit_pieceChecker:handle_cast, piecesHash -> something is wrong, refrences are not match! ~n"),
			ok
	end,
	{noreply, NewState};


%% To check the data's SHA1, if it matches then data was downloaded correctly
handle_cast({check_data, Data, PieceNumber}, State) ->
	Binary = dict:fetch(PieceNumber, State#state.piecesHash),
	case crypto:sha(Data) =:= <<Binary:160>> of
		true ->
%% 			io:format("erbit_pieceChecker:handle_cast, checkData -> pieceNumber ~p checked, correct~n",[PieceNumber]),
			case erbit_messageProvider:gotBlock(dict:fetch(erbit_messageProvider, State#state.sibling), State#state.reference, PieceNumber) of
				true->
					Bytes = size(Data),
					updateGUI(State#state.reference, Bytes),
					erbit_socketHandler:sendHave(dict:fetch(erbit_socketHandler, State#state.sibling), State#state.reference, PieceNumber),
					OffSet = 0,
					erbit_fileManager:writeToFile(State#state.reference, dict:fetch(erbit_fileManager, State#state.sibling),
											      Data, PieceNumber, OffSet),
					
					erbit_torrentManager:gotBlock(State#state.torrentPid, State#state.reference, Bytes);
				false ->
					already_have_block
			end;
		false ->
			io:format("erbit_pieceChecker:handle_cast, checkData -> pieceNumber ~p checked, INCORRECT",[PieceNumber])
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
	io:format("erbit_pieceChecker:terminate/2 -> working ~p~n", [Reason]),
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
updateGUI(Reference, Size) ->
	erbit_core:updateDownload(Reference, Size).
