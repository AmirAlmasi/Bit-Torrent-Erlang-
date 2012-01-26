%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_messageProvider.erl
%% @doc <html> a module to provide all messages and keep the bitfield </html>
%% @version 0.1

-module(erbit_messageProvider).
-behaviour(gen_server).

%% In version 1.0 of the BitTorrent protocol, pstrlen = 19, and pstr = "BitTorrent protocol".
-define(PROTOCOL_IDENTIFIER, "BitTorrent protocol").
-define(LENGTH_IDENTIFIER , 19).
-define(RESERVED, <<0:64>>).


%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, setInitBitField/3, getAblock/2, getAblock/1, makeHandShake/4,
		 gotBlock/3, getBitfield/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {reference = undefined, torrentPid = undefined, sibling = dict:new(),
				pieceNumber = undefined, bitField = undefined, counter = 0}).


%% ====================================================================
%% External functions
%% ====================================================================


%% @doc <html> A function to be called when downloaded a block </html>
%% @spec getAblock(pid(), reference()) -> {ok, integer()} | {done, downloaded}
getAblock(Pid, Reference)->
	gen_server:call(Pid, {get_block, Reference}).

%% @doc <html> A function to be called when downloaded a block </html>
%% @spec getAblock(pid()) ->{ok, integer()} | {done, downloaded}
getAblock(Pid)->
	gen_server:call(Pid, {get_block, not_reference}).

%% @doc <html> A function to set the init bitfield at first with piece number </html>
%% @spec setInitBitField(pid(), reference(), integer()) -> ok
setInitBitField(Pid, Reference, PieceNumber)->
	gen_server:cast(Pid, {set_init_bitField, Reference, PieceNumber}),
	ok.

%% @doc <html> A function to provide the handshake message </html>
%% @spec makeHandShake(pid(), reference(), binary(), string()) -> binary()
makeHandShake(Pid, Reference, InfoHash, PeerId) ->
	gen_server:call(Pid, {handshake, Reference, InfoHash, PeerId}).


%% @doc <html> A function to check the current bitfield and edit it </html>
%% @spec gotBlock(pid(), reference(), integer()) -> byte()
gotBlock(Pid, Reference, PieceNumber)->
	gen_server:call(Pid, {got_block, Reference, PieceNumber}).

%% @doc <html> A function to be called when cheching a block </html>
%% @spec getBitfield(pid()) -> {ok, binary()} | {error, first}
getBitfield(Pid) ->
	gen_server:call(Pid, {get_bitfield}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> A function to start the gen_server </html>
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
%% 	io:format("erbit_messageProvider:init/1 -> ~p working Reference ~p~n", [self(), Reference]),
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

%% a function to make the handshake for the peer
handle_call({handshake, Reference, InfoHash, PeerId}, _From, State) when Reference =:= State#state.reference ->
	PeerIdBinary = list_to_binary(PeerId),
	Reply = <<?LENGTH_IDENTIFIER , ?PROTOCOL_IDENTIFIER, 0:64, InfoHash:20/binary , PeerIdBinary:20/binary>>,
	{reply, Reply, State};

%% a message to check and edit the bitfield 
handle_call({got_block, Reference, PieceNumber}, _From,  State) when Reference =:= State#state.reference->
%% 	io:format("we got a block ~p~n",[PieceNumber]),
	%% Edit bitfield 
	<<Head:PieceNumber , _Element:1 , Tail/bits>> = State#state.bitField,
	case _Element of
		0 ->
			NewState = State#state{bitField = <<Head:PieceNumber , 1:1, Tail/bits>>},
			io:format("bitfield is ~p~n",[NewState#state.bitField]),
			Reply = true;
		1 ->
			NewState = State,
			Reply = false
	end,	
	{reply, Reply, NewState};

%% a function to get the bitfield for uploading to be sent to other peers
handle_call({get_bitfield}, _From, State) ->
	Bitfield = roundUp(State#state.pieceNumber / 8) * 8,
	case State#state.bitField of
		<<0:Bitfield>> ->
			Reply = {error, first};
		Data ->
			Reply = {ok, Data}
	end,
	{reply, Reply, State};


handle_call({get_block, not_reference}, _From, State) ->
	case findBlock(State#state.counter, State, 0) of
		{ok, Number} ->
			Reply = {ok, Number},
			NewState = State#state{counter =  Number +1},
			ok;
		{error, file_downloaded} ->
			Reply = {done, downloaded},
			NewState = State
	end,		
	{reply, Reply, NewState};

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

%% a function to set the first bitfield based on block number
handle_cast({set_init_bitField, Reference, PieceNumber}, State) ->
	case Reference =:= State#state.reference of 
		true ->
			%% making spare bits
			Bitfield = roundUp(PieceNumber / 8) * 8,
			NewState = State#state{bitField = <<0:Bitfield>>, pieceNumber = PieceNumber};
		false -> 
			io:format("erbit_messageProvider:handle_call, getAblock -> something is wrong, refrences are not match! ~n"),
			NewState = State
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
	io:format("erbit_messageProvider:terminate/2 -> working ~p~n",[Reason]),
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

findBlock(Counter, State, 100) when Counter >= State#state.pieceNumber ->
	{error, file_downloaded};

findBlock(Counter, State, Num) when Counter >= State#state.pieceNumber->
	findBlock(0, State, Num+1);

findBlock(Counter, State,Num) when Counter < State#state.pieceNumber ->
	Before = Counter,
	<<_:Before , _Element:1 , _/bits>> = State#state.bitField,
	case _Element of 
		0 ->
			{ok, Counter};
		1 ->
			findBlock(Counter+1, State, Num)
	end.

%% keepAlive() ->
%% 	<<0:32>>.
%% 
%% choke() ->
%% 	<<0:24,1:8,0:8>>.
%% 
%% unchoke() ->
%% 	<<0:24,1:8,1:8>>.
%% 
%% interested() ->
%% 	<<1:32, 2:8>>.
%% 
%% notInterested() ->
%% 	<<0:24,1:8,3:8>>.
%% 
%% have(SavedBlockNum) ->
%% 	<<0:24,5:8,4:8,SavedBlockNum:8>>.



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