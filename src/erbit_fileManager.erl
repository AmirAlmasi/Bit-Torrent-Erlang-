%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Retta Shiferaw <rsabitirta@gmail.com>
%% @module erbit_fileManager.erl
%% @doc <html>The purpose of the module is to handle reading and writing in file
%% <br/> Contributers: Farrokh tavakoli, readFile/1 -> read file normally 
%% <br/> Contributers: Amir Almasi, implement OTP behaviour, and estimate/4 -> to
%% update the aplication bitfield to the file which already downloaded in this
%% case we do not download the file that we already downloaded, application just 
%% starts seeding that one </html>

-module(erbit_fileManager).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start/0, start_link/2, readFile/3, makeEmptyFile/6, writeToFile/5, 
		 readPart/5, readPart/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {reference = undefined, torrentPid = undefined, filename = undefined,
				filesize = undefined, pieceSize = undefined, sibling = undefiend}).


%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> A function to read the file normally </html>
%% @spec readFile(refrence(), pid(), string()) -> {ok, data}
readFile(Reference, Pid, FileAddress)->
	gen_server:call(Pid, {readFile,Reference, FileAddress}).

%% @doc <html> A function write to the special part of the file </html>
%% @spec writeToFile(refrence(), pid(), byte(), integer(), integer()) -> ok
writeToFile(Reference, Pid, Data, BlockNumber, Offset) ->
	gen_server:cast(Pid, {write_to_file, Reference, Data, BlockNumber, Offset}),
	ok.

%% @doc <html> A function to write to the special part of the file </html>
%% @spec readPart(refrence(), pid(), byte(), integer(), integer()) -> ok
readPart(Reference, Pid, BlockNumber,Offset,Length) ->
	gen_server:call(Pid, {read_one_part, Reference, BlockNumber,Offset,Length}).

%% @doc <html> A function to read special part of the file </html>
%% @spec readPart(pid(), integer(), integer(), integer()) -> ok
readPart(Pid, BlockNumber,Offset,Length) ->
	gen_server:call(Pid, {read_one_part_uploader, BlockNumber,Offset,Length}).

%% @doc <html> A function to make the empty file with the exact size of the
%% original file. </html>
%% @spec makeEmptyFile(reference(), pid(), string(), integer(), integer(), list()) -> ok
makeEmptyFile(Reference,Pid, FileName, FileSize, PieceSize, Sibling) ->
	gen_server:cast(Pid, {empty_file, Reference, FileName, FileSize, PieceSize, Sibling}),
	ok.
%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> A function to start the gen_server </html>
%% @spec start() -> {ok, pid()}
start() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc <html> A function to start link the gen_server </html>
%% @spec start_link(reference(), pid()) -> {ok, pid()}
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
init([Reference, Pid]) ->
	io:format("erbit_fileManager:init/1 -> ~p working Reference ~p~n", [self(), Reference]),
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

%% read file in normal way 
handle_call({readFile,Reference, FileAddress}, _From, State) when Reference =:= State#state.reference ->
	Reply = readFile(FileAddress),
	{reply, Reply, State};

%% read special part of the file for downloading
handle_call({read_one_part, Reference, BlockNumber,Offset,Length}, _From, State) when Reference =:= State#state.reference->
	Reply = readOnePart({BlockNumber, Offset, Length}, State),
	{reply, Reply, State};

%% read special part of the file for uploading 
handle_call({read_one_part_uploader, BlockNumber,Offset,Length}, _From, State)->
	Block = readOnePart({BlockNumber, Offset, Length}, State),
	Reply = {send,BlockNumber, Offset,Block},
	updateGUI(State#state.reference, Length),
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


%% to make the empty file at first
handle_cast({empty_file, Reference, FileName, FileSize, PieceSize, Sibling}, State) when Reference =:= State#state.reference->
	NewState = State#state{filename = FileName , filesize = FileSize, pieceSize = PieceSize, sibling= Sibling},
	case file:open(FileName,[read, raw , binary]) of
		{ok, Io} ->
			file:close(Io),
			io:format("erbit_fileManager:handle_csat, makeEmptyFile-> File already exist ~n"),
			%% 					erbit_fileEstimator should be called in here which should return the bitfiled as well
			io:format("sibling is ~p~n",[Sibling]),
			estimate(FileSize, PieceSize, 0, NewState);
		_Error -> 
			io:format("erbit_fileManager:handle_csat, makeEmptyFile-> File creating working fileName: ~p filesize: ~p ~n",[FileName, FileSize]),
			makeFile(FileName, FileSize)
	end,
	%% 	io:format("Table id is: ~p~n" , [State#state.tableId]),
	{noreply, NewState};

%% to write to special part of the file
handle_cast({write_to_file, Reference, Data, BlockNumber, Offset}, State)->
	case Reference =:= State#state.reference of 
		true ->
			%% 			io:format("writeToFile test ~p~n",[Data]),
			writePart({Data, BlockNumber, Offset}, State),
			ok;
		false -> 
			io:format("erbit_fileManager:handle_cast, writetofile -> something is wrong, refrences are not match! ~n")
	end,
	{noreply, State};



handle_cast(_Msg, State)->
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
	io:format("erbit_fileManager:terminate/2 -> working ~p~n",[Reason]),
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


%% A funtion to be run when we have the file already downloaded and we just read it again
%% and update the bitfield to the already correct downloaded
estimate(FileSize, PieceSize, BlockNumber, State) ->
	IndexValue = FileSize - (BlockNumber * PieceSize),
	case IndexValue < PieceSize of
		true -> 
			{ok, Piece} = readOnePart({BlockNumber, 0, IndexValue}, State),
			erbit_pieceChecker:checkData(dict:fetch(erbit_pieceChecker, State#state.sibling), Piece, BlockNumber);
		false ->
			io:format("file is ~p~n",[BlockNumber]),
			{ok, Piece} = readOnePart({BlockNumber, 0, PieceSize}, State),
			erbit_pieceChecker:checkData(dict:fetch(erbit_pieceChecker, State#state.sibling), Piece, BlockNumber),
			estimate(FileSize, PieceSize, BlockNumber+1, State)
	end,
	
	ok.


%% a function to write to the special part of the file
writePart({Data, BlockNumber, Offset}, State)->
	Location=(BlockNumber * State#state.pieceSize) + Offset,
	{ok,Io}=file:open(State#state.filename,[read,write,raw,binary]),
	%% 	io:format("retta test location : ~p ~n", [Location]),
	file:pwrite(Io,Location,Data),
	file:close(Io),
	ok.

%% A function to read the data from part of the file 
readOnePart({BlockNumber, Offset, Length}, State)->
	io:format("Reading started ~n"),
	Location=(BlockNumber * State#state.pieceSize) + Offset,
	{ok,Io}=file:open(State#state.filename,[read,raw,binary]),
	Response=file:pread(Io,Location,Length),
	case Response of
		eof->
			io:format("erbit_fileManager:readPart -> endOfFile~n");
		_ ->
			ok
	end,
	file:close(Io),
	Response.


%% a function to make the empty file at first
makeFile(FileName, FileSize)->
	{ok,Io}=file:open(FileName,[write,raw,binary]),
	Bsize=4096,
	Bdata=binary:copy(<<0>>,Bsize),
	[ok=file:write(Io,Bdata)||_<-lists:seq(1,FileSize div Bsize)],
	BremSize=FileSize rem Bsize,
	BremData=binary:copy(<<0>>,BremSize),
	Reply=file:write(Io,BremData),
	file:close(Io),
	Reply.



%% @doc <html>A function to start reading the file </html>
%% @spec readFile(list())-> {ok, binary()} | {error, tuple()}
readFile(FileAddress) ->
	case file:read_file(FileAddress) of
		{ok, Data} ->
			io:format("erbit_fileManager:readFile -> working , file has been read properly~n"),
			{ok, Data};
		Unecpected -> 
			io:format("erbit_fileManager:readFile was called -> There was an error reading torrent file ~p~n", [Unecpected]),
			{error, Unecpected}
	end.

updateGUI(Reference, Byte) ->
	erbit_core:updateUpload(Reference, Byte).
