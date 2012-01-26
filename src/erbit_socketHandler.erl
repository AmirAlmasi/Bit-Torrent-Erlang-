%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Farrokh Tavakoli Banizi & Amir Almasi<farrokh-t-b@hotmail.com>
%% @module erbit_socketHandler.erl
%% @doc <html>A module for implementing ets and take care of all sockets if there would
%% be any new it will be added to ets
%% <br/> Contributers: Amir Almasi, implement OTP behaviour, getting query from ets, 
%% comparing new sockets </html>
-module(erbit_socketHandler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, deleteSocket/2 , processSocket/4, processUdp/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
		 changeStatus/3, updatePid/4, sendHave/3]).

-record(state, {reference = undefined, torrentPid = undefined, sibling = dict:new(),
				tableId = undefined, status = true}).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc <html> A function process http tracker response </html>
%% @spec processSocket(pid(), refernce(), list(), list())-> ok
processSocket(Pid, Reference, Sockets, Sibling) ->
	gen_server:cast(Pid, {process_socket, Reference, Sockets, Sibling}),
	ok.

%% @doc <html> A function process udp tracker response </html>
%% @spec processUdp(pid(), reference(), list(), list())-> ok
processUdp(Pid, Reference, Sockets, Sibling) ->
	gen_server:cast(Pid, {process_Udp, Reference, Sockets, Sibling}),
	ok.

%% @doc <html> A function to delete special socket from ets table</html>
%% @spec deleteSocket(pid(), tuple())-> ok
deleteSocket(Pid, Ip) ->
	gen_server:cast(Pid, {delete_broken_peer, Ip}),
	ok.

%% @doc <html> A function to change the status after completing downloading file</html>
%% @spec changeStatus(pid(), reference(), atom())-> ok
changeStatus(Pid, Reference, Status) ->
	gen_server:cast(Pid, {change_status, Reference, Status}),
	ok.

%% @doc <html> A function to update the ets key </html>
%% @spec updatePid(pid(), reference(), tuple(), tuple())-> ok
updatePid(Pid, Reference, Ip, PeerPid) ->
	gen_server:cast(Pid, {update_pid, Reference, Ip, PeerPid}),
	ok.

%% @doc <html> A function to send have message to the connected peers </html>
%% @spec sendHave(pid(), reference(), integer())-> ok
sendHave(Pid, Reference, BlockNumber) ->
	gen_server:cast(Pid, {make_query, Reference, BlockNumber}),
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> API function to start the process</html>
%% @spec start_link(reference(), pid())-> ok
start_link(Reference,Pid)->
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
%% 	io:format("erbit_socketHandler:init/1 -> working ~n", []),
	erbit_torrentManager:subscribe(Pid, ?MODULE, Reference, self()),
	TableId=ets:new(?MODULE,[set]),
	{ok, #state{reference = Reference, torrentPid = Pid, tableId = TableId}}.

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

%% a function to send back the query of free ip and port to peer connetion 

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

%% 
handle_cast({process_socket, Reference, Sockets, Sibling}, State) when Reference =:= State#state.reference ->
	case State#state.status of
		true ->
			NewState = State#state{sibling = Sibling},
			processTraRes(NewState,Sockets,[]),
			ok;
		false ->
			NewState = State
	end,
	{noreply, NewState};

handle_cast({delete_broken_peer, Ip}, State) ->
	deleteBrokenPeers(Ip, State#state.tableId),
	{noreply, State};


handle_cast({process_Udp, Reference, Sockets, Sibling}, State) when Reference =:= State#state.reference ->
	NewState = State#state{sibling = Sibling},
	splitIp(Sockets, NewState),
	%% 	tv:start(),
	{noreply, NewState};


handle_cast({change_status, Reference, Status}, State) when State#state.reference =:= Reference ->
	{noreply, State#state{status = Status}};


handle_cast({update_pid, Reference, Ip, PeerPid}, State) when State#state.reference =:= Reference ->
	updatePid(State#state.tableId, Ip, PeerPid),
	{noreply, State};

handle_cast({make_query, Reference, BlockNumber}, State) when Reference =:= State#state.reference ->
	Test = queryReadySockets(State#state.tableId),
	io:format("___________________________________~ntest is test ~p~n___________________________________~n",[Test]),
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
	io:format("erbit_socketHandler:terminate/2 -> working ~p~n", [Reason]),
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



%% a function to update the gui
updateGUI(Reference, PeerList)->
	erbit_core:updatePeerList(Reference, PeerList).


queryReadySockets(TableId) ->
	Pattern = {'$0', '$1', '$2', '$3'},
	ets:match(TableId, Pattern).



processTraRes(State ,TrackerResponse,_Acc) ->
	%% 	writeToFile(TrackerResponse),
	case parseTrackerResponse(TrackerResponse) of 
		{ok, BencodedResponse} ->
			
			%% 			write the tracker response to the file for debuging purpose 
			%% 			io:format("~p~n",[dict:fetch_keys(BencodedResponse)]),
			
%% 			io:format("erbit_socketHandler:processTraRes -> interval is ~p~n", [dict:fetch(<<"interval">>, BencodedResponse)]),
%% 			io:format("erbit_socketHandler:processTraRes -> complete is ~p~n", [dict:fetch(<<"complete">>, BencodedResponse)]),
%% 			io:format("erbit_socketHandler:processTraRes -> incomplete is ~p~n", [dict:fetch(<<"incomplete">>, BencodedResponse)]),
			updateGUI(dict:fetch(<<"complete">>, BencodedResponse), dict:fetch(<<"incomplete">>, BencodedResponse), State),
			%% 			io:format("Response is ~p~n",[dict:fetch(<<"peers">>, BencodedResponse)]),
			%% 			writeToFile(TrackerResponse),
			case dict:fetch(<<"peers">>, BencodedResponse) of
				[] ->
					updateGUI(State#state.reference, []),
%% 					io:format("erbit_socketHandler:processTraRes -> there is no peer to download ~n"),
					ok;
				PeerList ->
					splitIp(PeerList, State)
			end;
		Unexpected ->
 			io:format("erbit_socketHandler:processTraRes-> error parsing data from tracker ~p ~n",[Unexpected])
%%  			ok
	end,
	%% 	tv:start(),
	ok.

splitIp(PeerList, State) ->
	IpList = seperateIpsPorts(PeerList, [], State),
	deleteOffPeer(ets:match(State#state.tableId, {'$0', '$1', '_', '_'}), IpList, State#state.tableId),
	%% update GUI
	updateGUI(State#state.reference, queryReadySockets(State#state.tableId)),
	%% 					io:format("erbit_socketHandler:processTraRes -> ~p~n", [IpList]),
	ok.

deleteOffPeer(EtsKeys, IpList, TableId) ->
	%% 	io:format("erbit_socketHandler:deleteOFFpeer ->  this peer went off ~p~n", [lists:subtract( EtsKeys, IpList)]),
	[ets:delete(TableId,IP) || [IP, _Port] <- lists:subtract(EtsKeys, IpList)],
	ok.

deleteBrokenPeers(Ip, TableId) ->
	%% 	io:format("deleteBrokenPeers ~p",[Ip]),
	ets:delete(TableId ,Ip).

%% writeToFile(DataList) ->
%% 	file:write_file("debugTrackerResponse", DataList).

parseTrackerResponse(TrackerResponse) ->
	erbit_parser:bencodeData(list_to_binary(TrackerResponse)).


seperateIpsPorts(<<>>, Acc, _) ->
	Acc;
seperateIpsPorts(<<Ip:32, Tail/binary>>, Acc, State) ->
	<<Port:16, _Tail/binary>> = Tail,
	RealIp = converIp(Ip, []),
	Pattern = {'$0', '_', '_', '_'},
	EtsKeys = lists:flatten(ets:match(State#state.tableId, Pattern)),
	case lists:member(RealIp, EtsKeys) of 
		false ->
			%% 			io:format("processTraRes:seperateIpPorts -> A new key was added to the ets~p~n", [RealIp]),
			ets:insert(State#state.tableId,{RealIp, Port, false, false}),
			erbit_peerConnector:connectSocket(dict:fetch(erbit_peerConnector, State#state.sibling),
											  State#state.reference, {RealIp, Port}),
			ok;
		true ->
			%% 			io:format("processTraRes:seperateIpPorts -> Ip and port already exists ~n"),
			ok
	end,
	seperateIpsPorts(_Tail, [[converIp(Ip, []), Port]|Acc], State).


%% @doc <html>A function to convert the integer ip number to it's normal view</html>
%% @spec converIp(integer(), list())-> list()
converIp(Integer, Acc) when Integer < 256 ->
	list_to_tuple([Integer | Acc]);
converIp (Integer, Acc) ->
	converIp((Integer div 256), [Integer rem 256 | Acc]).


updateGUI(Seeders, Leechers, State) ->
	erbit_core:updateSeeder(State#state.reference, Seeders, Leechers).

updatePid(TableId, Ip, Pid)->
	case ets:update_element(TableId,Ip,{4,Pid}) of
		true->
			update;
		false ->
			error
	end.