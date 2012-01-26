%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @module erbit_trackerConnector.erl
%% @doc <html> This module is for connecting to the http trackers 
%%% Implement OTP behaviour: Amir Almasi - normal massage passing was changed to otp behaviuor </html>

-module(erbit_trackerConnector).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2,connectAnnounce/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {reference = undefined, torrentPid = undefined, sibling = dict:new(),
				hashUrl = undefined, trackerAddress = [], announceList = undefined, hash =undefined }).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> A function to connect to both http and udp trackers </html>
%% @spec connectAnnounce(pid(), reference(), string(), string(), binary(), list()) -> ok
connectAnnounce(Pid, Refernce, TrackerAddress, HashUrl, Hash, Sibling) ->
	gen_server:cast(Pid, {connect_tracker, Refernce, TrackerAddress, HashUrl, Hash, Sibling}),
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> A function to start gen_server </html>
%% @spec start_link(reference(), pid()) -> ok
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
	timer:send_interval(2000, self(), {connect, tracker, Reference}),
%% 	io:format("erbit_trackerConnector:init/1 -> ~p working Reference ~p~n", [self(), Reference]),
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

%% this function is to handle the tracker url, first we should understand if
%% it is udp or http
handle_cast({connect_tracker, Refernce, TrackerAddress, HashUrl, Hash, Sibling}, State) ->
	case Refernce =:= State#state.reference of
		true ->
			case erbit_torrentInfoManager:showAnnounceList(announce_list, Refernce, dict:fetch(erbit_torrentInfoManager,Sibling)) of
				{error,not_announceList} ->
					NewState = State#state{trackerAddress = [TrackerAddress],
								   hashUrl = HashUrl, hash = Hash, sibling = Sibling},
					ok;
				{ok, AnnounceList} -> 
					%% A list comes in here 
%% 					io:format("erbit_trackerConnector:handle_cast, connectTracker ~p ~n", [lists:flatten(AnnounceList)]),
					NewState = State#state{trackerAddress = [TrackerAddress | lists:flatten(AnnounceList) ],
								   hashUrl = HashUrl, hash = Hash, sibling = Sibling},
					ok
			end,
%% 			io:format("erbit_trackerConnector:cast, connectAnnounce -> trackerAddress: ~p~n",[NewState#state.trackerAddress]),
			ok;
		false -> 
			NewState = State,
			io:format("erbit_torrentManager:handle_call, showAnnounceUrl -> something is wrong, refrences are not match! ~n")
	end,
	timer:send_after(0, self(), {connect, tracker, State#state.reference}),
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

%% a function to connect to the trackers for period of the time
handle_info({connect, tracker, Reference}, State) ->
	
	case Reference =:= State#state.reference of 
		true ->
			case State#state.hashUrl of
				undefined ->
					io:format("erbit_trackerConnector:handle_info, each sec -> hashUrl is undefined ~n");
				_ ->
					%% basically the hash url is set and now we connect
					Reply = erbit_torrentManager:trackerValues(State#state.torrentPid, State#state.reference),
					
%% 					updateGUI(State#state.trackerAddress, State),
					
 					connectToAnnounce(State#state.trackerAddress, Reply, State),
					
					
					%% 					io:format("~p ~p~n", [State#state.trackerAddress, is_list(State#state.trackerAddress)]),
					%% 					io:format("~p ~p~n", [PeerId, is_list(PeerId)]),
					%% 					io:format("~p ~p~n", [Port, is_integer(Port)]),
					%% 					io:format("~p ~p~n", [Uploaded, is_list(Uploaded)]),
					%% 					io:format("~p ~p~n", [Downloaded, is_list(Downloaded)]),
					%% 					io:format("~p ~p~n", [Left, is_list(Left)]),
					%% 					io:format("Event Numwant ~p ~p~n", [Event, is_list(Event)]),
					%% 					io:format("~p ~p~n", [Numwant, is_list(Numwant)]),
					%% 					io:format("~p ~p~n", [No_peer_id, is_list(No_peer_id)]),
					%% 					io:format("~p ~p~n", [Compact, is_list(Compact)]),
					
					ok
			%% 					io:format("~p~n", [Url])
			end,
			%% 			io:format("I wanna connect to the tracker ~n");
			%% 			io:format("erbit_trackerConnector:cast, connectAnnounce -> TrackerAdderss ~p ~n",
			%% 			  [State#state.trackerAddress ++ State#state.hashUrl]);
			ok;
		false -> 
			io:format("erbit_trackerConnector:handle_info, each sec -> something is wrong, refrences are not match! ~n")
	end,
	{noreply, State};


handle_info({upd_answer, Data }, State) ->
	udpPeers(Data, State),
	{noreply, State};

handle_info(Info, State) ->
	io:format("tracker Connector got an unknown message ~p~n",[Info]),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	io:format("erbit_trackerConnector:terminate/2 -> working ~p~n",[Reason]),
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

%% a function to make the url to connect to the announce tracker
connectToAnnounce([], _Reply, _State) ->
	done;
%% a function to have event 
connectToAnnounce([Head|Tail], {9, Value}, State) ->
	{PeerId, Port, Uploaded, Downloaded, Left, Event, Numwant, No_peer_id, Compact} = Value,
	
	case lists:sublist(binary_to_list(Head), 3) of
		"htt" ->
			Url=binary_to_list(Head) ++ "?info_hash=" ++ State#state.hashUrl ++"&peer_id=" 
																			   ++ PeerId ++ "&port="++ integer_to_list(Port) ++ "&uploaded=" ++ integer_to_list(Uploaded)
																			   ++ "&downloaded=" ++ integer_to_list(Downloaded) ++ "&left=" ++ integer_to_list(Left)
																			   ++ "&event=" ++ atom_to_list(Event) ++ "&numwant=" ++ integer_to_list(Numwant)
																			   ++ "&no_peer_id=" ++ integer_to_list(No_peer_id) ++ "&compact=" ++ integer_to_list(Compact),
			case connectHttp(Url) of 
				{ok, Body} ->
					httpPeers(Body, State);
				_ ->
					error
			end;
		"udp" ->
			Address = lists:subtract(binary_to_list(Head), "udp://"),
			UrlPort =lists:sublist(Address,length(Address) - 9),
			{ok, [FirstIp|_IpList]} = inet:getiflist(),
			IntegerIp = converIp(list_to_binary(FirstIp), [] , <<>>),
			_Answer = erbit_udpHandler:start(self(), UrlPort, {9, Value}, list_to_binary(erbit_makeId:makeTransId()), State#state.hash, IntegerIp),

%% 			io:format("Mike ~p~n", [_Answer]),
			ok
	end,
	connectToAnnounce(Tail, {9, Value}, State);


%% a function to omit event 
connectToAnnounce([Head|Tail], {8, Value}, State) ->
	{PeerId, Port, Uploaded, Downloaded, Left, Numwant, No_peer_id, Compact} = Value,
	
	case lists:sublist(binary_to_list(Head), 3) of
		"htt" ->
			Url=binary_to_list(Head) ++ "?info_hash=" ++ State#state.hashUrl ++"&peer_id=" 
																			   ++ PeerId ++ "&port="++ integer_to_list(Port) ++ "&uploaded=" ++ integer_to_list(Uploaded)
																			   ++ "&downloaded=" ++ integer_to_list(Downloaded) ++ "&left=" ++ integer_to_list(Left)
																			   ++ "&numwant=" ++ integer_to_list(Numwant)
																			   ++ "&no_peer_id=" ++ integer_to_list(No_peer_id) ++ "&compact=" ++ integer_to_list(Compact),
			case connectHttp(Url) of 
				{ok, Body} ->
					httpPeers(Body, State);
				_ ->
					error
			end;
		"udp" ->
			Address = lists:subtract(binary_to_list(Head), "udp://"),
			UrlPort =lists:sublist(Address,length(Address) - 9),
			{ok, [FirstIp|_IpList]} = inet:getiflist(),
			IntegerIp = converIp(list_to_binary(FirstIp), [] , <<>>),
			Answer = erbit_udpHandler:start(self(), UrlPort, {8, Value}, list_to_binary(erbit_makeId:makeTransId()), State#state.hash, IntegerIp),

			io:format("Mike ~p~n", [Answer])
	end,
	connectToAnnounce(Tail, {8, Value}, State).
	


converIp(<<>>, Acc, Binary) ->
	Number = list_to_integer(Acc),
	<<Binary/binary, Number:8>>;

converIp(<<$. , Tail/binary>>, Acc, Binary) ->
%% 	Test = list_to_binary(Acc),
	Number = list_to_integer(Acc),
	converIp(Tail, [], <<Binary/binary, Number:8>>);

converIp(<<Num:8 , Tail/binary>>, Acc, Binary) ->
	converIp(Tail, Acc ++ [Num], Binary).%% <<Num:1 , Acc/binary>>).



%% a function for httpc request to coonect to the tracker
connectHttp(Url) ->
	inets:start(),
	case httpc:request(get, {Url, []}, [{connect_timeout, 1000}], []) of
		{ok, {{_Version, _Page, _ReasonPhrase}, _Headers, Body}} -> 
			{ok, Body};
		_ ->
			{error, tracker}
	end.
	%% 	io:format("erbit_trackerConnector:connectToTracker/1 -> _Version ~p _Page ~p ReasonPhrase ~p _Headers ~p ~n",[_Version, _Page,ReasonPhrase,_Headers]),
	%% 	io:format("erbit_trackerConnector:connectToTracker/1 -> Body ~p ~n", [Body]),
	




httpPeers(List, State)->
	erbit_socketHandler:processSocket(dict:fetch(erbit_socketHandler, State#state.sibling), State#state.reference, List, State#state.sibling).

udpPeers(List, State) ->
	erbit_socketHandler:processUdp(dict:fetch(erbit_socketHandler, State#state.sibling), State#state.reference, List, State#state.sibling),
	ok.

updateGUI(TrackerAddress, State) ->
	%% here I should put io:format(Format)
	io:format("updateGUI is working ~p~n", [TrackerAddress]),
	erbit_core:updateTracker(State#state.reference, TrackerAddress).


