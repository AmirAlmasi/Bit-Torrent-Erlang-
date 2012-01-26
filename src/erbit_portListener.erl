%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Mikael Arvidsson
%% @module erbit_portListener.erl
%% @doc <html> module for listening to the port
%% <br/> Contributers: Amir Almasi, implement OTP behaviour for the module </html>
-module(erbit_portListener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, setPort/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listenSocket = undefined, sibling = dict:new(), port = undefined,
				peerId = undefined}).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc <html> A function to change the port </html>
%% @spec setPort(port())-> ok
setPort(Port) ->
	gen_server:cast(?MODULE, {port, Port}),
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

%% @doc <html> A function to start gen_server </html>
%% @spec start_link(string(), port())-> {ok, port()}
start_link(PeerId, Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [PeerId, Port], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([PeerId, Port]) ->
%% 	io:format("erbit_portListener:init/1 -> port ~p listening ~p~n",[Port, self()]),
	timer:send_after(0, self(), {listen, port}),
    {ok, #state{port = Port, peerId = PeerId}}.

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
%% a function to set the port 
handle_cast({port, Port}, State) ->
	{ok, ListenSocket} = gen_tcp:listen(Port,[binary,{active,false},{packet,raw}]),
	 
	loop_connect(ListenSocket, State#state.peerId),
    {noreply, State#state{listenSocket = ListenSocket}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% a function to 
handle_info({listen, port}, State) ->
	{ok, ListenSocket} = gen_tcp:listen(State#state.port,[binary,{active,false},{packet,raw}]),
	%% for the time being 
	loop_connect(ListenSocket,State#state.peerId),
    {noreply, State#state{listenSocket = ListenSocket}};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	io:format("erbit_portListener:terminate/2 -> working ~p~n",[Reason]),
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

loop_connect(ListenSocket,Peer_ID) ->
case gen_tcp:accept(ListenSocket) of
{ok,Socket} -> 
Pid = spawn(peerHandeler, peerHandeler, [Socket,Peer_ID]),
gen_tcp:controlling_process(Socket,Pid),
loop_connect(ListenSocket,Peer_ID);
{error,Reason} -> io:format("There was an error on listener accept: ~w~n",[Reason]),
loop_connect(ListenSocket,Peer_ID)
end.