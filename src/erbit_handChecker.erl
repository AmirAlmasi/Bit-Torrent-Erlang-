%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_handChecker.erl
%% @doc <html> The purpose of the module is to check the info hash of other peers peer
%% that got application ip and port from tracker and they want to download </html>

-module(erbit_handChecker).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports

-export([start_link/0, saveInfo/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {infoHash = dict:new()}).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc <html> A function to keep all info hashes for all torrents in the 
%% application </html>
%% @spec saveInfo(byte(), list()) -> ok
saveInfo(Info, Sibling) ->
	gen_server:cast(?MODULE, {keep_info, Info, Sibling}),
	ok.
%% ====================================================================
%% Server functions
%% ====================================================================
%% @doc <html> A function to start the gen_server </html>
%% @spec start_link() -> {ok, pid()}
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	io:format("handCheker:init/0 -> working ~n"),
    {ok, #state{}}.

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

%% a function to keep the info hash per each torrent
handle_cast({keep_info, Info, Sibling}, State) ->
%% 	io:format("erbit_handChecker:keepInfo, handle_cast -> Info ~p is kept ~n" , [Info]),
    {noreply, State#state{infoHash = dict:store(Info, Sibling, State#state.infoHash)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% a function to check the external peer's handshake if it has the same info hash or not 
handle_info({hand, Pid, _Ip, _Port, Info}, State) ->
%% 	io:format("erbit_handChecker:checkInfo, handle_call -> Ip ~p Port ~p Info ~p~n",[Ip, Port, Info]),
	case lists:member(Info, dict:fetch_keys(State#state.infoHash)) of
		true ->
%% 			io:format("is a member ~n"),
			Pid ! {ok, dict:fetch(Info, State#state.infoHash)};
		false ->
			Pid ! {error, not_found}
	end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	io:format("erbit_handChecker:terminate/2 -> working ~p~n",[Reason]),
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

