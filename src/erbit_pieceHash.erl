%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_pieceHash.erl
%% @doc <html>The purpose of the module is to get all concatenated picese's hashes
%% and separate 20bytes for each piece </html>
%% @version 0.1.0.1
%% @date Created: Oct 23, 2011

-module(erbit_pieceHash).

%%
%% Exported Functions
%%
-export([start/0, start/1]).

%%
%% API Functions
%%

%% @doc <html>A function to test the module independently </html>
%% @spec start()-> list()
start() ->
	getHashes(dict:fetch(<<"pieces">>, dict:fetch(<<"info">>, erbit_parser:start()))).

%% @doc <html>A function to be used by dataManager module </html>
%% @spec start(binary())-> list()
start(Data) ->
%% 	io:format("erbit_pieceHash:start -> was called"),
	getHashes(dict:fetch(<<"pieces">>, dict:fetch(<<"info">>, Data))).


%%
%% Local Functions
%%

%% @doc <html> A function to get the binary and try to separate them </html>
%% @spec getHashes(binary())-> list()
getHashes(Binary) ->
	seperateHash(Binary, 0, dict:new()).

%% @doc <html> A function to get the binary data and map them with the corresponding number</html>
%% @spec seperateHash(binary(), integer(), list())-> list()
seperateHash(<<>>, _, List) ->
	List;
seperateHash(<<Data:160, Tail/binary>>, Number, List) ->
	%% 	io:format("piece number ~p is ~p~n",[Number , Data]),
	seperateHash(Tail, Number+1, dict:store(Number, Data, List)).
