%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_hashMaker.erl
%% @doc <html> a module to make the hash based on the information in torrent file -info part-</html>

-module(erbit_hashMaker).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([readFile/0, getHash/1]).

%%
%% API Functions
%%

%% @doc <html>A function to test the module independently </html>
%% @spec readFile()-> string()
readFile() ->
	{ok, Data} = file:read_file("C://Users//Emertat//Desktop//book.torrent//test//xubuntu-8.04.1-desktop-i386.iso.torrent"),
	makeHash(readInfo(Data)).

%% @doc <html> A function to get the file binary and return hash of the file
%% the function is an api for testing of time being</html>
%% @spec getHash(binary())-> string()
getHash(Data) ->
%% 	io:format("erbit_hashMaker:getHash -> working ~n"),
	makeHash(readInfo(Data)).

%%
%% Local Functions
%%

%% @doc <html>A function to read the file in binary way </html>
%% @ spec readInfo(binary())-> binary()
readInfo(<<$4,$:,$i,$n,$f,$o, Tail/binary>>) ->
	binary:part(Tail, {0,byte_size(Tail)-1});
readInfo(<<_:8 , Tail/binary>>) ->
	readInfo(Tail).

%% @doc <html>A function to get binary and return the sha1 of the binary</html>
%% @ spec makeHash(binary())-> binary()
makeHash(Binary)->
%% 	io:format("Test one: ~p ~n", [[ X || <<X,Y>> <= <<1,2,3,4,5>>, X rem 2 == 0]]),
%% 	crypto:sha(Binary).
	Hash = crypto:sha(Binary),
	{Hash, makeUrl(binary_to_list(Hash))}.

%% @doc <html>A function to get hash and make the url based bittorrent 
%% specification </html>
%% @spec makeUrl(binary())-> list()
makeUrl(HashBinary) ->
	lists:flatten([ "%" | string:join([string:right(Part, 2, $0) || Part <- [erlang:integer_to_list(Part, 16)
																			|| Part <- HashBinary]], "%")]).