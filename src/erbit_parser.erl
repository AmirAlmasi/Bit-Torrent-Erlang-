%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Amir Almasi <amir.fireflame@gmail.com>
%% @module erbit_parser.erl
%% @reference <a href="http://wiki.theory.org/Decoding_encoding_bencoded_data_with_erlang">
%% Bittorrent specification implementation</a>
%% @doc <html>The purpose of the module is to read and parse .torrent file containing 
%% information <br/> To see more detail about the module and its functions, take 
%% a look at bittorrent specification website. </html>
%% @version 0.1.0.3
%% @date Created: Sep 28, 2011
%% @date Edited: Sep 29, 2011 - implement all main functions
%% @date Edited: Sep 30, 2011 - implement some extra functions. For example: start and processData
%% @date Edited: Oct 02, 2011 - cleaning the code and checking comments.
%% @date Edited: Oct 23, 2011 - rewrite all the code again for understanding binaries and being satisfactory
%% @date Edited: Nov 7, 2011 - change the way to get a path and open a torrent file and match it with tracker response

-module(erbit_parser).

%%
%% Exported Functions
%%
-export([bencodeData/1]).

%%
%% API Functions
%%


%% @doc <html>A function to read the data from torrent file when it is called </html>
%% @spec bencodeData(binary()) -> tuple() | error
bencodeData(Data)->
%% 	io:format("erbit_parser:bencodeData-> here is running"),
	try parseData(Data) of 
		{Value, _} ->
%% 			io:format("erbit_parser:bencodeData/1 -> working ~n"),
			{ok, Value}
	catch 
		exit:Reason ->
			io:format("erbit_parser:bencodeData ->problem(~p) processing data ~n",[Reason]),
			{error, unparsed};
		Error:Reason2 ->
			io:format("erbit_parser:bencodeData ->problem(~p ~p) processing data ~n",[Error, Reason2]),
			{error, Error , Reason2}
	end.

%%
%% Local Functions
%%

%% @doc <html> A function to get binary data, then process it based on 
%% the information contained and decode it </html>
%% @spec parseData(byte()) -> tuple()
parseData(<<$d, Tail/binary>>) ->
	parseDictionary(Tail, dict:new());
parseData(<<$l, Tail/binary>>) ->
	parseList(Tail,[]);
parseData(<<$i, Tail/binary>>)->
	parseInteger(Tail, []);
parseData(_String)->
	parseString(_String, []).

%% @doc <html> A function to be applied for decoding binaries contaning a dictionary</html>
%% @spec parseDictionary(byte(), list()) -> tuple()
parseDictionary(<<$e, Tail/binary>>, Acc)->
	{Acc, Tail};
parseDictionary(Data, Acc)->
	{Key , _Tail} = parseData(Data),
	{Value, Tail} = parseData(_Tail),
	parseDictionary(Tail, dict:store(Key, Value, Acc)).

%% @doc <html> A function to be applied for decoding binaries contaning a list</html>
%% @spec parseList(byte(), list()) -> tuple()
parseList(<<$e, Tail/binary>>, Acc)->
	{lists:reverse(Acc), Tail};
parseList(Data, Acc) ->
	{Value, Tail}= parseData(Data),
	parseList(Tail, [Value|Acc]).	

%% @doc <html> A function to be applied for decoding binaries contaning an integer number</html>
%% @spec parseInteger(byte(), list()) -> tuple()
parseInteger(<<$e, Tail/binary>>, Acc)->
	{list_to_integer(lists:reverse(Acc)), Tail};
parseInteger(<<X, Tail/binary>>, Acc)->
	parseInteger(Tail, [X|Acc]).

%% @doc <html>A function to be applied for decoding binaries contaning string </html>
%% @spec parseString(byte(), list()) -> tuple()
parseString(<<$:, Tail/binary>>, Acc)->
	CharacterNumber = list_to_integer(lists:reverse(Acc)),
	<<String:CharacterNumber/binary, _Tail/binary>> = Tail,
	{String , _Tail};
parseString(<<X, Tail/binary>>, Acc)->
	parseString(Tail, [X|Acc]).


%% test function 

%% start() ->
%% 	case file:read_file("C://Users//Emertat//Desktop//book.torrent//test//xubuntu-8.04.1-desktop-i386.iso.torrent") of
%% 		{ok, Data} -> 
%% 			io:format("bencoder module, start function:File has been read properly ~n", []),
%% 			bencodeData(Data);
%% 		Unexpected ->
%% 			io:format("read module, start function:There was an unexpected error in reading the file ~p ~n"
%% 					  , [Unexpected]),
%% 			error			
%% 	end.