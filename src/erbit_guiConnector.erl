%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @author Denir <denir.leric@gmail.com>
%% @module erbit_guiConnector.erl
%% @doc <html>The purpose of the module is to update jinterface mail box in java part </html>
%% @version 0.1

-module(erbit_guiConnector).
-compile(export_all).

%% @doc <html> A function to be called at first when a torrent file is chosen on GUI </html>
%% @spec start() -> {ok, pid()}
start() ->
    init().
%% a function to start the process
init() ->
    register(?MODULE, spawn(erbit_guiConnector, loop, [self()])).

loop(JavaPid) ->
    receive
	{init, TorrentfilePath, DirectoryPath, NumWant, GlobalNumWant, Port, Pid} ->
		erbit_core:readFile(TorrentfilePath, {DirectoryPath, NumWant, GlobalNumWant, Port}),
	 loop(Pid);
	{start, _Identifier} ->
	    loop(JavaPid);
	{pause, _Identifier} ->
	    loop(JavaPid);
	{stop, _Identifier} ->
	    loop(JavaPid);
	{remove, _Identifier} ->
	    loop(JavaPid);
	{name, Identifier, Name} ->
	    JavaPid ! {name, Identifier, Name},
	    loop(JavaPid);
	{ips, Identifier, List} -> 
	    JavaPid ! {ips, Identifier, List},
	    loop(JavaPid);
	{size, Identifier, Size} ->
	    JavaPid ! {size, Identifier, Size},
	    loop(JavaPid);
	{seeders, Identifier, Seeders} ->
	    JavaPid ! {seeders, Identifier, Seeders},
	    loop(JavaPid);
	{leechers, Identifier, Leechers} ->
	    JavaPid ! {leechers, Identifier, Leechers},
	    loop(JavaPid);
	{downloaded, Identifier, DSize} ->
	    JavaPid ! {downloaded, Identifier, DSize},
	    loop(JavaPid);
	{uploaded, Identifier, UPSize} ->
	    JavaPid ! {uploaded, Identifier, UPSize},
	    loop(JavaPid);
	{trackers, Identifier, Trackers} ->
	    JavaPid ! {trackers, Identifier, Trackers},
	    loop(JavaPid);
	{files, Identifier, Files} ->
	    JavaPid ! {files, Identifier, Files},
	    loop(JavaPid);
	{exit, Reason} ->
		erbit_core:exit(Reason),
	    loop(JavaPid);
	{done, Identifier} ->
	    JavaPid ! {done, Identifier},
	    loop(JavaPid)
    end.

%% @doc <html> A function to set the name of the file on java part </html>
%% @spec showname(integer(), string()) -> {name, Identifier, Name}
showname(Identifier, Name) ->
    erbit_guiConnector ! {name, Identifier, Name}.

%% @doc <html> A function to show the peer list from the trackers on the GUI </html>
%% @spec showip(integer(), list()) -> {ips, Identifier, List}
showip(Identifier, List) ->
    erbit_guiConnector ! {ips, Identifier, List}.

%% @doc <html> A function to show the file size on the GUI </html>
%% @spec showsize(integer(), integer()) -> {size, Identifier, Size}
showsize(Identifier, Size) ->
    erbit_guiConnector ! {size, Identifier, Size}.

%% @doc <html> A function to show how many seeders there are </html>
%% @spec showseeders(integer(), integer()) -> {seeders, Identifier, Seeders}
showseeders(Identifier, Seeders) ->
    erbit_guiConnector ! {seeders, Identifier, Seeders}.

%% @doc <html> A function to show how many leechers there are </html>
%% @spec showleechers(integer(), integer()) -> {leechers, Identifier, Leechers}
showleechers(Identifier, Leechers) ->
    erbit_guiConnector ! {leechers, Identifier, Leechers}.

%% @doc <html> A function to show how much has been downloaded </html>
%% @spec showdownloaded(integer(), integer()) -> {downloaded, Identifier, DSize}
showdownloaded(Identifier, DSize) ->
    erbit_guiConnector ! {downloaded, Identifier, DSize}.

%% @doc <html> A function to show how much has been uploaded </html>
%% @spec showuploaded(integer(), integer()) -> {uploaded, Identifier, UPSize}
showuploaded(Identifier, UPSize) ->
    erbit_guiConnector ! {uploaded, Identifier, UPSize}.

%% @doc <html> A function to list all the trackers on the GUI </html>
%% @spec showtrackers(integer(), list()) -> {trackers, Identifier, Trackers}
showtrackers(Identifier, Trackers) ->
    erbit_guiConnector ! {trackers, Identifier, Trackers}.

%% @doc <html> A function to list all the files on the GUI </html>
%% @spec showfiles(integer(), string()) -> {files, Identifier, Files}
showfiles(Identifier, Files) ->
    erbit_guiConnector ! {files, Identifier, Files}.

%% @doc <html> A function to tell java that a torrent file is done </html>
%% @spec doneTorrent(integer()) -> {done, Identifier}
doneTorrent(Identifier) ->
    erbit_guiConnector ! {done, Identifier}.
