%% author Denir Leric

-module(test_gui).
-compile(export_all).
-import_all(erbit_guiConnector).

start() ->
    erbit_guiConnector:showname(0, "Torrent1"), 
    erbit_guiConnector:showip(0, [{ip1,port1},{ip2,port2},{ip3,port3}]),
    erbit_guiConnector:showdownspeed(0, 237.94),
    erbit_guiConnector:showupspeed(0, 37.00),
    erbit_guiConnector:showstatus(0, 98.40),
    erbit_guiConnector:showsize(0, 512.00).
