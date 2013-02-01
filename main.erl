-module(main).
-compile(export_all).

set_path() ->
	true = code:add_patha("config"),
	true = code:add_patha("proto"),
	true = code:add_patha("message_pack"),
	true = code:add_patha("message_cb"),
	true = code:add_patha("server").

start_game2database_server() ->
	set_path(),
	proto_server:start_server(config:get_database_game_port(),
		game2database, message_game2database, []).

test_start() ->
	start_game2database_server(),
	Socket = proto_server:connect_to_server(),
	{PackCall, _} = gen_protocb:gen_pack_and_cb_mod(game2database, message_game2database),
	Seqs = lists:seq(1, 1000000),
	statistics(runtime),
	statistics(wall_clock),
	lists:foreach(fun(_) -> PackCall(update_db_uint, Socket, ["123", 100]) end, Seqs),
	{_, Time} = statistics(runtime),
	{_, WallTime} = statistics(wall_clock),
	io:format("Cost:~p, wall time:~p~n", [Time/1000, WallTime/1000]).

start_database_server() ->
	set_path(),
	proto_server:start_server(config:get_database_game_port(),
		game2database, message_game2database, []),
	io:format("Database start listen game~n", []),
	proto_server:start_server(config:get_database_login_port(),
		login2database, message_login2database, []),
	io:format("Database start listen login~n", []),
	proto_server:start_server(config:get_database_master_port(),
		master2database, message_master2database, []),
	io:format("Database start listen master~n", []),
	ok.

