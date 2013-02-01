-module(main).
-compile(export_all).
-define(PORT, 30000).

set_path() ->
	true = code:add_patha("proto"),
	true = code:add_patha("message_pack"),
	true = code:add_patha("message_cb"),
	true = code:add_patha("server").

start_game2database_server() ->
	set_path(),
	proto_server:start_server(?PORT, game2database, message_game2database, []).

test_start() ->
	start_game2database_server(),
	Socket = proto_server:connect_to_server(),
	{PackCall, _} = gen_protocb:gen_pack_and_cb_mod(game2database, message_game2database),
	Seqs = lists:seq(1, 1000000),
	statistics(runtime),
	lists:foreach(fun(_) -> PackCall(update_db_uint, Socket, ["123", 100]) end, Seqs),
	{_, Time} = statistics(runtime),
	io:format("Cost:~p~n", [Time/1000]).


