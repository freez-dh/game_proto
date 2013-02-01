-module(message_game2database).
-compile(export_all).

update_db_uint(Socket, SaveId, Value) ->
	io:format("update db uint ~p ~p~n", [SaveId, Value]),
	ok.
