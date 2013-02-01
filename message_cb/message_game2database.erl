-module(message_game2database).
-compile(export_all).

update_db_uint(Socket, SaveId, Value) ->
	log:debug("update db uint", [save_id, SaveId, value, Value]),
	ok.
