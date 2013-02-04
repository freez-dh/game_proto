-module(message_game2database).
-compile(export_all).

extract_guid_from_save_id(SaveId) ->
	[GuidStr | _Tail] = string:tokens(SaveId, "."),
	list_to_integer(GuidStr).

update_db_uint(Socket, SaveId, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Value]),
	% log:debug("update db uint", [save_id, SaveId, value, Value]),
	Counter = case get(counter) of
		undefined -> 0;
		U -> U end,
	put(counter, Counter + 1),
	case Counter rem 1000 of
		0 ->
			io:format("TEsted ~p~n", [Counter]);
		_ ->
			ok
	end,
	ok.

