-module(message_game2database).
-compile(export_all).

extract_guid_from_save_id(SaveId) ->
	[GuidStr | _Tail] = string:tokens(SaveId, "."),
	list_to_integer(GuidStr).

update_db_uint({Socket, _Context}, SaveId, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Value]),
	log:debug("update db uint", [save_id, SaveId, value, Value]),
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

update_db_int({Socket, _Context}, SaveId, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Value]),
	log:debug("update db int", [save_id, SaveId, value, Value]).

update_db_double({Socket, _Context}, SaveId, StrValue) ->
	Value = string:to_float(StrValue),
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Value]),
	log:debug("update db double", [save_id, SaveId, value, Value]).

update_db_string({Socket, _Context}, SaveId, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Value]),
	log:debug("update db string", [save_id, SaveId, value, Value]).

update_db_list_remove({Socket, _Context}, SaveId, Index) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [2, SaveId, Index]),
	log:debug("update db list remove", [save_id, SaveId, index, Index]).

update_db_list_update_int({Socket, _Context}, SaveId, Index, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Index, Value]),
	log:debug("update db list int", [save_id, SaveId, index, Index, value, Value]).

update_db_list_update_uint({Socket, _Context}, SaveId, Index, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Index, Value]),
	log:debug("update db list uint", [save_id, SaveId, index, Index, value, Value]).

update_db_list_update_double({Socket, _Context}, SaveId, Index, StrValue) ->
	Value = string:to_float(StrValue),
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Index, Value]),
	log:debug("update db list double", [save_id, SaveId, index, Index, value, Value]).

update_db_list_update_string({Socket, _Context}, SaveId, Index, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Index, Value]),
	log:debug("update db list string", [save_id, SaveId, index, Index, value, Value]).

update_db_list_clear({Socket, _Context}, SaveId) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [3, SaveId]),
	log:debug("update db list string", [save_id, SaveId]).

update_db_map_remove({Socket, _Context}, SaveId, Key) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [2, SaveId, Key]),
	log:debug("update db map remove", [save_id, SaveId, key, Key]).

update_db_map_update_int({Socket, _Context}, SaveId, Key, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Key, Value]),
	log:debug("update db map update int", [save_id, SaveId, key, Key, value, Value]).

update_db_map_update_uint({Socket, _Context}, SaveId, Key, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Key, Value]),
	log:debug("update db map update uint", [save_id, SaveId, key, Key, value, Value]).

update_db_map_update_string({Socket, _Context}, SaveId, Key, Value) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Key, Value]),
	log:debug("update db map update string", [save_id, SaveId, key, Key, value, Value]).

update_db_map_update_double({Socket, _Context}, SaveId, Key, StrValue) ->
	Value = string:to_float(StrValue),
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [1, SaveId, Key, Value]),
	log:debug("update db map update double", [save_id, SaveId, key, Key, value, Value]).

update_db_add_obj({Socket, _Context}, SaveId, SubFields) ->
	Guid = extract_guid_from_save_id(SaveId),
	player_save:save_update(Guid, [4, SaveId, SubFields]),
	log:debug("update db add object", [save_id, SaveId, SubFields]).

request_player({Socket, _Context}, Guid) ->
	{PackCall, _} = gen_protocb:gen_pack_and_cb_mod(database2game, message_database2game),
	Result = player_save:request_player(Guid),
	case Result of
		{ok, Data} ->
			PackCall(load_player, Socket, [Guid, 1, Data]);
		{error, Reason} ->
			PackCall(load_player, Socket, [Guid, 1, ""]),
			log:error("Request player failed", [guid, Guid, reason, Reason])
	end.

save_player({Socket, _Context}, guid, finished, Data) ->
	ok.

save_player_nocb({Socket, _Context}, guid, finished, Data) ->
	ok.

sent_all_player(Socket) ->
	ok.

create_player({Socket, _Context}, Guid, Name, Urs, Occ, Level, Cash, Title) ->
	ok.

find_player_info({Socket, _Context}, src_id, tgt_name) ->
	ok.

find_player_info4relation({Socket, _Context}, src_id, tgt_name, relation_code) ->
	ok.

get_player_guid_by_name({Socket, _Context}, req_id, player_name) ->
	ok.

reload_player({Socket, _Context}, Guid) ->
	ok.

force_serialize_to_file({Socket, _Context}, Guid) ->
	ok.

get_player_name_by_guid({Socket, _Context}, req_id, player_guid) ->
	ok.

save_player_end({Socket, _Context}, PlayerGuid) ->
	{PackCall, _} = gen_protocb:gen_pack_and_cb_mod(database2game, message_database2game),
	PackCall(confirm_save_player_end, Socket, [PlayerGuid]).

