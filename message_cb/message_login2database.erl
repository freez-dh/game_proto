-module(message_login2database).
-compile(export_all).
-define(PART_DICT, login_part_dict).

get_part_dict() ->
	R = get(?PART_DICT),
	case R of
		undefined ->
			NewR = dict:new(),
			put(?PART_DICT, NewR),
			NewR;
		_ ->
			R
	end.

save_player(Guid, Finished, Data) ->
	PartDict = get_part_dict(),
	UpdateDict = dict:update(Guid, fun(DataList) ->
				[Data | DataList] end, [Data], PartDict),
	case Finished of
		1 ->
			AllList = dict:fetch(Guid, UpdateDict),
			AllData = lists:flatten(lists:reverse(AllList)),
			EraseDict = dict:erase(Guid, UpdateDict),
			put(?PART_DICT, EraseDict),
			player_save:save_player(Guid, AllData);
		0 ->
			put(?PART_DICT, UpdateDict)
	end.

request_player(Guid) ->
	Result = player_save:request_player(Guid),
	case Result of
		{ok, Data} ->
			ok;
		{error, Reason} ->
			log:error("Request player failed", [guid, Guid, reason, Reason])
	end.

del_player_info(GUid) ->
	ok.
