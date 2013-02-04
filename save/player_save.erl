-module(player_save).
-compile(export_all).
-define(TEST_COUNT, 200000).

make_two(Int)  when Int < 10 ->
	[$0 | integer_to_list(Int)];
make_two(Int) ->
	integer_to_list(Int).

get_player_path(Guid) ->
	Dir = make_two(Guid rem 100),
	filename:join(["db", Dir, integer_to_list(Guid)]).

get_undone_op_path(Guid) ->
	Dir = make_two(Guid rem 100),
	filename:join(["db", Dir, integer_to_list(Guid) ++ "_undone_op"]).

save_player_handle(Guid, Data, State) ->
	DoFunc = fun() ->
		Path = get_player_path(Guid),
		filelib:ensure_dir(Path),
		case file:open(Path, write) of
			{ok, File} ->
				Result = file:write(File, Data);
			{error, Reason} ->
				log:error("Open file to persist failed", [path, Path, reason, Reason]),
				Result = {error, Reason}
		end,
		{Result, State}
	end,
	HasUndoneOp = dict:find(has_undone_op, State),
	case HasUndoneOp of
		error ->
			DoFunc();
		{ok, _} ->
			{{error, has_undone_op}, State}
	end.

handle_all_undone_op(_Guid) ->
	%UndoneFilePath = get_undone_op_path(Guid),
	%Path = get_player_path(Guid),
	%{ok, PlayerText} = file:read_file(Path),
	%{ok, PlayerData, _} = rfc4627:decode(PlayerText),
	ok.

request_player_handle(Guid, _, State) ->
	DoFunc = fun(RetState) ->
		Path = get_player_path(Guid),
		{file:read_file(Path), RetState}
	end,
	HasUndoneOp = dict:find(has_undone_op, State),
	case HasUndoneOp of
		error ->
			DoFunc(State);
		{ok, _} ->
			handle_all_undone_op(Guid),
			NewState = dict:erase(has_undone_op, State),
			DoFunc(NewState)
	end.

do_realsave(_Guid, State) ->
	UndoneList = dict:fetch(undone_list, State),
	UndoneFile = dict:fetch(undone_file, State),
	JoinedStr = string:join(lists:reverse(UndoneList), "\n"),
	file:write(UndoneFile, JoinedStr),
	timer:send_after(500, self(), {do_realsave}),
	dict:store(undone_list, [], State).

save_update_handle(_Guid, Args, State) ->
	[Data | []] = io_lib:format("~p", [Args]),
	NewState = dict:update(undone_list, fun(List) -> [Data | List] end, State),
	dict:store(has_undone_op, true, NewState).

player_handle_loop(Guid, State) ->
	receive
		{call, Func, FromPid, CallId, Guid, Args} ->
			FuncName = list_to_atom(atom_to_list(Func) ++ "_handle"),
			{Result, NewState} = player_save:FuncName(Guid, Args, State),
			FromPid ! {result, CallId, Result};
		{call_noresult, Func, _FromPid, Guid, Args} ->
			FuncName = list_to_atom(atom_to_list(Func) ++ "_handle"),
			NewState = player_save:FuncName(Guid, Args, State);
		{do_realsave} ->
			NewState = do_realsave(Guid, State)
	end,
	player_handle_loop(Guid, NewState).

guid_save_server(Guid2Pid, Pid2Guid) ->
	receive
		{get_pid, From, Guid} ->
			case dict:find(Guid, Guid2Pid) of
				{ok, Pid} ->
					From ! {send_pid, Guid, Pid},
					guid_save_server(Guid2Pid, Pid2Guid);
				error -> 
					NewPid = spawn_link(fun() -> 
								State = dict:new(),
								UndoneFilePath = get_undone_op_path(Guid),
								filelib:ensure_dir(UndoneFilePath),
								{ok, UndoneFile} = file:open(UndoneFilePath, [append]),
								NewState = dict:store(undone_list, [], State),
								timer:send_after(500, self(), {do_realsave}),
								player_handle_loop(Guid, dict:store(undone_file, UndoneFile, NewState)) end),
					Guid2PidDict = dict:store(Guid, NewPid, Guid2Pid),
					Pid2GuidDict = dict:store(NewPid, Guid, Pid2Guid),
					From ! {send_pid, Guid, NewPid},
					guid_save_server(Guid2PidDict, Pid2GuidDict)
			end;
		{'EXIT', Pid, Why} ->
			Guid = dict:fetch(Pid, Pid2Guid),
			Guid2PidDict = dict:erase(Guid, Guid2Pid),
			Pid2GuidDict = dict:erase(Pid, Pid2Guid),
			log:error("Player serve process exit", [guid, Guid, pid, Pid, why, Why]),
			guid_save_server(Guid2PidDict, Pid2GuidDict)
	end.

%% ===============rpc===========================
async_rpc_with_noresult(Guid, FunName, Args) ->
	case get(Guid) of
		undefined ->
			guid_save_server ! {get_pid, self(), Guid},
			receive
				{send_pid, Guid, Pid} ->
					put(Guid, Pid),
					Pid ! {call_noresult, FunName, self(), Guid, Args}
			end;
		Pid ->
			Pid ! {call_noresult, FunName, self(), Guid, Args}
	end,
	ok.

sync_rpc(Guid, FunName, Args) ->
	guid_save_server ! {get_pid, self(), Guid},
	receive
		{send_pid, Guid, Pid} ->
			Id = make_ref(),
			Pid ! {call, FunName, self(), Id, Guid, Args},
			receive
				{result, Id, Result} ->
					Result
			end
	end.

%% ===============rpc interface===========================
save_player(Guid, Data) ->
	sync_rpc(Guid, save_player, Data).

request_player(Guid) ->
	sync_rpc(Guid, request_player, []).

save_update(Guid, Args) ->
	async_rpc_with_noresult(Guid, save_update, Args).

start_guid_save_server() ->
	ServerPid = spawn(fun() ->
				Guid2Pid = dict:new(),
				Pid2Guid = dict:new(),
				process_flag(trap_exit, true),
				guid_save_server(Guid2Pid, Pid2Guid) end),
	register(guid_save_server, ServerPid).

test_save_update() ->
	start_guid_save_server(),
	Seqs = lists:seq(1, ?TEST_COUNT),
	statistics(runtime),
	statistics(wall_clock),
	lists:foreach(fun(_) ->
				save_update(41012, [1, ["41012.component_attribute.items.page.0.endure", 100]]) end, Seqs
		),
	{_, Runtime} = statistics(runtime),
	{_, Walltime} = statistics(wall_clock),
	io:format("Runtime:~p,Walltime:~p~n", [Runtime/1000, Walltime/1000]).

test_a() ->
	State = dict:new(),
	Guid = 41012,
	UndoneFilePath = get_undone_op_path(Guid),
	filelib:ensure_dir(UndoneFilePath),
	{ok, UndoneFile} = file:open(UndoneFilePath, [append]),
	NewState = dict:store(undone_file, UndoneFile, State),
	Seqs = lists:seq(1, ?TEST_COUNT),
	statistics(runtime),
	statistics(wall_clock),
	MyPid = self(),
	spawn(fun() ->
		lists:foreach(fun(_) ->
					save_update_handle(Guid, [1, ["41012.component_attribute.items.page.0.endure", 100]], NewState) end, Seqs
			),
		MyPid ! done
		end),
	receive
		done -> ok
	end,
	{_, Runtime} = statistics(runtime),
	{_, Walltime} = statistics(wall_clock),
	io:format("Runtime:~p,Walltime:~p~n", [Runtime/1000, Walltime/1000]).

