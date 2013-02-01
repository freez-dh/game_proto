-module(log).
-compile(export_all).

format_log_args([]) ->
	[];
format_log_args([OnlyOne | []]) ->
	io:format("Args count is not right with ~p~n", [OnlyOne]),
	[];
format_log_args([Key, Value | Tail]) ->
	Pair = io_lib:format(",~p=~p", [Key, Value]),
	[Pair | format_log_args(Tail)].

log(Tag, Msg, Args, RedColor) ->
	{{Y,M,D}, {H,MI,S}} = erlang:localtime(),
	TimeStr = io_lib:format("[~w-~w-~w ~w:~w:~w]", [Y,M,D,H,MI,S]),
	Result = [TimeStr, Tag, Msg, format_log_args(Args)],
	case RedColor of
		true ->
			io:format("\e[1;31m~p\e[0m~n", [lists:flatten(Result)]);
		false ->
			io:format("~p~n", [lists:flatten(Result)])
	end.

debug(Msg, Args) ->
	log("[Debug]", Msg, Args, false).
debug(Msg) ->
	debug(Msg, []).

trace(Msg, Args) ->
	log("[Trace]", Msg, Args, false).
trace(Msg) ->
	trace(Msg, []).

error(Msg, Args) ->
	log("[Error]", Msg, Args, true).
error(Msg) ->
	error(Msg, []).

