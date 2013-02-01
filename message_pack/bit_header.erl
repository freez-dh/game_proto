-module(bit_header).
-compile(export_all).
-define(PACK_TABLE_DICT, pack_table_dict).
-define(UNPACK_TABLE_DICT, unpack_table_dict).

new_pack_int_s(0) ->
	[16#00];
new_pack_int_s(N) when is_integer(N) ->
	ThisPart = 16#7f band N,
	RemainPart = N bsr 7,
	case RemainPart of 
		0 -> [ThisPart];
		_ -> [ThisPart bor 16#80| new_pack_int_s(RemainPart)]
	end.

new_pack_int(N) when is_integer(N) ->
	PInt = new_pack_int_s(N),
	list_to_binary(PInt).

new_unpack_int([Value | []], N) ->
	Value bsl (N * 7);

new_unpack_int([Value | Remain], N) ->
	((Value band 16#7f) bsl (N * 7)) bor new_unpack_int(Remain, N + 1).

new_unpack_int(BinValue) ->
	new_unpack_int(binary:bin_to_list(BinValue), 0).

test_N(N) ->
	PackedValue = new_pack_int(N),
	Result = new_unpack_int(PackedValue),
	Result == N.

test_new_pack_unpack_int(From, From) ->
	case test_N(From) of
			true -> ok;
			false -> throw(From)
	end;

test_new_pack_unpack_int(From, End) ->
	case test_N(From) of
			true -> ok;
			false -> throw(From)
	end,
	test_new_pack_unpack_int(From + 1, End).

test_new_pack_unpack() ->
	test_new_pack_unpack_int(0, 1000000).

test_all_data(_, <<>>) ->
	ok;
test_all_data(N, Datas) ->
	<<Size, IncludeData/bitstring>> = Datas,
	{PackedBytes, Remain} = split_binary(IncludeData, Size),
	MyPackedValue = new_pack_int(N),
	case MyPackedValue == PackedBytes of
		true -> test_all_data(N + 1, Remain);
		false -> throw(N)
	end.

% int8
pack_int8(Result, IntValue) when is_integer(IntValue) ->
	<<Result/binary, IntValue>>.
unpack_int8(BinValue) ->
	<<Result/signed, Remain/binary>> = BinValue,
	{Remain, Result}.
unpack_uint8(BinValue) ->
	<<Result, Remain/binary>> = BinValue,
	{Remain, Result}.

% int16
pack_int16(Result, IntValue) when is_integer(IntValue) ->
	<<Result/binary, IntValue:16/little>>.
unpack_int16(BinValue) ->
	<<Result:16/little-signed, Remain/binary>> = BinValue,
	{Remain, Result}.
unpack_uint16(BinValue) ->
	<<Result:16/little, Remain/binary>> = BinValue,
	{Remain, Result}.

% int32
pack_int32(Result, IntValue) when is_integer(IntValue) ->
	<<Result/binary, IntValue:32/little>>.
unpack_int32(BinValue) ->
	<<Result:32/little-signed, Remain/binary>> = BinValue,
	{Remain, Result}.
unpack_uint32(BinValue) ->
	<<Result:32/little, Remain/binary>> = BinValue,
	{Remain, Result}.

% int64
pack_int64(Result, IntValue) when is_integer(IntValue) ->
	<<Result/binary, IntValue:64/little>>.
unpack_int64(BinValue) ->
	<<Result:64/little-signed, Remain/binary>> = BinValue,
	{Remain, Result}.
unpack_uint64(BinValue) ->
	<<Result:64/little, Remain/binary>> = BinValue,
	{Remain, Result}.

% float
pack_float(Result, FloatValue) when is_float(FloatValue) or is_integer(FloatValue) ->
	<<Result/binary, FloatValue:32/little-float>>.
unpack_float(BinValue) ->
	<<Result:32/little-float, Remain/binary>> = BinValue,
	{Remain, Result}.

% varint32
get_varint32_packed_size(Cur, BinValue) ->
	ByteValue = binary:at(BinValue, Cur),
	case ByteValue band 16#80 of
		0 -> Cur + 1;
		_ -> get_varint32_packed_size(Cur + 1, BinValue)
	end.
pack_varint32(Result, IntValue) when is_integer(IntValue) ->
	PackedValue = new_pack_int(IntValue),
	<<Result/binary, PackedValue/binary>>.
unpack_varint32(BinValue) ->
	PackedSize = get_varint32_packed_size(0, BinValue),
	{PackedValue, Remain} = split_binary(BinValue, PackedSize),
	Result = new_unpack_int(PackedValue),
	{Remain, Result}.

% protoid
pack_protoid(Result, ProtoId) when ProtoId < 256 ->
	pack_int8(Result, ProtoId);
pack_protoid(Result, ProtoId) ->
	PackedZero = pack_int8(Result, 0),
	pack_int16(PackedZero, ProtoId).
unpack_protoid(Result) ->
	case binary:at(Result, 0) of
		0 -> 
			{_, SplitResult} = split_binary(Result, 1),
			unpack_uint16(SplitResult);
		_ ->
			unpack_uint8(Result)
	end.

% string
pack_string(Result, List) ->
	PackedHeaderValue = pack_varint32(Result, length(List) + 1),
	ListPackedValue = list_to_binary(List),
	<<PackedHeaderValue/binary, ListPackedValue/binary, 0>>.
unpack_string(BinValue) ->
	{Remain, StringLength} = unpack_varint32(BinValue),
	{StringPacked, RRemain} = split_binary(Remain, StringLength),
	ResultList = binary:bin_to_list(StringPacked),
	Result = lists:sublist(ResultList, 1, length(ResultList) - 1),
	{RRemain, Result}.

%% ======================some tests==========================
test_pack_from_file(FileName) ->
	{ok, AllData} = file:read_file(FileName),
	test_all_data(0, AllData).

gen_all_dict_pack_unpack_func() ->
	Protos = table:get_protos(),
	put(?PACK_TABLE_DICT, dict:new()),
	put(?UNPACK_TABLE_DICT, dict:new()),
	gen_each_dict_pack_funs(Protos),
	gen_each_dict_unpack_funs(Protos).

%% ----------------------------pack related------------------------
gen_dict_pack_fun_by_type(Type) ->
	TableDict = get(?PACK_TABLE_DICT),
	StringType = atom_to_list(Type),
	Result = dict:fetch(StringType, TableDict),
	Result.
gen_pack_fun_by_type(Type) ->
	case Type of
		int8 -> fun pack_int8/2;
		uint8 -> fun pack_int8/2;
		int16 -> fun pack_int16/2;
		uint16 -> fun pack_int16/2;
		int32 -> fun pack_int32/2;
		uint32 -> fun pack_int32/2;
		uint64 -> fun pack_int64/2;
		string -> fun pack_string/2;
		bytes -> fun pack_string/2;
		float -> fun pack_float/2;
		vector2 -> fun(Buffer, Value) ->
					{X, Z} = Value,
					NewBuffer = pack_float(Buffer, X),
					pack_float(NewBuffer, Z)
				end;
		vector3 -> fun(Buffer, Value) ->
					{X, Y, Z} = Value,
					PackX = pack_float(Buffer, X),
					PackY = pack_float(PackX, Y),
					pack_float(PackY, Z)
				end;
		_ -> gen_dict_pack_fun_by_type(Type)
	end.

pack_list([Value | []], Buffer, PackElementFunc) ->
	PackElementFunc(Buffer, Value);
pack_list([Value | Remain], Buffer, PackElementFunc) ->
	NewBuffer = PackElementFunc(Buffer, Value),
	pack_list(Remain, NewBuffer, PackElementFunc).
gen_pack_list_fun_by_type(Type) ->
	PackElementFunc = gen_pack_fun_by_type(Type),
	fun(Buffer, Value) -> 
		ListLength = length(Value),
		PackedLengthBuffer = pack_varint32(Buffer, ListLength),
		pack_list(Value, PackedLengthBuffer, PackElementFunc)
	end.

gen_pack_fun_by_args(ArgsDesc) ->
	case size(ArgsDesc) of
		2 ->
			{_, Type} = ArgsDesc,
			gen_pack_fun_by_type(Type);
		3 ->
			{_, Type, _} = ArgsDesc,
			gen_pack_list_fun_by_type(Type)
	end.
gen_args_pack_func([]) ->
	[];
gen_args_pack_func([ArgsDesc | Tail]) ->
	[gen_pack_fun_by_args(ArgsDesc)| gen_args_pack_func(Tail)].
gen_each_dict_pack_funs([]) ->
	ok;
gen_each_dict_pack_funs([Proto | Tail]) ->
	{ProtoName, ArgsDesc} = Proto,
	PackFuns = gen_args_pack_func(ArgsDesc),
	Names = lists:map(fun(Desc) ->
				case size(Desc) of
					2 -> {Name, _} = Desc;
				    3 -> {Name, _, _}	= Desc
				end,
				Name end, ArgsDesc),
	PackFunsWithName = lists:zip(Names, PackFuns),
	ResultF = fun(Buffer, DictValue) -> pack_dicts(PackFunsWithName, Buffer, DictValue) end,
	PackFuncsDict = get(?PACK_TABLE_DICT),
	NewDict = dict:store(ProtoName, ResultF, PackFuncsDict),
	put(?PACK_TABLE_DICT, NewDict),
	gen_each_dict_pack_funs(Tail).

pack_dicts([], Buffer, _Dict) ->
	Buffer;
pack_dicts([PackFunWithName | Tail], Buffer, Dict) ->
	{Name, PackFun} = PackFunWithName,
	Value = dict:fetch(Name, Dict),
	PackedBuffer = PackFun(Buffer, Value),
	pack_dicts(Tail, PackedBuffer, Dict).

pack_with_funcs([], [], _Buffer) ->
	_Buffer;
pack_with_funcs([PackFunc | FuncRemain], [Value | ValueRemain], Buffer) ->
	NewBuffer = PackFunc(Buffer, Value),
   	pack_with_funcs(FuncRemain, ValueRemain, NewBuffer).

gen_proto_pack_func(ProtoDesc) ->
	{_, ProtoId, ArgsDesc} = ProtoDesc,
	ArgsPackFuncs = gen_args_pack_func(ArgsDesc),
	fun(Values) -> 
			PackedBuffer = pack_protoid(<<>>, ProtoId),
			WithoutHeader = pack_with_funcs(ArgsPackFuncs, Values, PackedBuffer),
			Header = pack_varint32(<<>>, size(WithoutHeader)),
			<<Header/binary, WithoutHeader/binary>>
	end.
%% ----------------------------pack related------------------------

%% --------------------------unpack related----------------------
gen_dict_unpack_fun_by_type(Type) ->
	TableDict = get(?UNPACK_TABLE_DICT),
	StringType = atom_to_list(Type),
	Result = dict:fetch(StringType, TableDict),
	Result.
gen_unpack_fun_by_type(Type) ->
	case Type of
		int8 -> fun unpack_int8/1;
		uint8 -> fun unpack_uint8/1;
		int16 -> fun unpack_int16/1;
		uint16 -> fun unpack_uint16/1;
		int32 -> fun unpack_int32/1;
		uint32 -> fun unpack_uint32/1;
		uint64 -> fun unpack_uint64/1;
		string -> fun unpack_string/1;
		bytes -> fun unpack_string/1;
		float -> fun unpack_float/1;
		vector2 -> fun(Buffer) ->
					{UnpackX, X} = unpack_float(Buffer),
					{UnpackZ, Z} = unpack_float(UnpackX),
					{UnpackZ, {X, Z}}
			end;
		vector3 -> fun(Buffer) ->
					{UnpackX, X} = unpack_float(Buffer),
					{UnpackY, Y} = unpack_float(UnpackX),
					{UnpackZ, Z} = unpack_float(UnpackY),
					{UnpackZ, {X, Y, Z}}
			end;
		_ -> gen_dict_unpack_fun_by_type(Type)
	end.
unpack_list(N, N, Buffer, UnpackElementFunc) ->
	{RemainBuffer, Value} = UnpackElementFunc(Buffer),
	{RemainBuffer, [Value]};
unpack_list(M, N, Buffer, UnpackElementFunc) ->
	{RemainBuffer, Value} = UnpackElementFunc(Buffer),
	{SubRemainBuffer, ValueList} = unpack_list(M + 1, N, RemainBuffer, UnpackElementFunc),
	{SubRemainBuffer, [Value | ValueList]}.
gen_unpack_list_fun_by_type(Type) ->
	UnpackElementFunc = gen_unpack_fun_by_type(Type),
	fun(Buffer) -> 
		{RemainBuffer, ListLength} = unpack_varint32(Buffer),
		unpack_list(1, ListLength, RemainBuffer, UnpackElementFunc)
	end.
gen_unpack_fun_by_args(ArgDesc) ->
	case size(ArgDesc) of
		2 ->
			{_, Type} = ArgDesc,
			gen_unpack_fun_by_type(Type);
		3 ->
			{_, Type, _} = ArgDesc,
			gen_unpack_list_fun_by_type(Type)
	end.
gen_args_unpack_func([]) ->
	[];
gen_args_unpack_func([ArgDesc | Tail]) ->
	[gen_unpack_fun_by_args(ArgDesc) | gen_args_unpack_func(Tail)].
gen_each_dict_unpack_funs([]) ->
	ok;
gen_each_dict_unpack_funs([Proto | Tail]) ->
	{ProtoName, ArgsDesc} = Proto,
	UnpackFuns = gen_args_unpack_func(ArgsDesc),
	Names = lists:map(fun(Desc) -> 
				case size(Desc) of
					2 -> {Name, _} = Desc;
					3 -> {Name, _, _}	= Desc
				end,
				Name end, ArgsDesc),

	UnpackFunsWithName = lists:zip(Names, UnpackFuns),
	ResultF = fun(Buffer) -> unpack_dicts(UnpackFunsWithName, Buffer, dict:new()) end,
	UnpackFuncsDict = get(?UNPACK_TABLE_DICT),
	NewDict = dict:store(ProtoName, ResultF, UnpackFuncsDict),
	put(?UNPACK_TABLE_DICT, NewDict),
	gen_each_dict_unpack_funs(Tail).

unpack_dicts([], Buffer, Dict) ->
	{Buffer, Dict};
unpack_dicts([UnpackFunWithName | Tail], Buffer, Dict) ->
	{Name, UnpackFun} = UnpackFunWithName,
	{RemainBuffer, Result} = UnpackFun(Buffer),
	NewDict = dict:store(Name, Result, Dict),
	unpack_dicts(Tail, RemainBuffer, NewDict).

unpack_with_funcs([], _Buffer) ->
	[];
unpack_with_funcs([UnpackFunc | FuncRemain], Buffer) ->
	{RemainBuffer, Value} = UnpackFunc(Buffer),
	[Value | unpack_with_funcs(FuncRemain, RemainBuffer)].

gen_proto_unpack_func(ProtoDesc) ->
	{_, ProtoId, ArgsDesc} = ProtoDesc,
	ArgsUnpackFuncs = gen_args_unpack_func(ArgsDesc),
	fun(Buffer) ->
			{RemainBuffer, ProtoId} = unpack_protoid(Buffer),
			unpack_with_funcs(ArgsUnpackFuncs, RemainBuffer)
	end.
%% --------------------------unpack related----------------------

for_spawn(N, N, F) ->
	[spawn(F)];
for_spawn(M, N, F) ->
	[spawn(F) | for_spawn(M + 1, N, F)].

for(N, N, F)->
	F();
for(M, N, F) ->
	F(),
	for(M + 1, N, F).

remote_pack_fun(PackF) ->
	receive
		{pack_mess, _From, _Seq, Mess} ->
			_Res = for(1, 3, fun() -> PackF(Mess) end),
			% From ! {Seq, Res},
			remote_pack_fun(PackF);
		{From, done} ->
			From ! {done, self()}
	end.

start_spawn_test(N, N, Pid) ->
	Pid ! {self(), done};
start_spawn_test(M, N, Pid) ->
	Pid ! {pack_mess, self(), M, ["40000.component_attribute.hp", 10000]},
	start_spawn_test(M + 1, N, Pid).

wait_done([]) ->
	ok;
wait_done([Pid | Tail]) ->
	receive
		{done, Pid} -> wait_done(Tail)
	end.

start_all_spawn_test([], _) ->
	ok;
start_all_spawn_test([Pid | Tail], Num) ->
	start_spawn_test(1, Num, Pid),
	start_all_spawn_test(Tail, Num).

spawn_test(N) ->
	statistics(wall_clock),
	%{PackF, _} = get_update_db_pack_unpack_func(),
	PackF = pass,
	Pids = for_spawn(1, N, fun() -> remote_pack_fun(PackF) end),
	EachTestCount = 1000000 div N,
	start_all_spawn_test(Pids, EachTestCount),
	{_, Time} = statistics(wall_clock),
	io:format("Spawn test cost:~p, each test count:~p~n", [Time/1000, EachTestCount]),
	wait_done(Pids),
	{_, DoneTime} = statistics(wall_clock),
	io:format("Wait Done Cost:~p~n", [DoneTime/1000]).

