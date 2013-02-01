-module(gen_protocb).
-compile(export_all).

gen_name2fun([], Dict) ->
	Dict;
gen_name2fun([Proto | Tail], Dict) ->
	{Name, _, _} = Proto,
	Result = {
		bit_header:gen_proto_pack_func(Proto),
		bit_header:gen_proto_unpack_func(Proto)
	},
	NameAtom = list_to_atom(Name),
	NewDict = dict:store(NameAtom, Result, Dict),
	gen_name2fun(Tail, NewDict).

gen_pack_and_cb_mod(ProtoModule, ProtoCbModule) ->
	ProtoModule = game2database,
	ProtoCbModule = message_game2database,
	bit_header:gen_all_dict_pack_unpack_func(),
	Protos = apply(ProtoModule, get_protos, []),
	Name2PackUnpackFunc = gen_name2fun(Protos, dict:new()),
	Id2Name = gen_protoid2name(Protos, dict:new()),
	PackCall = fun(ProtoName, Socket, Args) ->
			{PackFunc, _UnpackFunc} = dict:fetch(ProtoName, Name2PackUnpackFunc),
			PackedBytes = PackFunc(Args) end,
			% gen_tcp:send(Socket, PackedBytes) end,
	UnpackCall = fun(ProtoId, Socket, Packet, Context) ->
			ProtoName = dict:fetch(ProtoId, Id2Name),
			{_PackFunc, UnpackFunc} = dict:fetch(ProtoName, Name2PackUnpackFunc),
			Args = UnpackFunc(Packet),
			apply(ProtoCbModule, ProtoName, [{Socket, Context} | Args]) end,
	{PackCall, UnpackCall}.

gen_protoid2fun([], Dict) ->
	Dict;
gen_protoid2fun([Proto | Tail], Dict) ->
	{_, ProtoId, _} = Proto,
	Result = {
		bit_header:gen_proto_pack_func(Proto),
		bit_header:gen_proto_unpack_func(Proto)
	},
	NewDict = dict:store(ProtoId, Result, Dict),
	gen_protoid2fun(Tail, NewDict).

gen_protoid2name([], Dict) ->
	Dict;
gen_protoid2name([Proto | Tail], Dict) ->
	{Name, ProtoId, _} = Proto,
	NewDict = dict:store(ProtoId, list_to_atom(Name), Dict),
	gen_protoid2name(Tail, NewDict).

