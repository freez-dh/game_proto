-module(proto_server).
-compile(export_all).
-define(PORT, 30000).

start_server(Port, ProtoModule, ProtoCbModule, CbContext) ->
	{_PackCall, UnpackCall} = gen_protocb:gen_pack_and_cb_mod(ProtoModule, ProtoCbModule),
	spawn(fun() -> {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
						{reuseaddr, true}]),
			spawn(fun() -> par_connect(ListenSocket, UnpackCall, CbContext) end),
			% wait for ever not let listen socket close
			receive
				ok -> true
			end
		end).

par_connect(ListenSocket, UnpackCall, CbContext) ->
	{ok, ClientSocket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> par_connect(ListenSocket, UnpackCall, CbContext) end),
	handle_client_loop(ClientSocket, [], UnpackCall, CbContext).

check_header(N, N, _) ->
	-1;
check_header(M, N, Bin) ->
	Byte = binary:at(Bin, M),
	case Byte band 16#80 =:= 0 of
		true -> M;
		false -> check_header(M + 1, N, Bin)
	end.
header_size(Bin) ->
	check_header(0, size(Bin), Bin).

get_one_packet(ClientSocket, HeaderSize, Sofar, SofarLen, UnpackCall, CbContext) ->
	case SofarLen >= HeaderSize of
		true ->
			Joined = list_to_binary(lists:reverse(Sofar)),
			{Packet, NextPacket} = split_binary(Joined, HeaderSize),
			{_Remain, ProtoId} = bit_header:unpack_protoid(Packet),
			% io:format("Receive proto id ~p, packet:~p, next_packet:~p~n", [ProtoId, Packet, NextPacket]),
			UnpackCall(ProtoId, ClientSocket, Packet, CbContext),

			HeaderPos = header_size(NextPacket),
			% io:format("Headerpos ~p~n", [HeaderPos]),
			case HeaderPos of
				-1 -> handle_client_loop(ClientSocket, [NextPacket],  UnpackCall, CbContext);
				_ ->
					{NextPacketSize, NextRemain} = sub_packet_header(NextPacket, [], HeaderPos),
					get_one_packet(ClientSocket, NextPacketSize, [NextRemain], size(NextRemain),
						UnpackCall, CbContext)
			end;
		false ->
			receive
				{tcp, ClientSocket, Bin} ->
					get_one_packet(ClientSocket, HeaderSize, [Bin | Sofar], SofarLen + size(Bin),
						UnpackCall, CbContext);
				{tcp_closed, ClientSocket} ->
					io:format("Socket closed ~p~n", [ClientSocket])
			end
	end.

sub_packet_header(NewBin, Sofar, HeaderPos) ->
	ThisPackSize = size(NewBin),
	AllReceived = list_to_binary(lists:reverse([NewBin | Sofar])),
	AllReceivedSize = size(AllReceived),
	BeforeSize = AllReceivedSize - ThisPackSize,
	SubIndex = BeforeSize + HeaderPos + 1,
	% io:format("Sub index:~p~n", [SubIndex]),
	{HeaderBin, Remain} = split_binary(AllReceived, SubIndex),
	% io:format("Header bin ~p, remain bin:~p~n", [HeaderBin, Remain]),
	{HeaderBin, Remain} = split_binary(AllReceived, SubIndex),
	PacketSize = bit_header:new_unpack_int(HeaderBin),
	% io:format("Packet size:~p~n", [PacketSize]),
	{PacketSize, Remain}.

handle_client_loop(ClientSocket, Sofar, UnpackCall, CbContext) ->
	receive
		{tcp, ClientSocket, Bin} ->
			% io:format("Received ~p~n", [Bin]),
			HeaderPos = header_size(Bin),
			% io:format("Headerpos ~p~n", [HeaderPos]),
			case HeaderPos of
				-1 -> handle_client_loop(ClientSocket, [Bin | Sofar], UnpackCall, CbContext);
				_ ->
					{PacketSize, Remain} = sub_packet_header(Bin, Sofar, HeaderPos),
					get_one_packet(ClientSocket, PacketSize, [Remain], size(Remain), UnpackCall, CbContext)
			end;
		{tcp_closed, ClientSocket} ->
			io:format("Socket closed ~p~n", [ClientSocket])
	end.

send_by_bytes_s(_, []) ->
	ok;
send_by_bytes_s(Socket, [Byte |  Remain]) ->
	gen_tcp:send(Socket, <<Byte>>),
	receive
	after 10 ->
			true
	end,
	send_by_bytes_s(Socket, Remain).

send_and_close() ->
	{ok, Socket} = gen_tcp:connect("127.0.0.1", ?PORT, [binary]),
	{PackF, _} = bit_header:get_update_db_pack_unpack_func(),
	Packet = PackF(["40000.999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
			0000000000000000000000000000000000000000000000
			0000000000000000000000000000000000000000000000000000000000000
			000000000000000000000000000000000000000000000component_attribute.hp", 100000]),
	Header = bit_header:new_pack_int(size(Packet)),
	io:format("Send Packet ~p,~p~n", [Header, Packet]),
	send_by_bytes_s(Socket, binary:bin_to_list(Header)),
	% gen_tcp:send(Socket, Header),
	send_by_bytes_s(Socket, binary:bin_to_list(Packet)),
	% gen_tcp:send(Socket, Packet),

	send_by_bytes_s(Socket, binary:bin_to_list(Header)),
	send_by_bytes_s(Socket, binary:bin_to_list(Packet)),

	send_by_bytes_s(Socket, binary:bin_to_list(Header)),
	send_by_bytes_s(Socket, binary:bin_to_list(Packet)),

	send_by_bytes_s(Socket, binary:bin_to_list(Header)),
	send_by_bytes_s(Socket, binary:bin_to_list(Packet)),

	gen_tcp:close(Socket).

connect_to_server() ->
	{ok, Socket} = gen_tcp:connect("127.0.0.1", ?PORT, [binary]),
	Socket.

