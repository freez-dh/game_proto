-module(login2database).
-compile(export_all).
get_protos() ->
[
{
	"save_player",
	202,	[
		{"guid", uint64},
		{"finished", int8},
		{"data", bytes}
	]
},
{
	"request_player",
	203,	[
		{"guid", uint64}
	]
},
{
	"del_player_info",
	204,	[
		{"guid", uint64}
	]
}
].

