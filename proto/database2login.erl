-module(database2login).
-compile(export_all).
get_protos() ->
[
{
	"save_player_ok",
	202,	[
		{"guid", uint64}
	]
},
{
	"load_player",
	203,	[
		{"guid", uint64},
		{"finished", int8},
		{"data", bytes}
	]
},
{
	"load_player_failed",
	204,	[
		{"guid", uint64}
	]
}
].

