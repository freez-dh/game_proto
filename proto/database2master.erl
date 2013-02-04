-module(database2master).
-compile(export_all).
get_protos() ->
[
{
	"regist_database_server",
	1,	[
		{"address", string}
	]
},
{
	"load_player4gmt",
	200,	[
		{"guid", uint64},
		{"finished", int8},
		{"data", bytes}
	]
},
{
	"kick_player_for_reload",
	201,	[
		{"guid", uint64}
	]
},
{
	"send_player_guids",
	202,	[
		{"req_id", uint64},
		{"guids", uint64, 0}
	]
}
].

