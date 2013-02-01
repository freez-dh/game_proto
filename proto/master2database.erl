-module(master2database).
-compile(export_all).
get_protos() ->
[
{
	"update_module",
	200,	[
		{"module_name", string}
	]
},
{
	"request_player4gmt",
	201,	[
		{"guid", uint64}
	]
},
{
	"player_offline",
	202,	[
		{"guid", uint64}
	]
},
{
	"get_player_guids_by_urs",
	203,	[
		{"req_id", uint64},
		{"urs", string}
	]
},
{
	"get_player_guids_by_name",
	204,	[
		{"req_id", uint64},
		{"name", string}
	]
}
].

