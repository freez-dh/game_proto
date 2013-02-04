-module(database2game).
-compile(export_all).
get_protos() ->
[
{
	"load_player",
	200,	[
		{"guid", uint64},
		{"finished", int8},
		{"data", bytes}
	]
},
{
	"save_player_ok",
	201,	[
		{"guid", uint64}
	]
},
{
	"got_all_player",
	202,	[
	]
},
{
	"send_player_info",
	204,	[
		{"is_succ", int8},
		{"src_id", uint64},
		{"tgt_id", uint64},
		{"tgt_name", string},
		{"tgt_lv", uint32},
		{"tgt_occ", int8},
		{"tgt_title", string}
	]
},
{
	"send_player_info4relation",
	205,	[
		{"is_succ", int8},
		{"src_id", uint64},
		{"tgt_id", uint64},
		{"tgt_name", string},
		{"tgt_lv", uint32},
		{"tgt_occ", int8},
		{"relation_code", int8}
	]
},
{
	"send_player_guid",
	206,	[
		{"req_id", uint32},
		{"player_guid", uint64}
	]
},
{
	"send_player_name",
	207,	[
		{"req_id", uint32},
		{"player_guid", uint64},
		{"player_name", string}
	]
},
{
	"confirm_save_player_end",
	208,	[
		{"guid", uint64}
	]
}
].

