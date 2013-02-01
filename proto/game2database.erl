-module(game2database).
-compile(export_all).
get_protos() ->
[
{
	"update_db_uint",
	10,	[
		{"save_id", string},
		{"value", uint64}
	]
},
{
	"update_db_int",
	11,	[
		{"save_id", string},
		{"value", int32}
	]
},
{
	"update_db_double",
	12,	[
		{"save_id", string},
		{"value", string}
	]
},
{
	"update_db_string",
	13,	[
		{"save_id", string},
		{"value", string}
	]
},
{
	"update_db_list_remove",
	15,	[
		{"save_id", string},
		{"index", uint32}
	]
},
{
	"update_db_list_update_int",
	16,	[
		{"save_id", string},
		{"index", uint32},
		{"value", int32}
	]
},
{
	"update_db_list_update_uint",
	19,	[
		{"save_id", string},
		{"index", uint32},
		{"value", uint64}
	]
},
{
	"update_db_list_update_double",
	22,	[
		{"save_id", string},
		{"index", uint32},
		{"value", string}
	]
},
{
	"update_db_list_update_string",
	23,	[
		{"save_id", string},
		{"index", uint32},
		{"value", string}
	]
},
{
	"update_db_list_clear",
	25,	[
		{"save_id", string}
	]
},
{
	"update_db_map_clear",
	26,	[
		{"save_id", string}
	]
},
{
	"update_db_map_remove",
	27,	[
		{"save_id", string},
		{"key", string}
	]
},
{
	"update_db_map_update_int",
	28,	[
		{"save_id", string},
		{"key", string},
		{"value", int32}
	]
},
{
	"update_db_map_update_uint",
	29,	[
		{"save_id", string},
		{"key", string},
		{"value", uint64}
	]
},
{
	"update_db_map_update_string",
	30,	[
		{"save_id", string},
		{"key", string},
		{"value", string}
	]
},
{
	"update_db_map_update_double",
	31,	[
		{"save_id", string},
		{"key", string},
		{"value", string}
	]
},
{
	"update_db_add_obj",
	32,	[
		{"save_id", string},
		{"sub_fields", save_add_field, 0}
	]
},
{
	"request_player",
	200,	[
		{"guid", uint64}
	]
},
{
	"save_player",
	201,	[
		{"guid", uint64},
		{"finished", int8},
		{"data", bytes}
	]
},
{
	"save_player_nocb",
	202,	[
		{"guid", uint64},
		{"finished", int8},
		{"data", bytes}
	]
},
{
	"sent_all_player",
	203,	[
	]
},
{
	"create_player",
	204,	[
		{"guid", uint64},
		{"name", string},
		{"urs", string},
		{"occupation", int8},
		{"level", uint32},
		{"cash", uint32},
		{"title", string}
	]
},
{
	"find_player_info",
	207,	[
		{"src_id", uint64},
		{"tgt_name", string}
	]
},
{
	"find_player_info4relation",
	208,	[
		{"src_id", uint64},
		{"tgt_name", string},
		{"relation_code", int8}
	]
},
{
	"get_player_guid_by_name",
	210,	[
		{"req_id", uint32},
		{"player_name", string}
	]
},
{
	"reload_player",
	211,	[
		{"guid", uint64}
	]
},
{
	"force_serialize_to_file",
	212,	[
		{"guid", uint64}
	]
},
{
	"get_player_name_by_guid",
	213,	[
		{"req_id", uint32},
		{"player_guid", uint64}
	]
},
{
	"save_player_end",
	214,	[
		{"player_guid", uint64}
	]
}
].

