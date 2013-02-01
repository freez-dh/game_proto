-module(table).
-compile(export_all).
get_protos() ->
[
{
	"point3d",
	[
		{"x", float},
		{"y", float},
		{"z", float}
	]
},
{
	"key_value",
	[
		{"key", string},
		{"value", string}
	]
},
{
	"point2d",
	[
		{"x", float},
		{"z", float}
	]
},
{
	"login_charinfo",
	[
		{"guid", uint64},
		{"name", string},
		{"level", int32},
		{"sex", int32},
		{"occupation", int32},
		{"hair_style", uint8},
		{"hair_color", uint8},
		{"face", uint8},
		{"skin", uint8},
		{"equips", uint32},
		{"equip_pos", uint8}
	]
},
{
	"vclient_attr",
	[
		{"faction", uint32},
		{"name", string},
		{"level", uint32},
		{"title", string},
		{"cash", uint32},
		{"sex", uint8},
		{"exp", uint32},
		{"occupation", uint8},
		{"scene_id", uint32},
		{"other", string}
	]
},
{
	"hero_attr",
	[
		{"guid", uint64},
		{"name", string},
		{"hair_style", uint8},
		{"hair_color", uint8},
		{"face", uint8},
		{"skin", uint8},
		{"title_id", uint32},
		{"type_id", uint32},
		{"x", float},
		{"z", float},
		{"dir", float},
		{"dietype", int8},
		{"revive_type", uint8}
	]
},
{
	"player_attr",
	[
		{"guid", uint64},
		{"name", string},
		{"hair_style", uint8},
		{"hair_color", uint8},
		{"face", uint8},
		{"skin", uint8},
		{"title_id", uint32},
		{"type_id", uint32},
		{"transform_type_id", uint32},
		{"x", float},
		{"z", float},
		{"dir", float},
		{"emoteid", uint8},
		{"dietype", int8}
	]
},
{
	"server_addr",
	[
		{"ip", string},
		{"port", uint16}
	]
},
{
	"player_equips",
	[
		{"body", uint32},
		{"head", uint32},
		{"shoulder", uint32},
		{"glove", uint32},
		{"foot", uint32},
		{"left_weapon", uint32},
		{"right_weapon", uint32}
	]
},
{
	"npc_attr",
	[
		{"guid", uint64},
		{"name", string},
		{"type_id", int32},
		{"transform_type_id", uint32},
		{"pos", vector2},
		{"dir", float},
		{"dietype", int8},
		{"affixs", uint32},
		{"first_born", uint8}
	]
},
{
	"mirror_attr",
	[
		{"guid", uint64},
		{"name", string},
		{"master_id", uint64},
		{"type_id", int32},
		{"transform_type_id", uint32},
		{"pos", vector2},
		{"dir", float},
		{"first_born", int8},
		{"dietype", int8},
		{"hair_style", uint8},
		{"hair_color", uint8},
		{"face", uint8},
		{"skin", uint8}
	]
},
{
	"tele_attr",
	[
		{"guid", uint64},
		{"tele_id", int32},
		{"x", float},
		{"z", float},
		{"activated", int8},
		{"dir", float}
	]
},
{
	"chunk_attr",
	[
		{"x", float},
		{"y", float},
		{"z", float},
		{"flag", int16}
	]
},
{
	"drop_item_info",
	[
		{"guid", uint64},
		{"from_id", uint64},
		{"owner_id", uint64},
		{"attacker_id", uint64},
		{"type_id", int32},
		{"quality", int32},
		{"x", float},
		{"y", float},
		{"z", float},
		{"dir", float},
		{"protect_time", int32},
		{"amount", int16},
		{"drop_anim", int8},
		{"is_rolling", int8}
	]
},
{
	"item_info",
	[
		{"guid", uint64, 0},
		{"type_id", int32},
		{"grid", uint16},
		{"amount", uint16},
		{"quality", uint8},
		{"bind_status", uint8},
		{"lvl_req", uint16},
		{"identified", uint8}
	]
},
{
	"item_sell_info",
	[
		{"type_id", int32},
		{"amount", uint16},
		{"overlap", uint8},
		{"quality", uint8},
		{"price", int32},
		{"currency_type", uint8},
		{"token", uint32},
		{"lvl_req", uint16},
		{"identified", uint8}
	]
},
{
	"item_tips_attr",
	[
		{"key", string},
		{"value", string}
	]
},
{
	"item_sort_info",
	[
		{"guid", uint64},
		{"type_id", uint32},
		{"grid", uint16},
		{"amount", uint16}
	]
},
{
	"skill_tips_attr",
	[
		{"content", string},
		{"consume_mp", int32},
		{"consume_hp", int32},
		{"consume_soul", int32},
		{"consume_rage", int32},
		{"consume_crystal", int32},
		{"sing_time", float},
		{"cold_down", float}
	]
},
{
	"equip_dict_num",
	[
		{"key", string},
		{"value", float}
	]
},
{
	"suit_attr",
	[
		{"key", string},
		{"value", float},
		{"active", uint8}
	]
},
{
	"suit_parts_info",
	[
		{"name", string},
		{"active", uint8}
	]
},
{
	"relation",
	[
		{"guid", uint64},
		{"name", string},
		{"level", int32},
		{"occupation", uint8},
		{"online", uint8},
		{"relations", string}
	]
},
{
	"online_status",
	[
		{"guid", uint64},
		{"online", uint8},
		{"level", int32}
	]
},
{
	"quit_info",
	[
		{"guid", uint64},
		{"sn", string},
		{"aid", int32},
		{"level", int32},
		{"online_time", int32},
		{"idle_time", uint32},
		{"continue_time", uint32},
		{"total_online", int32},
		{"ip", string},
		{"disk_id", string},
		{"x", float},
		{"z", float},
		{"scene_proto", int32},
		{"exp", int32},
		{"cash", int32},
		{"friend_no1", int32},
		{"friend_no2", int32}
	]
},
{
	"option",
	[
		{"key", int32},
		{"value", string},
		{"status", uint16}
	]
},
{
	"toolbar_item",
	[
		{"index", uint8},
		{"type", uint8},
		{"value", uint64}
	]
},
{
	"task_item_reward_info",
	[
		{"type_id", uint32},
		{"amount", uint32}
	]
},
{
	"task_list_data",
	[
		{"task_id", uint32},
		{"name", string},
		{"status", uint16}
	]
},
{
	"activity_list_data",
	[
		{"activity_id", uint32},
		{"status", uint16},
		{"cycle", uint16},
		{"valid_time_index", uint8}
	]
},
{
	"trigger_attr",
	[
		{"guid", uint64},
		{"name", string},
		{"type_id", int32},
		{"x", float},
		{"z", float},
		{"dir", float},
		{"state", int8},
		{"is_enable", int8}
	]
},
{
	"trigger_block_attr",
	[
		{"block_index", int8},
		{"block_state", int8},
		{"min_x", float},
		{"min_y", float},
		{"min_z", float},
		{"max_x", float},
		{"max_y", float},
		{"max_z", float}
	]
},
{
	"trigger_block_attr_s",
	[
		{"scene_id", uint64},
		{"block_index", int8},
		{"block_state", int8},
		{"min_x", float},
		{"min_y", float},
		{"min_z", float},
		{"max_x", float},
		{"max_y", float},
		{"max_z", float}
	]
},
{
	"buff_data",
	[
		{"buff_id", uint32},
		{"attacker_id", uint64},
		{"type_id", uint32},
		{"lifetime", float},
		{"total_lifetime", float},
		{"layers", uint8},
		{"status", uint8}
	]
},
{
	"task_aim_data",
	[
		{"aim_type", uint8},
		{"data", uint32}
	]
},
{
	"task_extra_data",
	[
		{"aim_type", uint8},
		{"data", uint32}
	]
},
{
	"skill_info",
	[
		{"skill_id", uint32},
		{"skill_level", uint32},
		{"rune_index", uint8}
	]
},
{
	"mercenary_skill",
	[
		{"skill_id", uint32},
		{"exp", uint32},
		{"level", uint32}
	]
},
{
	"mercenary_attr",
	[
		{"guid", uint64},
		{"type_id", int32},
		{"page", uint8}
	]
},
{
	"player_pearl_info",
	[
		{"type_id", uint32},
		{"level", uint32},
		{"exp", uint32},
		{"page", int8}
	]
},
{
	"mercenary_pearl_info",
	[
		{"skill_id", uint32},
		{"level", uint32},
		{"slot", int8}
	]
},
{
	"equip_info",
	[
		{"type_id", uint32},
		{"pos", uint8},
		{"sfx_id", uint32}
	]
},
{
	"relation_info",
	[
		{"guid", uint64},
		{"relation_code", uint8}
	]
},
{
	"entered_cpyscn",
	[
		{"prototype", uint32},
		{"daily_count", int32}
	]
},
{
	"gem_insert_info",
	[
		{"gem_guid", uint64},
		{"socket_idx", uint8}
	]
},
{
	"stall_log",
	[
		{"data", string}
	]
},
{
	"stall_sell_item",
	[
		{"guid", uint64},
		{"type_id", uint32},
		{"amount", uint16},
		{"price", uint32},
		{"pos", uint8},
		{"quality", uint8}
	]
},
{
	"stall_buy_item",
	[
		{"pos", uint8},
		{"type_id", uint32},
		{"amount", uint16},
		{"price", uint32}
	]
},
{
	"consignment_item",
	[
		{"item_guid", uint64},
		{"seller_guid", uint64},
		{"seller_name", string},
		{"type_id", uint32},
		{"quality", uint32},
		{"amount", uint32},
		{"price", uint32},
		{"time_stamp", uint32},
		{"out_date_time", uint32},
		{"appoint_player", string},
		{"sold_time", uint32},
		{"remain_time", int32}
	]
},
{
	"consignment_save_info",
	[
		{"item_guid", uint64},
		{"seller_guid", uint64},
		{"seller_name", string},
		{"type_id", uint32},
		{"quality", uint32},
		{"amount", uint32},
		{"price", uint32},
		{"time_stamp", uint32},
		{"out_date_time", uint32},
		{"item_tips", string},
		{"item_data", string},
		{"appoint_player", string}
	]
},
{
	"grade_info",
	[
		{"id", uint64},
		{"name", string},
		{"grade", uint8}
	]
},
{
	"list_amount",
	[
		{"list", uint32},
		{"amount", uint32}
	]
},
{
	"setting",
	[
		{"show_halmet", uint8},
		{"show_other_name", uint8},
		{"show_monster_hp_bar", uint8},
		{"show_num", uint8},
		{"show_item_label", uint8},
		{"show_skill_tips", uint8},
		{"show_monster_name", uint8},
		{"allow_stranger_mess", uint8},
		{"allow_trade", uint8},
		{"allow_fight", uint8},
		{"allow_team", uint8},
		{"show_rookie_guide", uint8},
		{"control", bytes},
		{"show_fumospire_guide", uint8}
	]
},
{
	"team_plfm_entry",
	[
		{"leader_id", uint64},
		{"leader", string},
		{"level", uint16},
		{"act_id", uint16},
		{"difficulty", uint8},
		{"team_size", uint8},
		{"auto_join", uint8}
	]
},
{
	"tele_info",
	[
		{"guid", uint64},
		{"type_id", uint32},
		{"x", float},
		{"z", float}
	]
},
{
	"es_rank_entity",
	[
		{"guid", uint64},
		{"name", string},
		{"level", uint16},
		{"equip_score", uint32}
	]
},
{
	"cc_rank_entity",
	[
		{"guid", uint64},
		{"name", string},
		{"level", uint16},
		{"combat_capacity", uint32}
	]
},
{
	"map_entity_info",
	[
		{"guid", uint64},
		{"type_id", uint32},
		{"x", float},
		{"z", float}
	]
},
{
	"ranking_item",
	[
		{"guid", uint64},
		{"rank", uint16},
		{"trend", int8},
		{"point", uint32},
		{"extra_keys", string},
		{"extra_values", string}
	]
},
{
	"cpyscn_label_info",
	[
		{"title_str", string},
		{"num_str", string}
	]
},
{
	"cpyscn_reward_info",
	[
		{"type_id", uint32},
		{"count", uint32}
	]
},
{
	"save_add_field",
	[
		{"sub_save_id", string},
		{"value_type", uint8},
		{"value", string}
	]
},
{
	"cpyscn_check_fail",
	[
		{"guid", uint64},
		{"fail_msg", string}
	]
},
{
	"gang_base_info",
	[
		{"id", uint32},
		{"status", int8},
		{"level", uint8},
		{"name", string},
		{"admin_guid", uint64},
		{"admin_name", string}
	]
},
{
	"gang_member_brief",
	[
		{"guid", uint64},
		{"name", string},
		{"level", uint32},
		{"occ", uint8},
		{"group", uint8},
		{"ws_point", uint32},
		{"online", uint8}
	]
},
{
	"gang_pending_member_brief",
	[
		{"guid", uint64},
		{"name", string},
		{"level", uint32},
		{"occ", uint8},
		{"online", uint8}
	]
},
{
	"gang_building_info",
	[
		{"id", uint8},
		{"level", uint8},
		{"cost", uint32},
		{"percent", float}
	]
},
{
	"gang_strategy_info",
	[
		{"id", uint8},
		{"level", uint8},
		{"cost", uint32},
		{"percent", float}
	]
},
{
	"wizcmd",
	[
		{"name", string},
		{"desc", string},
		{"alias", string}
	]
},
{
	"team_cross",
	[
		{"guid", uint64},
		{"x", float},
		{"z", float},
		{"master_id", uint64},
		{"is_enable", int8}
	]
},
{
	"owned_scene_info",
	[
		{"scene_group_id", uint64},
		{"scene_ids", uint64},
		{"difficulty", uint8},
		{"entered_players", uint64}
	]
},
{
	"tele_session_info",
	[
		{"session_id", uint64},
		{"tele_guid", uint64},
		{"tgt_group_type", uint32},
		{"tgt_level", uint8},
		{"tgt_pos_x", float},
		{"tgt_pos_z", float},
		{"target_type", uint8},
		{"face_dir", float},
		{"difficulty", uint8},
		{"requestor_guid", uint64},
		{"team_id", uint64}
	]
},
{
	"mall_item",
	[
		{"type_id", uint32},
		{"is_hot", uint8},
		{"is_recommend", uint8},
		{"price", uint64},
		{"original_price", uint64}
	]
}
].

