-module(config).
-compile(export_all).

%% need config
get_server_group_id() ->
	24.

get_database_ip() ->
	"127.0.0.1".

get_internal_ip() ->
	"127.0.0.1".

get_external_ip() ->
	"127.0.0.1".

%% do not need config
get_group_port_base() ->
	10000 + get_server_group_id() * 50.

get_database_master_port() ->
	get_group_port_base() + 4.

get_database_game_port() ->
	get_group_port_base() + 5.

get_database_login_port() ->
	get_group_port_base() + 6.

