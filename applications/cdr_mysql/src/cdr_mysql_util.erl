%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cdr_mysql_util).

-include_lib("cdr_mysql.hrl").

-define(EMYSQL_CFG(X),              [<<"emysql">>, X]).
-define(EMYSQL_POOL,                'cdr_mysql_pool').

-define(INSERT_CDR,                 'insert_cdr').
-define(INSERT_BRIDGE_HISTORY,      'insert_bridge_history').

-export([init/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    init_emysql_pool(),
    prepare_insert_cdr_stmt(),
    prepare_insert_bridge_history_stmt().

-spec init_emysql_pool() -> 'ok'.
init_emysql_pool() ->
    Db = [{'host',      whapps_config:get_string(?CDR_MYSQL_CFG, ?EMYSQL_CFG(<<"host">>), <<"127.0.0.1">>)}
          ,{'user',     whapps_config:get_string(?CDR_MYSQL_CFG, ?EMYSQL_CFG(<<"user">>), <<"kazoo">>)}
          ,{'password', whapps_config:get_string(?CDR_MYSQL_CFG, ?EMYSQL_CFG(<<"password">>), <<"123456">>)}
          ,{'database', whapps_config:get_string(?CDR_MYSQL_CFG, ?EMYSQL_CFG(<<"database">>), <<"kazoo">>)}
          ,{'encoding', whapps_config:get_atom(?CDR_MYSQL_CFG, ?EMYSQL_CFG(<<"encoding">>), utf8)}
          ,{'size',     whapps_config:get_integer(?CDR_MYSQL_CFG, ?EMYSQL_CFG(<<"pool_size">>), 20)}
         ],
    emysql:add_pool(?EMYSQL_POOL, Db).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%--------------------------------------------------------------------
-spec prepare_insert_cdr_stmt() -> 'ok'.
prepare_insert_cdr_stmt() ->
    Cols = [<<"call_id">>
            ,<<"account_id">>
            ,<<"owner_id">>
            ,<<"owner_name">>
            ,<<"caller_number">>
            ,<<"callee_number">>
            ,<<"direction">>
            ,<<"call_type">>
            ,<<"start_time">>
            ,<<"end_time">>
            ,<<"during_sec">>
            ,<<"ringing_sec">>
            ,<<"billing_sec">>
            ,<<"sip_realm">>
            ,<<"sip_user">>
            ,<<"hangup_cause">>
            ,<<"disposition">>
            ,<<"media_server">>
            ,<<"media_name">>
            ,<<"other_data">>
           ],
    Tab = <<"cdr">>,
    prepare_insert_stmt(?INSERT_CDR, Tab, Cols).

-spec prepare_insert_bridge_history_stmt() -> 'ok'.
prepare_insert_bridge_history_stmt() ->
    Cols = [<<"leg">>
            ,<<"other_leg">>
            ,<<"timestamp">>
           ],
    Tab = <<"bridge_history">>,
    prepare_insert_stmt(?INSERT_BRIDGE_HISTORY, Tab, Cols).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%--------------------------------------------------------------------
prepare_insert_stmt(StmtName, Table, Cols) ->
    Stmt = build_insert_stmt(Table, Cols),
    lager:debug("prepare statement ~p: ~p", [StmtName, Stmt]),
    emysql:prepare(StmtName, Stmt).

build_insert_stmt(Table, Cols) when is_list(Cols) ->
    do_build_insert_stmt(Cols, <<"INSERT INTO ", Table/binary, " SET ">>).

do_build_insert_stmt([Col], Stmt) ->
    <<Stmt/binary, Col/binary, " = ?">>;
do_build_insert_stmt([Col | Cols], Stmt) ->
    do_build_insert_stmt(Cols, <<Stmt/binary, Col/binary, " = ?, ">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%--------------------------------------------------------------------
