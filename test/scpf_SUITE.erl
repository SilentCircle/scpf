%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'scpf' module.
%%%-----------------------------------------------------------------

-module(scpf_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("scpf_SUITE.hrl").

-import(scpf_test_support,
        [
         get_sim_config/2,
         get_svc_config/2,
         make_sim_notification/2,
         multi_store/2,
         pv/2,
         pv/3,
         req_val/2,
         start_apnsv3_sim/3,
         start_gcm_sim/3
        ]).

-compile(export_all).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> [
            {timetrap, {seconds, 30}},
            {require, sc_push},
            {require, gcm_erl},
            {require, apns_erlv3},
            {require, registrations},
            {require, gcm_sim_node},
            {require, gcm_sim_config},
            {require, apnsv3_sim_node},
            {require, apnsv3_sim_config},
            {require, lager},
            {require, databases},
            {require, connect_info}
           ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Sasl = ct:get_config(sasl),
    ct:pal("SASL: ~p~n", [Sasl]),

    set_all_env(sasl, Sasl),
    ensure_started(sasl),

    Databases = ct:get_config(databases),
    ct:pal("Databases: ~p~n", [Databases]),

    ConnectInfo = ct:get_config(connect_info),
    ct:pal("connect_info config: ~p~n", [ConnectInfo]),

    Cookie = "scpf",
    {ApnsV3SimNode, ApnsV3SimCfg} = get_sim_config(apnsv3_sim_config, Config),
    {GcmSimNode, GcmSimCfg} = get_sim_config(gcm_sim_config, Config),

    %% Start GCM simulator
    ct:log("Starting GCM simulator with config: ~p", [GcmSimCfg]),
    {ok, GcmSimStartedApps} = start_gcm_sim(GcmSimNode, GcmSimCfg, Cookie),

    ct:log("Starting APNS HTTP/2 simulator with config: ~p", [ApnsV3SimCfg]),
    {ok, ApnsSimStartedApps} = start_apnsv3_sim(ApnsV3SimNode, ApnsV3SimCfg,
                                                Cookie),
    ct:pal("init_per_suite: Started apns sim apps=~p", [ApnsSimStartedApps]),
    Started = start_per_suite_apps(Config),
    ct:pal("init_per_suite: Started apps=~p", [Started]),
    Registrations = ct:get_config(registrations),
    ct:pal("Registrations: ~p", [Registrations]),
    multi_store(Config, [{apnsv3_sim_started_apps, ApnsSimStartedApps},
                         {gcm_sim_started_apps, GcmSimStartedApps},
                         {suite_started_apps, Started},
                         {apnsv3_sim_node, ApnsV3SimNode},
                         {apnsv3_sim_config, ApnsV3SimCfg},
                         {gcm_sim_node, GcmSimNode},
                         {gcm_sim_config, GcmSimCfg},
                         {registrations, Registrations},
                         {databases, Databases},
                         {connect_info, ConnectInfo}]
               ).

%%--------------------------------------------------------------------
end_per_suite(Config) ->
    % We don't care about sim_started_apps because they are on
    % a slave node that will get shut down.
    Apps = req_val(suite_started_apps, Config),
    [application:stop(App) || App <- Apps],
    ok.

%%--------------------------------------------------------------------
init_per_group(Group, Config) when Group =:= internal_db;
                                   Group =:= external_db ->
    ct:pal("===== Running test group ~p =====", [Group]),
    DBMap = req_val(databases, Config),
    DBInfo = maps:get(Group, DBMap),
    NewConfig = lists:keystore(dbinfo, 1, Config, {dbinfo, DBInfo}),
    ok = application:set_env(sc_push_lib, db_pools, db_pools(DBInfo, NewConfig)),
    {ok, DbPools} = application:get_env(sc_push_lib, db_pools),
    ct:pal("DbPools: ~p", [DbPools]),

    case check_db_availability(NewConfig) of
        ok ->
            NewConfig;
        Error ->
            Reason = lists:flatten(
                       io_lib:format("Cannot communicate with ~p,"
                                     " error: ~p", [Group, Error])),
            {skip, Reason}
    end;
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    init_per_testcase_common(Config).

%%--------------------------------------------------------------------
end_per_testcase(_Case, Config) ->
    end_per_testcase_common(Config).

%%--------------------------------------------------------------------
groups() ->
    [
     {service, [], [
                    {internal_db, [], service_test_groups()},
                    {external_db, [], service_test_groups()}
                   ]
     }
    ].

%%--------------------------------------------------------------------
service_test_groups() ->
    [
     send_msg_test,
     send_msg_fail_test,
     send_msg_no_reg_test
    ].

%%--------------------------------------------------------------------
all() ->
    [
        {group, service}
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

send_msg_test(doc) ->
    ["sc_push:send/1 should send a message to all push services"];
send_msg_test(suite) ->
    [];
send_msg_test(Config) ->
    RegPLs = req_val(registrations, Config),
    SimHdrs = [{"X-GCMSimulator-StatusCode", "200"},
               {"X-GCMSimulator-Results", "message_id:1000"}],
    ok = sc_push:register_ids(RegPLs),
    [
        begin
            Tag = req_val(tag, RegPL),
            Notification = [
                    {alert, ?ALERT_MSG},
                    {tag, Tag},
                    {aps, [{badge, 42}]},
                    {gcm, [{collapse_key, <<"alert">>}]},
                    {return, success} % Will only work with sc_push_svc_null!
                    ],
            ct:pal("Sending notification ~p to ~p", [Notification, Tag]),
            Opts = [{http_headers, SimHdrs}], % Only for GCM
            Res = sc_push:send(Notification, Opts),
            ct:pal("Got result ~p", [Res]),
            [{ok, {UUID, Props}}] = Res,
            ct:pal("Sent notification to ~p, uuid: ~s, props: ~p",
                   [Tag, uuid_to_str(UUID), Props])
        end || RegPL <- RegPLs
    ],
    deregister_ids(RegPLs),
    ok.

%%--------------------------------------------------------------------
send_msg_fail_test(doc) ->
    ["sc_push:send/1 should fail"];
send_msg_fail_test(suite) ->
    [];
send_msg_fail_test(Config) ->
    RegPLs = req_val(registrations, Config),
    ServiceName = null,
    NullRegPL = get_service_properties(ServiceName, RegPLs),
    ok = sc_push:register_id(NullRegPL),
    Tag = req_val(tag, NullRegPL),
    ExpectedError = {error, forced_test_failure},
    Notification = [
        {alert, ?ALERT_MSG},
        {tag, Tag},
        {return, ExpectedError} % Will only work with sc_push_svc_null!
    ],
    ct:pal("Sending notification ~p to ~p", [Notification, Tag]),
    Res = sc_push:send(Notification),
    ct:pal("Got result: ~p", [Res]),
    [{error, {_UUID, ExpectedError}}] = Res,
    ct:pal("Expected failure sending notification to service ~p, tag ~p",
           [ServiceName, Tag]),
    deregister_ids(RegPLs),
    ok.

%%--------------------------------------------------------------------
send_msg_no_reg_test(doc) ->
    ["sc_push:send/1 should fail with tag not found"];
send_msg_no_reg_test(suite) ->
    [];
send_msg_no_reg_test(Config) ->
    RegPLs = req_val(registrations, Config),
    ServiceName = null,
    NullRegPL = get_service_properties(ServiceName, RegPLs),
    ok = sc_push:register_id(NullRegPL),
    FakeTag = <<"$$Completely bogus tag$$">>,
    ExpectedError = [{reg_not_found_for_tag, FakeTag}],
    Notification = [
        {alert, ?ALERT_MSG},
        {tag, FakeTag}
    ],
    Res = sc_push:send(Notification),
    ct:pal("Got result: ~p", [Res]),
    [{error, ExpectedError}] = Res,
    ct:pal("Got expected error (~p) from send notification", [ExpectedError]),
    deregister_ids(RegPLs),
    ok.

%%====================================================================
%% Internal helper functions
%%====================================================================
init_per_testcase_common(Config) ->
    (catch end_per_testcase_common(Config)),
    db_create(Config),
    Apps = [sc_push, gcm_erl, apns_erlv3],
    AppList = lists:foldl(
                fun(App, Acc) ->
                        set_env(App, get_svc_config(App, Config)),
                        {ok, L} = application:ensure_all_started(App),
                        Acc ++ L
                end, [], Apps),
    multi_store(Config, [{started_apps, AppList}]).

%%--------------------------------------------------------------------
end_per_testcase_common(Config) ->
    Apps = lists:reverse(pv(started_apps, Config, [])),
    _ = [ok = application:stop(App) || App <- Apps],
    db_destroy(Config),
    lists:keydelete(started_apps, 1, Config).

%%--------------------------------------------------------------------
set_env(App, FromEnv) ->
    [ok = application:set_env(App, K, V) || {K, V} <- FromEnv].

%%--------------------------------------------------------------------
%% First try to get the app config from Config, failing which get it from
%% ct:get_config().
get_env_val(App, Config) ->
    case pv(App, Config) of
        undefined ->
            ct:get_config(App);
        Env ->
            Env
    end.

%%--------------------------------------------------------------------
deregister_ids(RegPLs) ->
    [deregister_id(RegPL) || RegPL <- RegPLs].

%%--------------------------------------------------------------------
deregister_id(RegPL) ->
    ct:pal("Deregistering ~p", [RegPL]),
    Service = req_val(service, RegPL),
    Token = req_val(token, RegPL),
    ID = sc_push_reg_api:make_id(Service, Token),
    ok = sc_push:deregister_ids([ID]),
    ct:pal("Deregistered IDs ~p", [ID]).

%%====================================================================
%% Lager support
%%====================================================================
lager_config(Config, RawLagerConfig) ->
    PrivDir = req_val(priv_dir, Config), % Standard CT variable

    Replacements = [
                    {error_log_file, filename:join(PrivDir, "error.log")},
                    {console_log_file, filename:join(PrivDir, "console.log")},
                    {crash_log_file, filename:join(PrivDir, "crash.log")}
                   ],

    replace_template_vars(RawLagerConfig, Replacements).

replace_template_vars(RawLagerConfig, Replacements) ->
    Ctx = dict:from_list(Replacements),
    Str = lists:flatten(io_lib:fwrite("~1000p~s", [RawLagerConfig, "."])),
    SConfig = mustache:render(Str, Ctx),
    {ok, Tokens, _} = erl_scan:string(SConfig),
    {ok, LagerConfig} = erl_parse:parse_term(Tokens),
    LagerConfig.

%%====================================================================
%% General helper functions
%%====================================================================
start_per_suite_apps(Config) ->
    Apps = [lager, ssl],
    Lager = lager_config(Config, ct:get_config(lager)),
    set_env(lager, Lager),
    Fun = fun(App, Acc) ->
                  {ok, L} = application:ensure_all_started(App),
                  Acc ++ L
          end,
    StartedApps = lists:foldl(Fun, [], Apps),
    lists:usort(StartedApps).

set_mnesia_dir(Config) ->
    DataDir = req_val(data_dir, Config),
    MnesiaDir = filename:join(DataDir, "db"),
    ok = filelib:ensure_dir(MnesiaDir),
    ok = application:set_env(mnesia, dir, MnesiaDir).

uuid_to_str(UUID) ->
    uuid:uuid_to_string(UUID, binary_standard).

get_service_properties(Service, [_|_]=RegPLs) when is_atom(Service) ->
    case [PL || PL <- RegPLs, req_val(service, PL) =:= Service] of
        [RegPL] ->
            RegPL;
        [] ->
            ct:fail({cannot_find_service, Service});
        L ->
            ct:fail({unexpected_response, L})
    end.

%%--------------------------------------------------------------------
db_create(Config) ->
    DBInfo = req_val(dbinfo, Config),
    DB = maps:get(db, DBInfo),
    db_create(DB, DBInfo, Config).

db_create(mnesia, _DBInfo, Config) ->
    PrivDir = req_val(priv_dir, Config), % Standard CT variable
    MnesiaDir = filename:join(PrivDir, "mnesia"),
    ok = application:set_env(mnesia, dir, MnesiaDir),
    db_destroy(mnesia, _DBInfo, Config),
    ok = mnesia:create_schema([node()]);
db_create(DB, DBInfo, Config) ->
    db_create(mnesia, DBInfo, Config),
    clear_external_db(DB, DBInfo, Config).

%%--------------------------------------------------------------------
db_destroy(Config) ->
    DBInfo = req_val(dbinfo, Config),
    DB = maps:get(db, DBInfo),
    db_destroy(DB, DBInfo, Config).

db_destroy(mnesia, _DBInfo, _Config) ->
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]);
db_destroy(DB, DBInfo, Config) ->
    db_destroy(mnesia, DBInfo, Config),
    clear_external_db(DB, DBInfo, Config).

%%--------------------------------------------------------------------
clear_external_db(postgres=EDB, _DBInfo, Config) ->
    ConnParams = db_conn_params(postgres, Config),
    Tables = ["scpf.push_tokens"],
    case epgsql:connect(ConnParams) of
        {ok, Conn} ->
            lists:foreach(
              fun(Table) ->
                      {ok, _} = epgsql:squery(Conn, "delete from " ++ Table)
              end, Tables),
            ok = epgsql:close(Conn);
        Error ->
            ct:fail("~p error: ~p", [EDB, Error])
    end.

%%--------------------------------------------------------------------
check_db_availability(Config) ->
    DBInfo = req_val(dbinfo, Config),
    DB = maps:get(db, DBInfo),
    check_db_availability(DB, Config).

check_db_availability(mnesia, _Config) ->
    ok;
check_db_availability(postgres, Config) ->
    ConnParams = db_conn_params(postgres, Config),
    try epgsql:connect(ConnParams) of
        {ok, Conn} ->
            ok = epgsql:close(Conn);
        Error ->
            Error
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
db_pools(#{db := DB, mod := DBMod}, Config) ->
    [
     {sc_push_reg_pool, % name
      [ % sizeargs
       {size, 10},
       {max_overflow, 20}
      ],
      [ % workerargs
       {db_mod, DBMod},
       {db_config, db_config(DB, Config)}
      ]}
    ].

%%--------------------------------------------------------------------
db_config(DB, Config) ->
    maps:get(DB, req_val(connect_info, Config), []).

%%--------------------------------------------------------------------
db_conn_params(DB, Config) ->
    case db_config(DB, Config) of
        #{connection := ConnParams} ->
            ConnParams;
        Val ->
            Val
    end.

%%--------------------------------------------------------------------
set_all_env(App, Props) ->
    lists:foreach(fun({K, V}) ->
                          ok = application:set_env(App, K, V)
                  end, Props).

%%--------------------------------------------------------------------
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

