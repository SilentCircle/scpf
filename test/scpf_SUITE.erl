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
            {require, apnsv3_sim_config}
           ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    set_mnesia_dir(Config),
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
                         {registrations, Registrations}]
               ).

%%--------------------------------------------------------------------
end_per_suite(Config) ->
    % We don't care about sim_started_apps because they are on
    % a slave node that will get shut down.
    Apps = req_val(suite_started_apps, Config),
    [application:stop(App) || App <- Apps],
    ok.

%%--------------------------------------------------------------------
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
        {
            service,
            [],
            [
                {group, clients}
            ]
        },
        {
            clients,
            [],
            [
                send_msg_test,
                send_msg_fail_test,
                send_msg_no_reg_test
            ]
        }
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
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
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
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
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
lager_config(Config) ->
    PrivDir = req_val(priv_dir, Config), % Standard CT variable
    [
     {handlers,
      [
       {lager_console_backend, debug},
       {lager_file_backend, [{file, filename:join(PrivDir, "log/error.log")},
                             {level, error},
                             {size, 10485760},
                             {date, "$D0"},
                             {count, 5}]},
       {lager_file_backend, [{file, filename:join(PrivDir, "log/console.log")},
                             {level, debug },
                             {size, 10485760},
                             {date, "$D0"},
                             {count, 5}
                            ]
       }
      ]
     },
     %% Whether to write a crash log, and where. Undefined means no crash logger.
     {crash_log, filename:join(PrivDir, "log/crash.log")},
     %% Maximum size in bytes of events in the crash log - defaults to 65536
     {crash_log_msg_size, 65536},
     %% Maximum size of the crash log in bytes, before its rotated, set
     %% to 0 to disable rotation - default is 0
     {crash_log_size, 10485760},
     %% What time to rotate the crash log - default is no time
     %% rotation. See the README for a description of this format.
     {crash_log_date, "$D0"},
     %% Number of rotated crash logs to keep, 0 means keep only the
     %% current one - default is 0
     {crash_log_count, 5},
     %% Whether to redirect error_logger messages into lager - defaults to true
     {error_logger_redirect, true}
    ].

%%====================================================================
%% General helper functions
%%====================================================================
start_per_suite_apps(Config) ->
    Apps = [lager, ssl],
    set_env(lager, lager_config(Config)),
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
