%%% ==========================================================================
%%% Copyright 2016 Silent Circle
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% ==========================================================================

%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'scpf' module.
%%%-----------------------------------------------------------------

-module(scpf_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(assertMsg(Cond, Fmt, Args),
    case (Cond) of
        true ->
            ok;
        false ->
            ct:fail("Assertion failed: ~p~n" ++ Fmt, [??Cond] ++ Args)
    end
).

-define(assert(Cond), ?assertMsg((Cond), "", [])).
-define(assertThrow(Expr, Class, Reason),
    begin
            ok = (fun() ->
                    try (Expr) of
                        Res ->
                            {unexpected_return, Res}
                    catch
                        C:R ->
                            case {C, R} of
                                {Class, Reason} ->
                                    ok;
                                _ ->
                                    {unexpected_exception, {C, R}}
                            end
                    end
            end)()
    end
).

-define(ALERT_MSG, <<"scpf_SUITE test alert">>).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [
        {timetrap, {seconds, 30}},
        {require, sc_push},
        {require, gcm_erl},
        {require, apns_erl},
        {require, registrations}
    ].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(sasl),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(goldrush),
    ok = application:load(lager),
    set_env(lager, lager_config(Config)),
    ok = application:start(lager),
    ok = ssl:start(),
    Registrations = ct:get_config(registrations),
    ct:pal("Registrations: ~p~n", [Registrations]),
    Services = ct:get_config(services),
    ct:pal("Services: ~p~n", [Services]),
    [{registrations, Registrations},{services, Services} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = ssl:stop(),
    ok = application:stop(lager),
    ok = application:unload(lager),
    code:purge(lager_console_backend), % ct gives error otherwise
    ok = application:stop(sasl),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    init_per_testcase_common(Config).

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_Case, Config) ->
    end_per_testcase_common(Config).

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
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
                send_msg_no_reg_test,
                subscription_test
            ]
        }
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
    [
        {group, service}
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

% t_1(doc) -> ["t/1 should return 0 on an empty list"];
% t_1(suite) -> [];
% t_1(Config) when is_list(Config)  ->
%     ?line 0 = t:foo([]),
%     ok.

send_msg_test(doc) ->
    ["sc_push:send/1 should send a message to all push services"];
send_msg_test(suite) ->
    [];
send_msg_test(Config) ->
    RegPLs = value(registrations, Config),
    ok = sc_push:register_ids(RegPLs),
    [
        begin
            Tag = value(tag, RegPL),
            Notification = [
                    {alert, ?ALERT_MSG},
                    {tag, Tag},
                    {aps, [{badge, 42}]},
                    {gcm, [{collapse_key, true}]},
                    {return, success} % Will only work with sc_push_svc_null!
                    ],
            [{ok, ReqId}] = sc_push:send(Notification),
            ct:pal("Sent notification to ~p, req id = ~p~n", [Tag, ReqId])
        end || RegPL <- RegPLs
    ],
    deregister_ids(RegPLs),
    ok.

send_msg_fail_test(doc) ->
    ["sc_push:send/1 should fail"];
send_msg_fail_test(suite) ->
    [];
send_msg_fail_test(Config) ->
    RegPLs = value(registrations, Config),
    [NullRegPL] = [PL || PL <- RegPLs, proplists:get_value(service, PL) =:= null],
    ok = sc_push:register_id(NullRegPL),
    Tag = value(tag, NullRegPL),
    Notification = [
        {alert, ?ALERT_MSG},
        {tag, Tag},
        {return, {error, forced_test_failure}} % Will only work with sc_push_svc_null!
    ],
    [{error, forced_test_failure}] = sc_push:send(Notification),
    ct:pal("Expected failure sending notification to service ~p, tag ~p~n", [null, Tag]),
    deregister_ids(RegPLs),
    ok.

send_msg_no_reg_test(doc) ->
    ["sc_push:send/1 should fail with tag not found"];
send_msg_no_reg_test(suite) ->
    [];
send_msg_no_reg_test(Config) ->
    RegPLs = value(registrations, Config),
    [NullRegPL] = [PL || PL <- RegPLs, proplists:get_value(service, PL) =:= null],
    ok = sc_push:register_id(NullRegPL),
    FakeTag = <<"$$Completely bogus tag$$">>,
    Notification = [
        {alert, ?ALERT_MSG},
        {tag, FakeTag}
    ],
    [{error, [{reg_not_found_for_tag, FakeTag}]}] = sc_push:send(Notification),
    ct:pal("Got expected error (reg_not_found_for_tag) from send notification~n", []),
    deregister_ids(RegPLs),
    ok.

subscription_test(doc) ->
    ["sc_push:send/3 should send a message to GCM and deliver results to self"];
subscription_test(suite) ->
    [];
subscription_test(Config) ->
    RegPLs = value(registrations, Config),
    ok = sc_push:register_ids(RegPLs),
    [
        begin
            Tag = value(tag, RegPL),
            Notification = [
                    {alert, ?ALERT_MSG},
                    {tag, Tag},
                    {aps, [{badge, 42}]},
                    {gcm, [{collapse_key, true}]},
                    {return, success} % Will only work with sc_push_svc_null!
                    ],
            Opts = [{callback, {self(), [progress, completion]}}],
            [{ok, ReqId}] = sc_push:send(Notification, Opts),
            ct:pal("Sent notification with callbacks to ~p, req id = ~p~n", [Tag, ReqId]),
            subs_receive_loop(ReqId)
        end || RegPL <- RegPLs
    ],
    deregister_ids(RegPLs),
    ok.

subs_receive_loop(Ref) ->
    receive
        {Service, completion, Ref, Status} ->
            ct:log("~p completion: req id ~p -> ~p", [Service, Ref, Status]),
            ok = Status;
        {Service, completion, OtherRef, Status} ->
            ct:log("~p completion: req id ~p -> ~p", [Service, OtherRef, Status]),
            subs_receive_loop(Ref);
        {Service, progress, Ref, Status} ->
            ct:log("~p progress: req id ~p -> ~p", [Service, Ref, Status]),
            subs_receive_loop(Ref);
        {Service, progress, OtherRef, Status} ->
            ct:log("~p progress: req id ~p -> ~p", [Service, OtherRef, Status]),
            subs_receive_loop(Ref)
    after
        3000 ->
            ct:log("Timeout waiting for req id ~p", [Ref])

    end.

%%====================================================================
%% Internal helper functions
%%====================================================================
init_per_testcase_common(Config) ->
    (catch end_per_testcase_common(Config)),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    Apps = [sc_push, gcm_erl, apns_erl],
    set_envs(Apps),
    [ok = start_app_and_its_deps(App) || App <- Apps],
    Config.

end_per_testcase_common(Config) ->
    ok = application:stop(apns_erl),
    ok = application:stop(gcm_erl),
    ok = application:stop(sc_push_lib),
    ok = application:stop(sc_push),
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    Config.

set_envs(Apps) ->
    [set_env(App) || App <- Apps].

set_env(App) ->
    FromEnv = ct:get_config(App),
    set_env(App, FromEnv).

set_env(App, FromEnv) ->
    [ok = application:set_env(App, K, V) || {K, V} <- FromEnv].

deregister_ids(RegPLs) ->
    [deregister_id(RegPL) || RegPL <- RegPLs].

deregister_id(RegPL) ->
    ct:pal("Deregistering ~p~n", [RegPL]),
    Service = value(service, RegPL),
    Token = value(token, RegPL),
    ID = sc_push_reg_api:make_id(Service, Token),
    ok = sc_push:deregister_ids([ID]),
    ct:pal("Deregistered IDs ~p~n", [ID]).

%%====================================================================
%% Lager support
%%====================================================================
lager_config(Config) ->
    PrivDir = value(priv_dir, Config), % Standard CT variable
    [
        %% What handlers to install with what arguments
        {handlers, [
                {lager_console_backend, info},
                {lager_file_backend, [
                        {filename:join(PrivDir, "error.log"), error, 10485760, "$D0", 5},
                        {filename:join(PrivDir, "console.log"), info, 10485760, "$D0", 5}
                    ]
                }
            ]},
        %% Whether to write a crash log, and where. Undefined means no crash logger.
        {crash_log, filename:join(PrivDir, "crash.log")}
    ].

%%====================================================================
%% General helper functions
%%====================================================================
value(Key, Config) when is_list(Config) ->
    V = proplists:get_value(Key, Config),
    ?assertMsg(V =/= undefined, "Required key missing: ~p~n", [Key]),
    V.

maybe_get_pid(X) when is_pid(X) ->
    X;
maybe_get_pid(X) when is_atom(X) ->
    erlang:whereis(X).

start_app_and_its_deps(App) ->
    case application:start(App) of
        ok ->
            ct:log("Started ~p", [App]);
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, DepApp}} ->
            start_app_and_its_deps(DepApp),
            start_app_and_its_deps(App)
    end.

