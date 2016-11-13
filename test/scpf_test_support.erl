-module(scpf_test_support).

-include_lib("common_test/include/ct.hrl").

-export([
         add_props/2,
         check_parsed_resp/1,
         fix_all_cert_paths/2,
         fix_simulator_cert_paths/2,
         get_sim_config/2,
         get_svc_config/2,
         is_uuid/1,
         make_aps_props/1,
         make_aps_props/2,
         make_aps_props/3,
         make_sim_notification/2,
         multi_store/2,
         new_prop/1,
         new_prop/2,
         props_to_aps_json/1,
         pv/2,
         pv/3,
         reason_list/0,
         req_val/2,
         start_gcm_sim/3,
         start_apnsv3_sim/3,
         to_bin_prop/2
        ]).

-define(assertMsg(Cond, Fmt, Args),
        case (Cond) of
        true ->
            ok;
        false ->
            ct:fail("Assertion failed: ~p~n" ++ Fmt, [??Cond] ++ Args)
    end
       ).

-define(assert(Cond), ?assertMsg((Cond), "", [])).

%%====================================================================
%% API
%%====================================================================
add_props(FromProps, ToProps) ->
    lists:foldl(fun(KV, Acc) -> add_prop(KV, Acc) end, ToProps, FromProps).

%%--------------------------------------------------------------------
check_parsed_resp(ParsedResp) ->
    Keys = [id, status, status_desc, reason, reason_desc, body],
    ok = assert_keys_present(Keys, ParsedResp),
    case {req_val(status, ParsedResp), req_val(reason, ParsedResp)} of
        {<<"410">>, <<"BadDeviceToken">>} ->
            ok = assert_keys_present([timestamp, timestamp_desc], ParsedResp);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
fix_all_cert_paths(DataDir, Sessions) ->
    KeyCfg = {config, ssl_opts},
    [fix_cert_paths(KeyCfg, DataDir, Session) || Session <- Sessions].

%%--------------------------------------------------------------------
fix_simulator_cert_paths(DataDir, SimConfig) ->
    SslOptsKey = ssl_options,
    SslOpts0 = req_val(SslOptsKey, SimConfig),
    SslOpts = fix_ssl_opts(SslOpts0, DataDir),
    replace_kv(SslOptsKey, SslOpts, SimConfig).

%%--------------------------------------------------------------------
for_all_sessions(Fun, Sessions) when is_function(Fun, 1), is_list(Sessions) ->
    _ = [Fun(Session) || Session <- Sessions],
    ok.

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
-spec get_svc_config(Key, Config) -> Result when
      Key :: atom(), Config :: proplists:proplist(),
      Result :: proplists:proplist().

%%--------------------------------------------------------------------
get_svc_config(apns_erlv3=Svc, Config) ->
    DataDir = ?config(data_dir, Config),
    Cfg = ct:get_config(Svc),
    Sessions = fix_all_cert_paths(DataDir, req_val(sessions, Cfg)),
    ct:pal("Adjusted apns_erlv3 sessions: ~p", [Sessions]),
    lists:keystore(sessions, 1, Cfg, {sessions, Sessions});
get_svc_config(Svc, Config) ->
    get_env_val(Svc, Config).

%%--------------------------------------------------------------------
get_sim_config(apnsv3_sim_config=Name, Config) ->
    DataDir = ?config(data_dir, Config),
    SimName = ct:get_config(apnsv3_sim_node),
    SimCfg = ct:get_config(Name),
    {SimName, fix_simulator_cert_paths(DataDir, SimCfg)};
get_sim_config(gcm_sim_config=Name, _Config) ->
    SimName = ct:get_config(gcm_sim_node),
    SimCfg = ct:get_config(Name),
    {SimName, SimCfg}.


%%--------------------------------------------------------------------
is_uuid(X) ->
    %% UUID should be 8-4-4-4-12
    match =:= re:run(X,
                     "^[[:xdigit:]]{8}(-[[:xdigit:]]{4}){3}-[[:xdigit:]]{12}$",
                     [{capture, none}]).

%%--------------------------------------------------------------------
make_aps_props(Alert) ->
    new_prop(to_bin_prop(alert, Alert)).

%%--------------------------------------------------------------------
make_aps_props(Alert, Badge) when is_integer(Badge) ->
    add_prop({badge, Badge}, make_aps_props(Alert)).

%%--------------------------------------------------------------------
make_aps_props(Alert, Badge, Sound) when is_integer(Badge) ->
    add_prop(to_bin_prop(sound, Sound), make_aps_props(Alert, Badge)).

%%--------------------------------------------------------------------
make_sim_notification(Notification, SimCfg) ->
    APS0 = pv(aps, Notification, []),
    Extra0 = pv(extra, APS0, []),
    Extra = lists:keystore(sim_cfg, 1, Extra0, {sim_cfg, SimCfg}),
    APS = lists:keystore(extra, 1, APS0, {extra, Extra}),
    lists:keystore(aps, 1, Notification, {aps, APS}).

%%--------------------------------------------------------------------
multi_store(Props, PropsToStore) ->
    lists:ukeysort(1, lists:keymerge(1, lists:keysort(1, PropsToStore),

                                     lists:keysort(1, Props))).
%%--------------------------------------------------------------------
new_prop({_, _} = KV) ->
    [KV].

%%--------------------------------------------------------------------
new_prop(K, V) ->
    new_prop({K, V}).

%%--------------------------------------------------------------------
props_to_aps_json(ApsProps) when is_list(ApsProps) ->
    jsx:encode([{aps, ApsProps}]).

%%--------------------------------------------------------------------
reason_list() ->
    [
     <<"BadCertificate">>,
     <<"BadCertificateEnvironment">>,
     <<"BadDeviceToken">>,
     <<"BadExpirationDate">>,
     <<"BadMessageId">>,
     <<"BadPath">>,
     <<"BadPriority">>,
     <<"BadTopic">>,
     <<"DeviceTokenNotForTopic">>,
     <<"DuplicateHeaders">>,
     <<"Forbidden">>,
     <<"IdleTimeout">>,
     <<"InternalServerError">>,
     <<"MethodNotAllowed">>,
     <<"MissingDeviceToken">>,
     <<"MissingTopic">>,
     <<"PayloadEmpty">>,
     <<"PayloadTooLarge">>,
     <<"ServiceUnavailable">>,
     <<"Shutdown">>,
     <<"TooManyRequests">>,
     <<"TopicDisallowed">>,
     <<"Unregistered">>
    ].

%%--------------------------------------------------------------------
req_val(Key, Config) when is_list(Config) ->
    V = pv(Key, Config),
    ?assertMsg(V =/= undefined, "Required key missing: ~p", [Key]),
    V.

%%--------------------------------------------------------------------
pv(Key, PL) ->
    pv(Key, PL, undefined).

%%--------------------------------------------------------------------
pv(Key, PL, DefVal) ->
    case lists:keyfind(Key, 1, PL) of
        false -> DefVal;
        {_, V} -> V
    end.

%%--------------------------------------------------------------------
%% SimConfig = [{ssl_options, []},{ssl_true}].
start_apnsv3_sim(Name, SimConfig, Cookie) ->
    %% Get important code paths
    CodePaths = [Path || Path <- code:get_path(),
                         string:rstr(Path, "_build/") > 0],
    %{ok, GenResultPL} = generate_sys_config(chatterbox, SimConfig, PrivDir),
    ct:pal("Starting apnsv3 sim node named ~p", [Name]),
    {ok, Node} = start_slave(Name, ["-setcookie " ++ Cookie]),
    ct:pal("Sim node name: ~p", [Node]),
    ct:pal("Setting simulator configuration"),
    ct:pal("~p", [SimConfig]),
    _ = [ok = rpc:call(Node, application, set_env,
                       [chatterbox, K, V], 1000) || {K, V} <- SimConfig],

    SimCBEnv = rpc:call(Node, application, get_all_env, [chatterbox], 1000),
    ct:pal("Sim's chatterbox environment: ~p", [SimCBEnv]),
    ct:pal("Adding code paths to node ~p", [Node]),
    ct:pal("~p", [CodePaths]),
    ok = rpc:call(Node, code, add_pathsz, [CodePaths], 1000),

    ct:pal("Starting apns_erl_sim application on ~p", [Node]),
    {ok, L} = rpc:call(Node, application, ensure_all_started,
                       [apns_erl_sim], 5000),

    ct:pal("Simulator on ~p: Started ~p", [Node, L]),
    ct:pal("Waiting for simulator ~p to accept connections", [Node]),
    SslOptions = req_val(ssl_options, SimConfig),
    ok = wait_for_sim(SslOptions, 5000),
    {ok, L}.

%%--------------------------------------------------------------------
start_gcm_sim(Name, SimConfig, Cookie) ->
    %% Get important code paths
    CodePaths = [Path || Path <- code:get_path(),
                         string:rstr(Path, "_build/") > 0],
    %{ok, GenResultPL} = generate_sys_config(chatterbox, SimConfig, PrivDir),
    ct:pal("Starting sim node named ~p", [Name]),
    {ok, Node} = start_slave(Name, ["-setcookie " ++ Cookie]),
    ct:pal("Sim node name: ~p", [Node]),
    ct:pal("Setting simulator configuration"),
    ct:pal("~p", [SimConfig]),
    _ = [ok = rpc:call(Node, application, set_env,
                       [gcm_sim, K, V], 1000) || {K, V} <- SimConfig],

    SimEnv = rpc:call(Node, application, get_all_env, [gcm_erl], 1000),
    ct:pal("gcm_sim's environment: ~p", [SimEnv]),
    ct:pal("Adding code paths to node ~p", [Node]),
    ct:pal("~p", [CodePaths]),
    ok = rpc:call(Node, code, add_pathsz, [CodePaths], 1000),

    ct:pal("Starting gcm_sim application on ~p", [Node]),
    {ok, L} = rpc:call(Node, application, ensure_all_started, [gcm_sim], 5000),

    ct:pal("Simulator on ~p: Started ~p", [Node, L]),
    ct:pal("Waiting for simulator ~p to accept connections", [Node]),
    TcpOptions = req_val(wm_config, SimConfig),
    ok = wait_for_sim(TcpOptions, 5000),
    {ok, L}.

%%--------------------------------------------------------------------
wait_for_sim(TcpOptions, Timeout) ->
    Ref = erlang:send_after(Timeout, self(), {sim_timeout, self()}),
    Addr = req_val(ip, TcpOptions),
    Port = req_val(port, TcpOptions),
    wait_sim_loop(Addr, Port),
    (catch erlang:cancel_timer(Ref)),
    ok.

%%--------------------------------------------------------------------
wait_sim_loop(Addr, Port) ->
    Self = self(),
    ct:pal("wait_sim_loop: connecting to {~p, ~p}", [Addr, Port]),
    case gen_tcp:connect(Addr, Port, [], 100) of
        {ok, Socket} ->
            ct:pal("wait_sim_loop: Success opening socket to {~p, ~p}",
                   [Addr, Port]),
            ok = gen_tcp:close(Socket);
        {error, Reason} ->
            ct:pal("wait_sim_loop: failed connecting to {~p, ~p}: ~p",
                   [Addr, Port, gen_tcp:format_error(Reason)]),
            receive
                {sim_timeout, Self} ->
                    ct:pal("wait_sim_loop: timed out connecting to "
                           "{~p, ~p}", [Addr, Port]),
                    throw(sim_ping_timeout)
            after
                1000 ->
                    wait_sim_loop(Addr, Port)
            end
    end.

%%--------------------------------------------------------------------
to_bin_prop(K, V) ->
    {sc_util:to_atom(K), sc_util:to_bin(V)}.

%%%====================================================================
%%% Internal functions
%%%====================================================================
%%--------------------------------------------------------------------
stop_simulator(StartedApps) ->
    [ok = application:stop(App) || App <- StartedApps].

%%--------------------------------------------------------------------
generate_sys_config(AppName, AppConfig, PrivDir) ->
    Config = {AppName, AppConfig},
    ensure_dir(filename:join(PrivDir, "sim_config")),
    Filename = filename:join(PrivDir, "sim_config", "sys.config"),
    SysConfig = lists:flatten(io_lib:format("~p~n", [Config])),
    ok = file:write_file(Filename, SysConfig),
    {ok, [{config_file, Filename},
          {contents, SysConfig}]}.

%%--------------------------------------------------------------------
get_saved_req_val(K, Config, Def) ->
    case ?config(saved_config, Config) of
        undefined ->
            pv(K, Config, Def);
        {Saved, OldConfig} ->
            ct:pal("Got config saved by ~p", [Saved]),
            pv(K, OldConfig, Def)
    end.

%%--------------------------------------------------------------------
fix_cert_paths({ConfigKey, SslOptsKey}, DataDir, Session) ->
    Config = req_val(ConfigKey, Session),
    SslOpts = fix_ssl_opts(req_val(SslOptsKey, Config), DataDir),
    replace_kv(ConfigKey,
               replace_kv(SslOptsKey, SslOpts, Config),
               Session).


%%--------------------------------------------------------------------
fix_ssl_opts(SslOpts, DataDir) ->
    OptCaCertKV = fix_opt_kv(cacertfile, SslOpts, DataDir),
    CertFilePath = fix_path(certfile, SslOpts, DataDir),
    KeyFilePath = fix_path(keyfile, SslOpts, DataDir),
    PartialOpts = delete_keys([cacertfile, certfile, keyfile], SslOpts),
    OptCaCertKV ++ [{certfile, CertFilePath},
                    {keyfile, KeyFilePath} | PartialOpts].


%%--------------------------------------------------------------------
fix_path(Key, PL, DataDir) ->
    filename:join(DataDir, req_val(Key, PL)).


%%--------------------------------------------------------------------
fix_opt_kv(Key, PL, DataDir) ->
    case pv(Key, PL) of
        undefined ->
            [];
        Val ->
            [{Key, filename:join(DataDir, Val)}]
    end.

%%--------------------------------------------------------------------
replace_kv(Key, Val, PL) ->
    [{Key, Val} | delete_key(Key, PL)].

%%--------------------------------------------------------------------
add_prop({K, _V} = KV, Props) ->
    lists:keystore(K, 1, Props, KV).

%%--------------------------------------------------------------------
delete_key(Key, PL) ->
    lists:keydelete(Key, 1, PL).

%%--------------------------------------------------------------------
delete_keys(Keys, PL) ->
    lists:foldl(fun(Key, Acc) -> delete_key(Key, Acc) end, PL, Keys).

%%--------------------------------------------------------------------
ensure_dir(Dirname) ->
    case file:make_dir(Dirname) of
        X when X == {error, eexist} orelse X == ok ->
            ok; % Fine if it's there
        {error, POSIX} ->
            Errmsg = file:format_error({?LINE, ?MODULE, POSIX}),
            ct:fail("Error (~p) trying to create ~d: ~s",
                    [POSIX, Dirname, Errmsg]),
            {error, Errmsg}
    end.

%%--------------------------------------------------------------------
ensure_stopped(App) when is_atom(App) ->
    case application:stop(App) of
        ok ->
            ok;
        {error, {not_started, App}} ->
            ok;
        {error, Error} ->
            throw(Error)
    end.

%%--------------------------------------------------------------------
check_parsed_resp_timestamp(ParsedResp) ->
    Keys = [
            timestamp, timestamp_desc],
    _ = [{'value', _} = lists:keysearch(Key, 1, ParsedResp) || Key <- Keys].

%%--------------------------------------------------------------------
assert_keys_present(Keys, PL) ->
    _ = [req_val(Key, PL) || Key <- Keys],
    ok.

%%====================================================================
%% Slave node support
%%====================================================================
start_slave(Name, Args) ->
    {ok, Host} = inet:gethostname(),
    slave:start(Host, Name, Args).

session_info(SessCfg, StartedSessions) ->
    Name = req_val(name, SessCfg),
    {Pid, Ref} = req_val(Name, StartedSessions),
    {Name, Pid, Ref}.


