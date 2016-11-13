%%
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

-module(backup_database).
-compile(export_all).
-mode(compile).

-define(TIMEOUT, 60000).
-define(INFO(Fmt, Args), io:format(Fmt,Args)).

main([NodeName, Cookie, BackupFile]) ->
    TargetNode = start_distribution(NodeName, Cookie),
    ok = rpc:call(TargetNode, mnesia, backup, [BackupFile], ?TIMEOUT),
    ?INFO("Backed up ~p tables to ~p~n", [NodeName, BackupFile]),
    halt(0);
main([BackupFile, "list"]) ->
    Acc = list_backup_tables(BackupFile),
    show_table_backup_info(Acc),
    halt(0);
main(_) ->
    usage().

usage() ->
    ScriptName = filename:basename(escript:script_name()),
    ?INFO("usage: ~s node-name cookie backup-file-name~n"
          "       ~s backup-file-name 'list'~n",
          [ScriptName, ScriptName]),
    halt(1).

start_distribution(NodeName, Cookie) ->
    MyNode = make_script_node(NodeName),
    {ok, _Pid} = net_kernel:start([MyNode, longnames]),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    TargetNode = make_target_node(NodeName),
    case {net_kernel:hidden_connect_node(TargetNode),
          net_adm:ping(TargetNode)} of
        {true, pong} ->
            ok;
        {_, pang} ->
            ?INFO("Node ~p not responding to pings.\n", [TargetNode]),
            init:stop(1)
    end,
    TargetNode.

make_target_node(Node) ->
    list_to_atom(Node).

make_script_node(Node0) ->
    [Node, Host] = string:tokens(Node0, "@"),
    list_to_atom(lists:concat([Node, "_backuptask_", os:getpid(), "@", Host])).

traverser({schema, Tab, _L}, {Cs, Ds, Rs}) ->
    {[], {[Tab|Cs], Ds, Rs}};
traverser({schema, Tab}, {Cs, Ds, Rs}) ->
    {[], {Cs, [Tab|Ds], Rs}};
traverser(Rec, {Cs, Ds, Rs}) when is_tuple(Rec) ->
    Tab = element(1, Rec),
    {[], {Cs, Ds, dict:update_counter(Tab, 1, Rs)}};
traverser(_, Acc) ->
    {[], Acc}.

list_backup_tables(BackupFile) ->
    Acc0 = {[], [], dict:new()},
    {ok, LastAcc} = mnesia:traverse_backup(BackupFile, mnesia_backup, dummy,
                                           read_only, fun traverser/2, Acc0),
    LastAcc.

show_table_backup_info({Cs, Ds, Rs}) ->
    show_table_list("Tables created", Cs),
    show_table_list("Tables deleted", Ds),
    show_record_stats(Rs).

show_table_list(Msg, L) ->
    ?INFO("~s:~n~s~n",
          [Msg, string:join([atom_to_list(C) || C <- L], "\n")]).

show_record_stats(Rs) ->
    ?INFO("Record info:~n~s~n", [rs_to_str(Rs)]).

rs_to_str(Rs) ->
    [[atom_to_list(K), " ", integer_to_list(V), "\n"]
     || {K, V} <- dict:to_list(Rs)].
