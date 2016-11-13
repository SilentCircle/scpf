%%
%%

-module(mnesia_init).
-mode(compile).
-compile(export_all).

-define(TIMEOUT, 300000).

main([]) ->
    main(default_node_and_dirname());
main([S]) when S =:= "-h"; S =:= "--help" ->
    usage();
main([SMnesiaNode, SMnesiaDir]) ->
    main(["-name", SMnesiaNode, SMnesiaDir]);
main([NameTypeArg, SMnesiaNode, SMnesiaDir]) ->
    RC = try
        run(NameTypeArg, SMnesiaNode, SMnesiaDir),
        0
    catch X:Y ->
        err_msg("***** ~p:~n~p~n", [X, Y]),
        err_msg("~p~n", [erlang:get_stacktrace()]),
        1
    end,
    RC;
main(_) ->
    usage().


run(NameTypeArg, SMnesiaNode, SMnesiaDir) ->
    Node = list_to_atom(SMnesiaNode),
    start_distribution(Node, NameTypeArg),
    application:set_env(mnesia, dir, SMnesiaDir),
    ok = filelib:ensure_dir(SMnesiaDir),
    create_schema(Node, SMnesiaDir).

%%--------------------------------------------------------------------
%% usage
%%--------------------------------------------------------------------
create_schema(Node, Dir) ->
    case mnesia:create_schema([Node]) of
        ok ->
            err_msg("Created disk schema for ~p in directory ~s~n",
                    [Node, Dir]),
            show_info(),
            0;
        {error, {Node, {already_exists, Node}}} ->
            err_msg("Schema for node ~p already exists in directory ~s~n",
                    [Node, Dir]),
            show_info(),
            0;
        Error ->
            err_msg("Error creating schema for node ~p: ~p~n", [Node, Error]),
            1
    end.

%%--------------------------------------------------------------------
%% usage
%%--------------------------------------------------------------------
usage() ->
    ScriptName = filename:basename(escript:script_name()),
    [SNode, SDir] = default_node_and_dirname(),
    err_msg("~s [equivalent to ~s -name ~s ~s]\n",
            [ScriptName, ScriptName, SNode, SDir]),
    err_msg("~s -sname|-name mnesia-node-name mnesia-dir\n", [ScriptName]),
    halt(1).

%%--------------------------------------------------------------------
%% err_msg
%%--------------------------------------------------------------------
err_msg(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

start_distribution(ThisNode, NameTypeArg) ->
    case net_kernel:start([ThisNode, get_name_type(NameTypeArg)]) of
        {ok, _Pid} ->
            err_msg("Started node ~p as a distributed node~n", [ThisNode]);
        {error, {already_started, Pid}} ->
            err_msg("Node ~p (~p) was already started as a distributed node~n",
                    [ThisNode, Pid]);
        {error, Reason} ->
            err_msg("Cannot start this node (~p) as a distributed node,"
                    " reason:~n~p~n", [ThisNode, Reason]),
            throw(Reason)
    end.

%% get name type from arg
get_name_type(NameTypeArg) ->
	case NameTypeArg of
		"-sname" ->
			shortnames;
		_ ->
			longnames
	end.

show_info() ->
    ok = mnesia:start(),
    show_tables(mnesia:system_info(tables)),
    mnesia:stop().

show_tables(TabList) ->
    err_msg("Tables:~n", []),
    [err_msg("  ~p~n", [Tab]) || Tab <- TabList],
    ok.

default_node_and_dirname() ->
    SMnesiaNode = atom_to_list(node()),
    SMnesiaDir = "Mnesia." ++ SMnesiaNode,
    [SMnesiaNode, SMnesiaDir].

%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
