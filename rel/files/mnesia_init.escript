%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

main([SMnesiaNode, SMnesiaDir]) ->
    RC = try
        run(SMnesiaNode, SMnesiaDir),
        0
    catch X:Y ->
        err_msg("***** ~p:~n~p~n", [X, Y]),
        err_msg("~p~n", [erlang:get_stacktrace()]),
        1
    end,
    halt(RC);

main(_) ->
    usage().


run(SMnesiaNode, SMnesiaDir) ->
    Node = list_to_atom(SMnesiaNode),
    start_distributed(Node),
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
    err_msg("~s mnesia-node-name mnesia-dir~n",
            [escript:script_name()]),
    halt(1).

%%--------------------------------------------------------------------
%% err_msg
%%--------------------------------------------------------------------
err_msg(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

start_distributed(ThisNode) ->
    case net_kernel:start([ThisNode, longnames]) of
        {ok, _Pid} ->
            err_msg("Started node ~p as a distributed node~n", [ThisNode]);
        {error, Reason} ->
            err_msg("Cannot start this node (~p) as a distributed node,"
                    " reason:~n~p~n", [ThisNode, Reason]),
            throw(Reason)
    end.

show_info() ->
    ok = mnesia:start(),
    show_tables(mnesia:system_info(tables)),
    mnesia:stop().

show_tables(TabList) ->
    err_msg("Tables:~n", []),
    [err_msg("  ~p~n", [Tab]) || Tab <- TabList],
    ok.

