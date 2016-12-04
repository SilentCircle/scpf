#!/usr/bin/escript

%%====================================================================
%% Push a message to a token/appid combination.
%%====================================================================

%% Internal exports
main([SNode, SCookie, SAppId, SToken, SMsg]) ->
    RC = try
        run(SNode, SCookie, SAppId, SToken, SMsg)
    catch X:Y ->
        err_msg("***** ~p:~n~p~n", [X, Y]),
        err_msg("~p~n", [erlang:get_stacktrace()]),
        1
    end,
    halt(RC);

main(_) ->
    usage().

run(SNode, SCookie, SAppId, SToken, SMsg) ->
    Node = list_to_atom(SNode),
    connect_to_node(Node, list_to_atom(SCookie)),
    remote_push(Node, SMsg, SToken, SAppId).

connect_to_node(Node, Cookie) ->
    start_distributed(derive_node_name(Node)),
    erlang:set_cookie(node(), Cookie),
    net_kernel:hidden_connect_node(Node).

remote_push(Node, Msg, Tok, AppId) when is_list(Msg),
                                        is_list(Tok),
                                        is_list(AppId) ->
    Notification = [
        {alert, list_to_binary(Msg)},
        {sound,<<"something.wav">>}
    ],
    Result = do_rpc(Node, apns_erl_session_sup,
                    get_child_pid, [list_to_atom(AppId)]),
    case Result of
        undefined ->
            err_msg("Unknown app id ~p~n", [AppId]),
            1;
        Fsm when is_pid(Fsm) ->
            Token = do_rpc(Node, sc_util, hex_to_bitstring, [Tok]),
            JSON = do_rpc(Node, apns_json, make_notification, [Notification]),
            SendRes = do_rpc(Node, apns_erl_session, send, [Fsm, Token, JSON]),
            case SendRes of
                {ok, RefNo} ->
                    err_msg("Sent msg, refno = ~B~n", [RefNo]),
                    0;
                Error ->
                    err_msg("Failed to send msg: ~p~n", [Error]),
                    1
            end
    end.

%%--------------------------------------------------------------------
%% usage
%%--------------------------------------------------------------------
usage() ->
    err_msg("usage: ~s scpf-node cookie app-id apns-token message~n",
            [escript:script_name()]),
    halt(1).

%%--------------------------------------------------------------------
%% err_msg
%%--------------------------------------------------------------------
err_msg(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

%%--------------------------------------------------------------------
%%% Perform an RPC call and throw on error
%%%--------------------------------------------------------------------
do_rpc(Node, M, F, A) ->
    try rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            err_msg("RPC Error: ~p~n", [Reason]),
            throw({rpcerror, {Reason, {Node, M, F, A}}});
        Result ->
            Result
        catch _:Why ->
            throw(Why)
    end.

start_distributed(ThisNode) ->
    case net_kernel:start([ThisNode, longnames]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            err_msg("Cannot start this node (~p) as a distributed node,"
                    " reason:~n~p~n", [ThisNode, Reason]),
            throw(Reason)
    end.

nodehost(Node) when is_atom(Node) ->
    nodehost(atom_to_list(Node));
nodehost(SNode) when is_list(SNode) ->
    [_Name, Host] = string:tokens(SNode, "@"),
    Host.

derive_node_name(Node) ->
    list_to_atom(atom_to_list(?MODULE) ++ "@" ++ nodehost(Node)).

