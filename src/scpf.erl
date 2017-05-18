%%%-------------------------------------------------------------------
%%% @author Edwin Fine
%%% @copyright (C) 2017, Silent Circle LLC
%%% @doc
%%%
%%% @end
%%% Created : 2017-05-11 16:38:00.260089
%%%-------------------------------------------------------------------
-module(scpf).
-export([
         info/0,
         info/1,
         vm_stats_entries/0,
         keylist_to_binary/1
        ]).

%%%===================================================================
%%%  API
%%%===================================================================
info() ->
    Paths = [Path || {Path, _, Status} <- vm_stats_entries(),
                     Status == enabled],
    maps:from_list([get_value(Path) || Path <- Paths]).

%%--------------------------------------------------------------------
info(json) ->
    EJSON = lists:map(fun({KL, V}) -> {keylist_to_binary(KL), V} end,
                      maps:to_list(info())),
    jsx:encode(EJSON).

%%--------------------------------------------------------------------
vm_stats_entries() ->
    exometer:find_entries([scpf, vm]).

%%--------------------------------------------------------------------
keylist_to_binary([_|_] = KeyList) ->
    list_to_binary(string:join([atom_to_list(A) || A <- KeyList], ".")).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
get_value(Path) ->
    case exometer:get_value(Path) of
        {ok, Value} ->
            {Path, Value};
        _ ->
            {Path, undefined}
    end.

