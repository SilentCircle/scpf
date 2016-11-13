-module(gendocs).

main(_) ->
    Apps = ["apns_erl", "apns_erl_util", "gcm_erl", "sc_push", "sc_push_lib",
            "sc_util"],
    Opts = edoc_opts(),
    SourceFiles = filelib:wildcard("src/*.erl") ++ get_sources(Apps, "deps"),
    case edoc:run([], SourceFiles, Opts) of
        ok ->
            halt(0);
        Res ->
            io:format("~p~n", [Res]),
            halt(1)
    end.

get_sources(Apps, BaseDir) ->
    DirList = "{" ++ string:join(Apps, ",") ++ "}",
    filelib:wildcard(filename:join([BaseDir, DirList, "src", "*.erl"])).

edoc_opts() ->
    {ok, Cwd} = file:get_cwd(),
    DocDir = filename:join(Cwd, "doc/api"),
    [
        {new, true},
        {dir, DocDir},
        {packages, false},
        {subpackages, true},
        {source_path, ["src", "deps/sc_push/src"]},
        {pretty_printer, erl_pp}
    ].
