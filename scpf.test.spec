%%====================================================================
%% Common Test Test Spec
%%====================================================================

%%--------------------------------------------------------------------
%% Config Settings
%%--------------------------------------------------------------------

%% {node, NodeAlias, Node}.

%% {init, InitOptions}.
%% {init, [NodeAlias], InitOptions}.

%% {label, Label}.
%% {label, NodeRefs, Label}.

%% {multiply_timetraps, N}.
%% {multiply_timetraps, NodeRefs, N}.

%% {scale_timetraps, Bool}.
%% {scale_timetraps, NodeRefs, Bool}.

%% NOTE: rebar overrides {cover, ...} by looking for a file ending in cover.spec

%% {cover, CoverSpecFile}.
%% {cover, NodeRefs, CoverSpecFile}.

%% {include, IncludeDirs}.
%% {include, NodeRefs, IncludeDirs}.

{config, ["test/test.config"]}.

%% {config, NodeRefs, ConfigFiles}.

%% {userconfig, {CallbackModule, ConfigStrings}}.
%% {userconfig, NodeRefs, {CallbackModule, ConfigStrings}}.

%% {alias, DirAlias, Dir}.

%% {merge_tests, Bool}.

%% NOTE: rebar.config ct_log_dir overrides {logdir, ...}

%% {logdir, LogDir}.
%% {logdir, NodeRefs, LogDir}.

%% {create_priv_dir, PrivDirOption}.
%% {create_priv_dir, NodeRefs, PrivDirOption}.

%% {event_handler, EventHandlers}.
%% {event_handler, NodeRefs, EventHandlers}.
%% {event_handler, EventHandlers, InitArgs}.
%% {event_handler, NodeRefs, EventHandlers, InitArgs}.

%% {ct_hooks, CTHModules}.
%% {ct_hooks, NodeRefs, CTHModules}.

%% {enable_builtin_hooks, Bool}.

%%--------------------------------------------------------------------
%% Test terms
%%--------------------------------------------------------------------

{suites, "./test", all}.

%% {suites, NodeRefs, DirRef, Suites}.

%% {groups, DirRef, Suite, Groups}.
%% {groups, NodeRefsDirRef, Suite, Groups}.

%% {groups, DirRef, Suite, GroupSpec, {cases,Cases}}.
%% {groups, NodeRefsDirRef, Suite, GroupSpec, {cases,Cases}}.

%% {cases, DirRef, Suite, Cases}.
%% {cases, NodeRefs, DirRef, Suite, Cases}.

%% {skip_suites, DirRef, Suites, Comment}.
%% {skip_suites, NodeRefs, DirRef, Suites, Comment}.

%% {skip_groups, DirRef, Suite, GroupNames, Comment}.
%% {skip_groups, NodeRefs, DirRef, Suite, GroupNames, Comment}.

%% {skip_cases, DirRef, Suite, Cases, Comment}.
%% {skip_cases, NodeRefs, DirRef, Suite, Cases, Comment}.
