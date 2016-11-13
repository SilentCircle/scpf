%%====================================================================
%% Common Test Test Spec
%%====================================================================

{node, ct1, 'ct1_scpf@sxfcctpavd03.fcci.com'}.
{config, ["_build/test/lib/scpf/test/test.config"]}.
{suites, "_build/test/lib/scpf/test", [scpf_SUITE]}.

%% ex: ft=erlang ts=4 sts=4 sw=4 et:
