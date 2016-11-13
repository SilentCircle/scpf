#!/bin/bash
MNESIA_DIR="/var/lib/scpf"
SCPF=/usr/sbin/scpf
 
die() {
    echo $* >&2
    exit 1
}
  
dire_warning() {
    local extra_db_nodes="$1"; shift
 
    echo     "Mnesia directory: ${MNESIA_DIR}"
    echo -ne "Extra db nodes  : ${extra_db_nodes}\n\n"
 
    cat <<'END'
  ****************************************************************************
  ***** WARNING!!! *****
  ****************************************************************************
  *****
  ***** This will COMPLETELY DELETE ALL SCPF DATABASE TABLES on this node.
  ***** Are you REALLY sure you want to do this?
  *****
  ***** Press CTRL-C to abort or ENTER to continue:
  ****************************************************************************
END
    read
}
  
check_user() {
    [[ $(id -un) == 'scpf' ]] || die "This must be run as scpf user"
}
  
check_scpf_stopped() {
    ${SCPF} status > /dev/null 2>&1 && die "Please stop scpf first"
}

ping_node() {
    local this_node="$1"; shift
    local node=$1; shift

    erl -name ${this_node} \
        -setcookie $(<${MNESIA_DIR}/.erlang.cookie) \
        -noinput -noshell \
        -kernel inet_dist_listen_min 40000 inet_dist_listen_max 40999 \
        -eval "Res = net_adm:ping('$node'), io:format(\"~p~n\", [Res]), init:stop()."
}

check_connectivity() {
    local this_node="$1"; shift
    local node=$1; shift

    while [[ -n "$node" ]]; do
        echo -n "Pinging $node... "
        [[ $(ping_node $this_node $node) == "pong" ]] || die "Cannot ping $node"
        echo "OK"
        node=$1; shift
    done
}
  
recreate_node() {
    local this_node="$1"; shift
    local extra_db_nodes="$1"; shift
    local dir="\"${MNESIA_DIR}\""
    local node1=$(echo $extra_db_nodes | cut -f1 -d' ')
    rm -f ${MNESIA_DIR}/*

    erl -name ${this_node} \
        -setcookie $(<${MNESIA_DIR}/.erlang.cookie) \
        -noinput -noshell \
        -kernel inet_dist_listen_min 40000 inet_dist_listen_max 40999 \
        -mnesia dir "${dir}" \
        -mnesia extra_db_nodes "${extra_db_nodes}" \
        -s mnesia \
        -s init stop
    echo "RC=$?"
}
  
get_nodes() {
    local node=$1; shift
    local extra_db_nodes='['
 
    while [[ -n "$node" ]]; do
        extra_db_nodes="${extra_db_nodes}'${node}'"
        node=$1; shift
        [[ -n "${node}" ]] && extra_db_nodes="${extra_db_nodes},"
    done
 
    extra_db_nodes="${extra_db_nodes}]"
    echo "${extra_db_nodes}"
}
 
usage() {
    die "usage: $(basename $0) this_erlang_node extra_db_node1 [extra_db_node2...]"
}
 
 
#
# Main
#
[[ $# -ge 2 ]] || usage
 
THIS_NODE="$1"; shift
NODE_ARGS=$*
EXTRA_DB_NODES=$(get_nodes $*)
check_user
check_scpf_stopped
check_connectivity "${THIS_NODE}" ${NODE_ARGS}
dire_warning "${EXTRA_DB_NODES}"
recreate_node "${THIS_NODE}" "${EXTRA_DB_NODES}"
