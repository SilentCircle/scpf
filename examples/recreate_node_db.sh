#!/bin/bash
die() {
    echo $* >&2
    exit 1
}
  
dire_warning() {
    local app="$1"; shift
    local extra_db_nodes="$1"; shift
 
    echo     "Mnesia directory: ${MNESIA_DIR}"
    echo -ne "Extra db nodes  : ${extra_db_nodes}\n\n"
 
    cat <<END
  ****************************************************************************
  ***** WARNING!!! *****
  ****************************************************************************
  *****
  ***** This will COMPLETELY DELETE ALL DATABASE TABLES in ${MNESIA_DIR}
  ***** and recreate them, possibly replicating from one of these nodes:
  *****
  ***** ${extra_db_nodes}
  *****
  ***** If the nodes are not available, empty tables will be created, and 
  ***** after starting ${app}, if one of the other nodes is accessible,
  ***** the empty tables will be populated.
  *****
  ***** Are you REALLY sure you want to do this?
  *****
  ***** Press CTRL-C to abort or ENTER to continue:
  ****************************************************************************
END
    read
}
  
check_user() {
    local app=$1; shift
    [[ $(id -un) == "${app}" ]] || die "This must be run as username ${app}"
}
  
check_app_stopped() {
    local app=$1; shift
    /usr/sbin/${app} status > /dev/null 2>&1 && die "Please stop ${app} first"
}
  
recreate_node() {
    local this_node="$1"; shift
    local extra_db_nodes="$1"; shift
    local dir="\"${MNESIA_DIR}\""
    rm -f ${MNESIA_DIR}/*
    erl -name ${this_node} \
        -setcookie $(<${MNESIA_DIR}/.erlang.cookie) \
        -noinput -noshell \
        -mnesia dir "${dir}" \
        -mnesia extra_db_nodes "${extra_db_nodes}" \
        -s mnesia \
        -s init stop
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
    die "usage: $(basename $0) appname this_erlang_node extra_db_node1 [extra_db_node2...]"
}
 
 
#
# Main
#
[[ $# -ge 3 ]] || usage

APP=$1; shift
THIS_NODE="$1"; shift
EXTRA_DB_NODES=$(get_nodes $*)

MNESIA_DIR="/var/lib/${APP}"
 
check_user "${APP}"
check_app_stopped "${APP}"
dire_warning "${APP}" "${EXTRA_DB_NODES}"
recreate_node "${THIS_NODE}" "${EXTRA_DB_NODES}"
