#!/bin/bash

# Try to figure out SCPF version

GIT=$(which git)

find_version() {
    local vsn=$(date --utc +"%Y%m%dT%H%M%SZ")

    if [[ -r APP_VERSION ]]; then
        vsn=$(cat APP_VERSION)
    elif [[ -n $GIT ]] && [[ -d ./.git ]]; then
        vsn=$($GIT describe --always --long)
    fi

    echo $vsn
}

find_version
