#!/bin/bash

# Try to figure out SCPF version

GIT=$(which git)
DPKG_PARSE=$(which dpkg-parsechangelog)

file_version() {
    local dsc=$(find .. -maxdepth 1 -type f -name 'scpf_*.dsc' | sort | tail -1)

    if [[ -n $dsc ]]; then
        echo -n $(basename $dsc .dsc | sed 's/^scpf_//')
    fi
}

find_version() {
    # Default to current UTC date/time
    local vsn=$(date -u +"%Y%m%d%H%M%S")

    if [[ -r ./APP_VERSION ]]; then
        vsn=$(< ./APP_VERSION)
    elif [[ -n $GIT ]] && [[ -d ./.git ]]; then
        vsn=$($GIT describe --always --long)
    elif [[ -n $DPKG_PARSE ]] && [[ -r ./debian/changelog ]]; then
        vsn=$($DPKG_PARSE --count 0 | awk '/^Version:/ { print $$2 }')
    else
        local fvsn=$(file_version)
        if [[ -n $fvsn ]]; then
            vsn=$fvsn
        else
            # Last-ditch effort - write current datetime to APP_VERSION file
            echo $vsn > ./APP_VERSION
        fi
    fi

    echo -n $vsn
}

find_version
