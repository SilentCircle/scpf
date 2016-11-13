#!/bin/bash

PROFILE=$1; shift

find _build/${PROFILE:-default}/lib -type d -name ".git" |
while read gd; do
	wd="$(dirname $gd)"
	app="$(basename $wd)"
	printf '%s\t%s\n' $app "$(git --git-dir="$gd" --work-tree="$wd" describe --long --always)"
done | sort

