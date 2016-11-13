#!/bin/bash

set -e

(( $# == 3 )) || exit 1

NODE_NAME=$1; shift
INPUT_FILENAME=$1; shift
OUTPUT_FILENAME=$1; shift

sed -e 's/{{NEEDFULLNODENAME}}/'${NODE_NAME}'/' ${INPUT_FILENAME} > ${OUTPUT_FILENAME}

# ex: ft=sh ts=4 sts=4 sw=4 et:
