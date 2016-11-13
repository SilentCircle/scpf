#!/bin/bash

set -e

(( $# == 2 ))

NODE_NAME=$1; shift
TEST_SPEC_NAME=$1; shift

TEST_SUITE_DATA=test/scpf_SUITE_data
TEST_SUITE_DIR=_build/test/lib/scpf/${TEST_SUITE_DATA}

copy_cert_data() {
    CA_DIR=tools/apns_tools/CA

    [[ -d ${CA_DIR} ]] || die "Expected ${CA_DIR} to exist"

    mkdir -p ${TEST_SUITE_DIR}
    find ${TEST_SUITE_DIR} -name '*.pem' | xargs rm -f

    cp ${CA_DIR}/*.pem ${TEST_SUITE_DIR}/

    for dir in $CA_DIR ${CA_DIR}/WWDRCA ${CA_DIR}/ISTCA2G1; do
        cp $dir/{certs,private}/*.pem ${TEST_SUITE_DIR}/
    done
}

echo $0: Node: $NODE_NAME Test spec: $TEST_SPEC_NAME

# Get the cert generation tools
echo $0: Get fake cert tools and generate certs in $(pwd)/tools/apns_tools/CA
./get_apns_tools.sh

echo $0: Copy fake cert data to ${TEST_SUITE_DIR}
copy_cert_data

# Generate the test spec
echo $0: Generate test spec ${TEST_SPEC_NAME} for node ${NODE_NAME}
./template_nodename.sh ${NODE_NAME} ${TEST_SPEC_NAME}.src ${TEST_SPEC_NAME}
