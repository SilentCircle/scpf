#!/bin/bash

set -e

echo "Environment:"
env
echo "Current dir: $(pwd)"

TEST_SUITE_DIR=test/scpf_SUITE_data

copy_cert_data() {
    CA_DIR=tools/apns_tools/CA

    [[ -d ${CA_DIR} ]] || die "Expected ${CA_DIR} to exist"

    if [[ ! -d ${TEST_SUITE_DIR} ]]; then
        mkdir -p ${TEST_SUITE_DIR}
    elif ls ${TEST_SUITE_DIR}/*.pem > /dev/null 2>&1; then
        chmod a+w ${TEST_SUITE_DIR}/*.pem
        rm -f ${TEST_SUITE_DIR}/*.pem
    fi

    cp ${CA_DIR}/*.pem ${TEST_SUITE_DIR}/

    for dir in $CA_DIR ${CA_DIR}/WWDRCA ${CA_DIR}/ISTCA2G1; do
        cp $dir/{certs,private}/*.pem ${TEST_SUITE_DIR}/
    done

    chmod -R a+w ${TEST_SUITE_DIR}/*.pem
}

# Get the cert generation tools
echo $0: Get fake cert tools and generate certs in $(pwd)/tools/apns_tools/CA
./get_apns_tools.sh

echo $0: Copy fake cert data to ${TEST_SUITE_DIR}
copy_cert_data

