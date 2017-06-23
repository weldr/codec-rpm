#!/bin/bash

# NOTE:
# this file should be called from the project's root directory

RPM=$1

cat $RPM | ./dist/build/inspect/inspect
cat $RPM | ./dist/build/rpm2json/rpm2json | json_verify

if [[ `./dist/build/unrpm/unrpm` != "Usage: unrpm RPM [RPM ...]" ]]; then
    exit 1
fi

./dist/build/unrpm/unrpm $RPM
