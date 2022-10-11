#!/bin/bash

set -o pipefail
set -exu

cd "$(dirname "$0")"

rm -rf ./example/outputs
mkdir -p ./example/outputs

stack run -- -v --basedir=./example/inputs --outdir=./example/outputs/no-flags
stack run -- -v --basedir=./example/inputs --outdir=./example/outputs/recursive --recurse
stack run -- -v --basedir=./example/inputs --outdir=./example/outputs/trimmed --trim --recurse

ls -R ./example/outputs/*
