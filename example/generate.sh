#!/bin/bash

set -o pipefail
set -exu

cd "$(dirname "$0")"

rm -rf ./outputs
mkdir -p ./outputs

stack run -- -v --basedir=./inputs --outdir=./outputs/no-flags
stack run -- -v --basedir=./inputs --outdir=./outputs/recursive --recurse
stack run -- -v --basedir=./inputs --outdir=./outputs/trimmed --trim --recurse

ls -R ./outputs/*
