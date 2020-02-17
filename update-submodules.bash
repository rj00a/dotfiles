#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

# Required for new submodules
git submodule update --init

git submodule update --recursive --remote --jobs 10

echo 'Updated submodules.'
