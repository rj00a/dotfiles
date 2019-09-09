#!/usr/bin/env bash
# Because I always forget.

# Required for new submodules
git submodule update --init

git submodule update --recursive --remote --jobs 10
