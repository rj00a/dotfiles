#!/usr/bin/env fish

cd (dirname (status --filename)) || exit

# Required for new submodules
git submodule update --init &&

git submodule update --recursive --remote --jobs 10 &&

echo 'Updated submodules.'
