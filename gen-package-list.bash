#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

explicit='package-list/explicit.txt'
everything='package-list/everything.txt'

printf '==== Generated with "pacman -Qqe" ====\n\n' > $explicit
pacman -Qqe >> $explicit

printf '==== Generated with "pacman -Q" ====\n\n' > $everything
pacman -Q >> $everything

echo "Finished updating package list."
