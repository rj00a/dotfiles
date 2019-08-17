#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

explicit='package-list/explicit.txt'
everything_verbose='package-list/everything-verbose.txt'
everything='package-list/everything.txt'

printf '==== Generated with "pacman -Qqe" ====\n\n' > $explicit
pacman -Qqe >> $explicit

printf '==== Generated with "pacman -Qq" ====\n\n' > $everything
pacman -Qq >> $everything

printf '==== Generated with "pacman -Qie" ====\n\n' > $everything_verbose
pacman -Qi >> $everything_verbose

