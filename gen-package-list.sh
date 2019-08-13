#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

explicit='package-list/explicit-only.txt'
everything='package-list/everything-verbose.txt'

printf '==== Generated with "pacman -Qq" ====\n\n' > $explicit
pacman -Qqe >> $explicit

printf '==== Generated with "pacman -Qie" ====\n\n' > $everything
pacman -Qi >> $everything

