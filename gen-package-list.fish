#!/usr/bin/env fish

cd (dirname (status --filename)) || exit

set explicit 'package-list/explicit.txt'
set everything 'package-list/everything.txt'

printf '==== Generated with "pacman -Qqe" ====\n\n' > "$explicit"
pacman -Qqe >> "$explicit"

printf '==== Generated with "pacman -Q" ====\n\n' > "$everything"
pacman -Q >> "$everything"

echo 'Finished updating package list.'

