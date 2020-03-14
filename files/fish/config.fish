#TODO: Change the cursor shape in insert mode
#TODO: Make ctrl+e finish the current autocompletion.
#TODO: Make 'u' in normal mode undo?

fish_vi_key_bindings
# Set the normal and visual mode cursors to a block
set fish_cursor_default block

# Set the insert mode cursor to a line
set fish_cursor_insert line

# Set the replace mode cursor to an underscore
set fish_cursor_replace_one underscore

# The following variable can be used to configure cursor shape in
# visual mode, but due to fish_cursor_default, is redundant here
set fish_cursor_visual block

set -x EDITOR nvim
set -x VISUAL nvim
set -x BROWSER chromium

# Make python write .pyc files to this dir instead of cwd for wherever
set -x PYTHONCACHEPREFIX "$HOME/.cache/cpython/"

# Set the timezone for 'date'
set -x TZ 'America/Los_Angeles'

alias nv='nvim'
alias nethack='nethack -d ~/games/nethack-playground'
alias ls='ls -aA --color=auto --group-directories-first'
alias view='nvim -R'

# Print usage information about the current filesystem
alias ds="df -B GiB . | tr -s ' ' | cut -d ' ' -f 3,4,5 | column -t"

alias tra='trash'
alias tra-empty='trash-empty'
alias tra-list='trash-list'
alias tra-put='trash-put'
alias tra-restore='trash-restore'
alias tra-rm='trash-rm'

alias mkd='mkdir -p'

# Prompt before overwriting
alias sm='mv -i'
alias sc='cp -ri'

alias rtor='rtorrent'

alias syu='yay -Syu'

# Import math and fractions, start REPL, hide copyright msg, and don't write .pyc files
# Makes python more suitable as a calculator
alias py="python3 -Bqic 'from math import *; from fractions import Fraction'"

# Kill process by name
alias fuck='sudo pkill -ie'

# Search for files interactively in this directory
alias sear='find . | fzf'

# Grep for installed packages
alias paks='pacman -Qq | grep -i'

# Download video
alias dlv='youtube-dl -iwcR infinite --add-metadata'

# Download audio
alias dla='youtube-dl -xwicR infinite -f bestaudio --audio-quality 0 --add-metadata'

function ytv -d 'Play a youtube video using arguments as the search terms'
    if test (count $argv) -gt 0
        mpv --ytdl-format=bestvideo+bestaudio "ytdl://ytsearch:$argv" &
        disown
    end
end

function yta -d 'Play a youtube video (audio only) using arguments as the search terms'
    if [ (count $argv) -gt 0 ]
        mpv --ytdl-format=bestaudio "ytdl://ytsearch:$argv" &
        disown
    end
end

function rbg -d 'Run program in the background'
    $argv > /dev/null 2>&1 &
    if jobs -q
        disown
    end
end

function errcho -d 'echo to stderr'
    echo $argv > /dev/stderr
end

function ccd -d 'Make a directory and cd into it'
    mkdir -p $argv[1] && cd $argv[1]
end

function play -d 'Play something with mpv using fzf and exit'
    set file (find /mnt/sda1/music/ -type f 2> /dev/null | fzf)
    if [ -n "$file" ]
        mpv --player-operation-mode=pseudo-gui "$file" &
        disown
        exit 0
    end
end

function playd -d 'Play a directory of audio files with mpv in proper album order'
    set dir (find /mnt/sda1/music/ -type d 2> /dev/null | fzf)
    if [ -z "$dir" ]
        return
    end
    set trax (python -B ~/shared/scripts/trackord.py "$dir"/*)
    if [ -z "$trax" ]
        return
    end
    mpv --player-operation-mode=pseudo-gui $trax &
    disown
    exit 0
end

function zath -d 'Open zathura on file and close terminal'
    if [ -n "$argv" ]
        zathura $argv &
        disown
        exit 0
    end
end

function git-update
    begin
        git add -A && git commit -m update
    end || return 0
    # Only push if we need to.
    if git status | grep 'ahead of' -q
        git push origin master
    end
end

function update -d 'Update system packages and push changes to shared repos.'
    pushd .
    # ====
    ~/shared &&
    echo '==== in '(pwd)' ====' &&
    yay -Syu &&
    fish gen-package-list.fish &&
    fish update-submodules.fish &&
    nvim -c 'PlugUpgrade|PlugUpdate|PlugClean!|qa' &&
    fish -c 'paplay files/bell.ogg' &&
    git-update &&
    /mnt/sdb1/keepass &&
    echo '==== in '(pwd)' ====' &&
    git-update &&
    ~/school &&
    echo '==== in '(pwd)' ====' &&
    git-update
    # ====
    popd
end

# Start X at login (Keep this at the end of the script)
if status is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
        exec startx -- -keeptty
    end
end
