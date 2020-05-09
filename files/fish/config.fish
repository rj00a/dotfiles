#TODO: Change the cursor shape in different modes (should be working already; bugged with alacritty?)
#TODO: Make 'u' in normal mode undo (instead of command history)?
#TODO: change colors to match vim colorscheme.

# No fish greeting
set fish_greeting

function hybrid_key_bindings -d "Vi bindings that inherit emacs bindings in all modes."
    for mode in default insert visual
        fish_default_key_bindings -M $mode
    end
    fish_vi_key_bindings --no-erase
end
set -g fish_key_bindings hybrid_key_bindings

# Disable the mode indicator
set fish_mode_prompt

# Set the cursor shapes for the different vi modes.
set fish_cursor_default block blink
set fish_cursor_insert line blink
set fish_cursor_replace_one underscore blink
set fish_cursor_visual block

# Show the full path of the cwd in the prompt.
set fish_prompt_pwd_dir_length 0

# Initialize z
source ~/shared/files/z.fish
z --add "$CWD"

set -x XDG_DOWNLOAD_DIR ~/dl/

set -x VISUAL vi
set -x BROWSER chromium

# Make python write .pyc files to this dir instead of cwd for wherever
set -x PYTHONCACHEPREFIX "$HOME/.cache/cpython/"

# Run this file on startup
set -x PYTHONSTARTUP ~/shared/files/.pythonrc

# Make less not write to ~/.lesshst
set -x LESSHISTFILE -

# Config directory for DOOM Emacs.
set -x DOOMDIR ~/shared/files/.doom.d/

# Set the timezone for 'date' command
set -x TZ 'America/Los_Angeles'

# Save data is located in nethack-playground
alias nethack='nethack -d ~/games/nethack-playground'

alias ls='ls -aA --color=auto --group-directories-first'
alias backup='fish ~/shared/scripts/backup-data.fish'

# Don't let sxiv cache files for privacy reasons.
alias sxiv='sxiv -p'

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
    if [ (count $argv) -gt 0 ]
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

function rbg -d 'Run program in the background and disown'
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
    set file (find /mnt/sda1/{music/, films/} -type f 2> /dev/null | fzf)
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
    nvim -c 'PlugUpdate|PlugInstall|qa' &&
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

function ezdd -d "'dd' with confirmation prompt and good arguments."
    set numargs (count $argv)
    if [ $numargs != 2 ]
        errcho "Expected 2 arguments (input and output file). Found $numargs arguments."
        return 1
    end
    set reply (read -n 1 -P "Are you sure you want to write \"$argv[1]\" to \"$argv[2]\"? [y/N] ")
    echo
    if [ "$reply" = y ]; or [ "$reply" = Y ]
        sudo dd bs=16M if="$argv[1]" of="$argv[2]" status=progress
    else
        return 2
    end
end

function em -d 'Fuzzy find an emoji and copy it to the clipboard'
    set res (fzf < ~/shared/files/emojis.txt)
    if [ $status = 0 ]
        printf '%s' (echo $res | cut -f 1 -d ' ') | xclip -selection clipboard
    end
end

# Start X at login (Keep this at the end of the script)
if status is-login
    if [ -z "$DISPLAY" -a $XDG_VTNR = 1 ]
        exec startx -- -keeptty
    end
end
