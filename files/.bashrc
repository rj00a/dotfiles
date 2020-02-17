# Extended globbing.
shopt -s extglob

# Include hidden items in globs
shopt -s dotglob

# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

# Make autocomplete use colors
bind 'set colored-stats on'

# A generated PS1 from http://bashrcgenerator.com
# It looks something like this: [ryan@rjpc 0 ~]$
export PS1="\[\033[38;5;7m\][\[$(tput sgr0)\]\[\033[38;5;13m\]\u\[$(tput sgr0)\]\[\033[38;5;7m\]@\[$(tput sgr0)\]\[\033[38;5;14m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]\[\033[38;5;7m\]\$?\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]\[\033[38;5;7m\]\w]\[$(tput sgr0)\]\[\033[38;5;8m\]\\$\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"

# Preferred Programs
export EDITOR=vim
export VISUAL=vim
export BROWSER=chromium

# Ignore duplicate entries in command history
export HISTCONTROL=ignoreboth:erasedups

# Sets the timezone for the 'date' command
export TZ=America/Los_Angeles

# Allows use of the 'z' command
# Installed with the 'z' pacman package (community repo)
source /usr/share/z/z.sh

alias backup='sh ~/shared/scripts/desktop-backup-data.sh'

alias nethack='nethack -d ~/games/nethack-playground'

alias ls='ls -A --color=auto --group-directories-first'

# List lots of stuff
alias la='ls -la --color=auto'

alias grep='grep --color=auto'

alias view='vim -R'

# Print usage information about the current filesystem
alias ds="df -B GiB . | tr -s ' ' | cut -d ' ' -f 3,4,5 | column -t"

alias tra='trash'
alias tra-clear='trash-clear'
alias tra-empty='trash-empty'
alias tra-list='trash-list'

alias mkd='mkdir -p'

alias tou='touch'

# Safe move, prompt before overwriting
alias sm='mv -i'

# Safe copy, prompt before overwriting
alias sc='cp -ri'

# BitTorrent Curses Interface
alias rtor='rtorrent'

alias syu='yay -Syu'

# Import math and fractions, start REPL, hide copyright msg, and don't write .pyc files
# Makes python more suitable as a calculator
alias py="python3 -Bqic 'from math import *; from fractions import Fraction'"

# Download youtube video
alias yv='youtube-dl -iwcR infinite --add-metadata'

# Download youtube audio only
alias ya='youtube-dl -xwicR infinite -f bestaudio --audio-quality 0 --add-metadata'

# Open link in clipboard with mpv.
# Useful for youtube videos, twitch streams, etc.
alias mpvx='mpv --fs "$(xclip -o)"'

# Kill process by name
alias fuck='sudo pkill -ie'

# Grep for installed packages
alias paks='yay -Qq | rg -i'

# Activate xscreensaver
alias screensaver='xscreensaver-command -activate'

# Echo stdin to stderr
errcho() {
    >&2 echo $@
}

# Run command in the background
rbg() {
    [[ -z "$@" ]] && {
        errcho 'Expected argument(s)'
        return 1
    }
    ("$@" &) &> /dev/null
}

# Update the system and push changes to shared repo.
# Plays a sound when changes are ready to be pushed.
update() (
    # Note that this is running in a subshell
    gitupdate() {
        {
            git add -A
            git status
        } || return
        # If there is nothing to push, don't try to push anything
        git commit -m update || git push origin master
        return 0
    }
    {
        cd ~/shared &&
        echo "==== in $(pwd) ====" &&
        yay -Syu &&
        sh gen-package-list.sh &&
        sh update-submodules.sh &&
        vim -c 'PlugInstall|PlugUpdate|qa' &&
        (paplay files/bell.ogg &)
        gitupdate &&
        cd /mnt/sdb1/keepass &&
        echo "==== in $(pwd) ====" &&
        gitupdate &&
        cd ~/school/ &&
        echo "==== in $(pwd) ====" &&
        gitupdate
    } || exit
)

# Make directory if not exist and cd into it
ccd() {
    [[ -z "$1" ]] && {
        errcho 'Expected one argument'
        return 1
    }
    mkdir -p "$1" && cd "$1"
}

# Play something with mpv using fzf and exit
play() {
    # Pipe stderr to null to hide annoying error messages like "lost+found permission denied"
    local file="$(find /mnt/sda1/ 2> /dev/null | fzf)"
    [[ -z "$file" ]] && return
    # pseudo-gui causes mpv to display a gui even when there is no video (playing music for example)
    mpv --player-operation-mode=pseudo-gui "$file" &
    exit 0
}

# Pipe stdout and stderr of command to less
le() {
    [[ -z "$@" ]] && {
        errcho 'Expected argument(s)'
        return 1
    }
    "$@" |& less -r --
}

# Run pandoc on a file and convert it to HTML, then open it in a browser
panv() {
    [[ $# != 1 ]] && {
        errcho 'Expected exactly one argument'
        return 1
    }
    local tmp=`mktemp`.html
    pandoc "$1" > "$tmp"
    [[ -z "$BROWSER" ]] && {
        errcho '$BROWSER is unset'
        return 2
    }
    $BROWSER "$tmp"
}

# Open zathura on file, disown it, and quit.
# Error out if file does not exit
zath() {
    # If file does not exist
    [[ ! -f "$1" ]] && {
        errcho "Unknown file $1"
        return 1
    }
    zathura "$1" & disown
    exit 0
}

# Run 'dd' with some sensible defaults and a confirmation prompt
ezdd() {
    [[ $# != 2 ]] && {
        errcho "Expected two arguments (input and output file). Found $# arguments."
        return 1
    }
    read -rn 1 -p "Are you sure you want to write \"$1\" to \"$2\"? [y/N] "
    echo
    if [[ "$REPLY" =~ ^[yY]$ ]]; then
        sudo dd bs=16M if="$1" of="$2" status=progress
    else
        return 2
    fi
}

