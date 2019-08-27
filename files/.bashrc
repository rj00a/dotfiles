# Extended globbing. Lots of cool stuff
shopt -s extglob

# Can change the working directory without using 'cd'
# Annoyingly echoes to stdout though
shopt -s autocd

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

# Allows use of the 'z' command
# Installed with the 'z' pacman package (community repo)
source /usr/share/z/z.sh

alias backup-data='~/programs/backup-data.sh'

alias nethack='nethack -d ~/games/nethack-playground'

alias ls='ls -A --color=auto --group-directories-first'

# List lots of stuff
alias la='ls -oA --color=auto'

alias view='vim -R'

#alias fm='vifm'

alias tra='trash'

alias gv='gvim'

alias mkd='mkdir -p'

alias tou='touch'

# Safe move, prompt before overwriting
alias sm='mv -i'

# Safe copy, prompt before overwriting
alias sc='cp -i'

# BitTorrent Curses Interface
alias rtor='rtorrent'

# Update all packages and package repo
alias syyu='yay -Syyu'

# Import math, start REPL, hide copyright msg, and don't write .pyc files
# Makes python more suitable as a calculator
alias py="python3 -Bqic 'from math import *'"

# Download youtube video
alias yv='youtube-dl -iwcR infinite --add-metadata'
# Download youtube audio only
alias ya='youtube-dl -xwicR infinite -f bestaudio --audio-quality 0 --add-metadata'

# Useful for youtube videos, twitch streams, etc.
alias vlcx='vlc "$(xclip -o)"'

# Self explanatory
alias fuck='sudo pkill -ie'

# Grep for some installed packages
alias paks='yay -Qq | rg -i'

# Echo to stderr
errcho() {
	>&2 echo $@
}

# Make directory if not exist and cd into it
ccd() {
	mkdir -p "$1" && cd "$1"
}

# Play something with vlc using fzf and exit
play() {
	local file="$(fzf)"
	if [ -z "$file" ]; then
		return $?
	fi
	vlc "$file" &
	exit 0
}

# Open all files in a directory with vlc and exit
# Useful for quickly playing albums
playd() {
	local file="$(fzf)"
	if [ -z "$file" ]; then
		return $?
	fi
	vlc "$(dirname "$file")" &
	echo "$(dirname "$file")"
	exit 0
}

# Pipe stdout and stderr of command to less
le() {
	if [ -z "$1" ]; then
		errcho 'Expected at least one argument'
		return 1
	fi
	"$@" |& less -r --
}

# Run pandoc on a file and convert it to HTML, then open it in a browser
panv() {
	if [ $# != 1 ]; then
		errcho 'Expected exactly one argument'
		return 1
	fi
	local tmp=`mktemp`.html
	pandoc "$1" > "$tmp"
	cat "$tmp"
	echo ========================================
	if [ -z "$BROWSER" ]; then
		errcho '$BROWSER is unset'
		return 2
	fi
	$BROWSER "$tmp"
}

# Run 'dd' with some sensible defaults and a confirmation prompt
ezdd() {
	if [ $# != 2 ]; then
		errcho "Expected two arguments (input and output file). Found $# arguments."
		return 2
	fi
	read -r -n 1 -p "Are you sure you want to write \"$1\" to \"$2\"? [y/N] "
	echo
	if [[ "$REPLY" =~ ^[yY]$ ]]; then
		sudo dd bs=1M if="$1" of="$2" status=progress
	else
		return 1
	fi
}

