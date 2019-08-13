# bash_profile: This file runs on login

# Source bashrc if it exists
[[ -f ~/.bashrc ]] && . ~/.bashrc

# Start i3 if it's not already running and we are on tty1
# This causes ~/.xinitrc to run, which actually starts i3
if [ "$(tty)" = "/dev/tty1" ]; then
    pgrep -x i3 || exec startx
fi
