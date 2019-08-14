#!/usr/bin/env bash

cd "$(dirname "$0")"

cfg () {
	ln -sf $(pwd)/files/"$1" "$2"
}

cfg .vim/ ~/
cfg i3/ ~/.config/
cfg .bashrc ~/
cfg .bash_profile ~/
cfg .xinitrc ~/.xinitrc
cfg gtk-3.0 ~/.config/
cfg .gtkrc-2.0 ~/
cfg .Xresources ~/
cfg ranger ~/.config/
cfg .gitconfig ~/
cfg terminalrc ~/.config/xfce4/terminal/

xrdb -load ~/.Xresources

# Run `nitrogen ~/config/` to configure wallpaper.

