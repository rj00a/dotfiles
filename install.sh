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
cfg .gitconfig ~/
cfg alacritty.yml ~/.config/
cfg vlcrc ~/.config/vlc/

xrdb -load ~/.Xresources

sudo systemctl enable NetworkManager
sudo systemctl enable org.cups.cupsd

# Run `nitrogen ~/install/` to configure wallpaper.

