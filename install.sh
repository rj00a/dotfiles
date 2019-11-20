#!/usr/bin/env bash

cd "$(dirname "$0")"

l () {
	ln -sf $(pwd)/files/"$1" "$2"
}

l .vim/ ~/
l i3/ ~/.config/
l .bashrc ~/
l .bash_profile ~/
l .xinitrc ~/.xinitrc
l gtk-3.0 ~/.config/
l .gtkrc-2.0 ~/
l .Xresources ~/
l .gitconfig ~/
l alacritty.yml ~/.config/
l vlcrc ~/.config/vlc/
l .nethackrc ~/
l .inputrc ~/
l .gdbinit.d ~/
l gdb-dashboard/.gdbinit ~/
l vim-plug/plug.vim ~/.vim/autoload/
l fontconfig ~/.config/
l zathura ~/.config/
l polybar ~/.config/

sudo systemctl enable NetworkManager
sudo systemctl enable org.cups.cupsd

# Run `nitrogen ~/shared/` to configure wallpaper.

