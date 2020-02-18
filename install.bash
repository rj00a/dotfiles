#!/usr/bin/env bash

cd "$(dirname "$0")"

l () {
    mkdir -p $(dirname "$2")
    ln -sf $(pwd)/files/"$1" "$2"
}

bash update-submodules.bash

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
l .nethackrc ~/
l .inputrc ~/
l .gdbinit.d ~/
l gdb-dashboard/.gdbinit ~/
l vim-plug/plug.vim ~/.vim/autoload/
l fontconfig ~/.config/
l zathura ~/.config/
l polybar ~/.config/
l .xscreensaver ~/.xscreensaver
l mpv ~/.config/
l polybar ~/.config

[[ "$(systemctl is-enabled NetworkManager)" == 'disabled' ]] &&
sudo systemctl enable NetworkManager

[[ "$(systemctl is-enabled org.cups.cupsd)" == 'disabled' ]] &&
sudo systemctl enable org.cups.cupsd

# Run `nitrogen ~/shared/` to configure wallpaper.

# TODO: print diff of installed pacman packages?

exit 0
