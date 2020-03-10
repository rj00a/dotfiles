#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

l () {
    mkdir -p "$2"
    ln -sf "$(pwd)/files/$1" "$2"
}

bash update-submodules.bash

l fish ~/.config/
l nvim ~/.config/
l plug.vim ~/.local/share/nvim/site/autoload/
l i3/ ~/.config/
l .xinitrc ~/
l gtk-3.0 ~/.config/
l .gtkrc-2.0 ~/
l .Xresources ~/
l .gitconfig ~/
l alacritty.yml ~/.config/
l .nethackrc ~/
l .inputrc ~/
l .gdbinit.d ~/
l gdb-dashboard/.gdbinit ~/
l fontconfig ~/.config/
l zathura ~/.config/
l polybar ~/.config/
l mpv ~/.config/
l polybar ~/.config
l .haskeline ~/

[[ "$(systemctl is-enabled NetworkManager)" == 'disabled' ]] &&
sudo systemctl enable NetworkManager

[[ "$(systemctl is-enabled org.cups.cupsd)" == 'disabled' ]] &&
sudo systemctl enable org.cups.cupsd

[[ "$(systemctl is-enabled ntpd)" == 'disabled' ]] &&
sudo systemctl enable ntpd

# Don't forget to install graphics drivers specific for your hardware.

exit 0
