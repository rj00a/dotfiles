#!/usr/bin/env fish

cd (dirname (status --filename)) || exit

function l
    mkdir -p $argv[2]
    ln -sf (pwd)/files/$argv[1] $argv[2]
end

fish update-submodules.fish

l fish ~/.config/
l bspwm ~/.config/
l sxhkd ~/.config/
l plug.vim ~/.local/share/nvim/site/autoload/
l .xinitrc ~/
l gtk-3.0 ~/.config/
l .gtkrc-2.0 ~/
l .Xresources ~/
l .gitconfig ~/
l alacritty.yml ~/.config/
l .nethackrc ~/
l .inputrc ~/
l .gdbinit.d ~/
l .haskeline ~/
l init.el ~/.emacs.d/
l gdb-dashboard/.gdbinit ~/
l fontconfig ~/.config/
l zathura ~/.config/
l polybar ~/.config/
l mpv ~/.config/
l index.theme ~/.icons/default/
l polybar ~/.config

if [ (systemctl is-enabled NetworkManager) = disabled ]
    sudo systemctl enable NetworkManager
end

if [ (systemctl is-enabled org.cups.cupsd) = disabled ]
    sudo systemctl enable org.cups.cupsd
end

if [ (systemctl is-enabled ntpd) = disabled ]
    sudo systemctl enable ntpd
end

# Don't forget to install graphics drivers specific for your hardware.
