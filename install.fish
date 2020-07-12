#!/usr/bin/env fish

cd (dirname (status --filename)) || exit

function l
    mkdir -p $argv[2]
    ln -sf (pwd)/files/$argv[1] $argv[2]
end

fish update-submodules.fish

l .Xresources ~/
l .gdbinit.d ~/
l .gitconfig ~/
l .gtkrc-2.0 ~/
l .haskeline ~/
l .ideavimrc ~/
l .inputrc ~/
l .nethackrc ~/
l .xinitrc ~/
l alacritty.yml ~/.config/
l bspwm ~/.config/
l fish ~/.config/
l fontconfig ~/.config/
l gdb-dashboard/.gdbinit ~/
l gtk-3.0 ~/.config/
l gzdoom.ini ~/.config/gzdoom/
l htoprc ~/.config/htop/
l index.theme ~/.icons/default/
l mpv ~/.config/
l nvim/ ~/.config/
l plug.vim ~/.local/share/nvim/site/autoload/
l polybar ~/.config
l polybar ~/.config/
l sxhkd ~/.config/
l zathura ~/.config/

if [ (systemctl is-enabled NetworkManager) = disabled ]
    sudo systemctl enable NetworkManager
end

if [ (systemctl is-enabled org.cups.cupsd.service) = disabled ]
    sudo systemctl enable org.cups.cupsd.service
end

if [ (systemctl is-enabled ntpd) = disabled ]
    sudo systemctl enable ntpd
end

if [ (systemctl is-enabled avahi-daemon.service) = disabled ]
    sudo systemctl enable avahi-daemon.service
end

# Don't forget to install graphics drivers
