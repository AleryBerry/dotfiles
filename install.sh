#!/bin/bash

sudo pacman-key --recv-key FBA220DFC880C036 --keyserver keyserver.ubuntu.com
sudo pacman-key --lsign-key FBA220DFC880C036
sudo pacman-key --keyserver hkps://keyserver.ubuntu.com --recv-keys 9AE4078033F8024D && sudo pacman-key --lsign-key 9AE4078033F8024D
sudo pacman -U 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-keyring.pkg.tar.zst' 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-mirrorlist.pkg.tar.zst'
sudo cp ./pacman/pacman.conf /etc/

sudo pacman -S neovim-git yay lightdm lightdm-webkit2-greeter numlockx qutebrowser bspwm sxhkd alacritty ncmpcpp mpd zsh polybar light $(pacman -Ssq xorg | grep -v git)
chsh -s /usr/bin/zsh
git clone 'https://github.com/AleryBerry/lightdm-webkit2-monoarch'
sudo cp ./lightdm-webkit2-monoarch /usr/share/lightdm-webkit/themes/monoarch -r
sudo rm ./lightdm-webkit2-monoarch
mkdir ~/.config/ncmpcpp ~/.config/sxhkd ~/.config/bspwm ~/.config/alacritty ~/.config/nvim/ ~/.config/qutebrowser/ ~/.config/mpd/ ~/.config/berry ~/.config/polybar
ln -srf ./config/*/* ~/.config/
ln -srf ./00-keyboard.conf /etc/X11/xorg.conf.d/
ln -srf ./.zshrc ~/
ln -srf ./.aliasrc ~/
ln -srf ./.outputrc ~/

curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

