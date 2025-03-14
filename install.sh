#!/bin/bash
sudo pacman-key --recv-key 3056513887B78AEB --keyserver keyserver.ubuntu.com
sudo pacman-key --lsign-key 3056513887B78AEB
sudo pacman -U 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-keyring.pkg.tar.zst'
sudo pacman -U 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-mirrorlist.pkg.tar.zst'
curl -s 'https://liquorix.net/install-liquorix.sh' | sudo bash
sudo cp ./pacman/pacman.conf /etc/

sudo pacman -Syu efibootmgr networkmanager xclip dmenu dunst feh devilspie bc grub os-prober linux-lqx linux-lqx-headers yay lightdm lightdm-webkit2-greeter numlockx qutebrowser bspwm sxhkd alacritty ncmpcpp mpd zsh light $(pacman -Ssq xorg | grep -v git)
yay -S sysfex tstock lightscreen polybar-git xcompmgr xorg python-pip tabnine unzip 7-zip npm neovim-git
chsh -s /usr/bin/zsh
git clone 'https://github.com/timberhill/lightdm-webkit2-monoarch'
sudo cp ./lightdm-webkit2-monoarch /usr/share/lightdm-webkit/themes/monoarch -r
sudo rm ./lightdm-webkit2-monoarch -r
mkdir ~/.config/ncmpcpp ~/.config/sxhkd ~/.config/bspwm ~/.config/alacritty ~/.config/nvim/ ~/.config/qutebrowser/ ~/.config/mpd/ ~/.config/berry ~/.config/polybar
ln -srf ./.config/bspwm/* ~/.config/bspwm
ln -srf ./.config/sxhkd/* ~/.config/sxhkd
ln -srf ./.config/alacritty/* ~/.config/alacritty
ln -srf ./.config/nvim/* ~/.config/nvim
ln -srf ./.config/qutebrowser/* ~/.config/qutebrowser
ln -srf ./.config/mpd/* ~/.config/mpd
sudo ln -srf ./00-keyboard.conf /etc/X11/xorg.conf.d/
ln -srf ./.zshrc ~/
ln -srf ./.aliasrc ~/
ln -srf ./.outputrc ~/

git clone https://github.com/zplug/zplug ~/.zplug
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
python -m pip install pynvim
chmod +x ~/.config/bspwm/bspwmrc
sudo systemctl enable lightdm
sudo systemctl enable NetworkManager
