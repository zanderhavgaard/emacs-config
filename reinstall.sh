#!/bin/bash

rm ~/.emacs
rm -r ~/.emacs.d

sudo pacman -Rns emacs
sudo pacman -S emacs

bash ~/emacs-config/setup.sh
