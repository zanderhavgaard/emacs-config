#!/bin/bash

rm /home/zander/.emacs
rm -r /home/zander/.emacs.d

sudo pacman -Rns emacs
sudo pacman -S emacs

bash /home/zander/emacs-config/setup.sh
