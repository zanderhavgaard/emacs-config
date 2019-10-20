echo -e "
░█▀▀░█▄█░█▀█░█▀▀░█▀▀░░░█▀▀░█▀▀░▀█▀░█░█░█▀█
░█▀▀░█░█░█▀█░█░░░▀▀█░░░▀▀█░█▀▀░░█░░█░█░█▀▀
░▀▀▀░▀░▀░▀░▀░▀▀▀░▀▀▀░░░▀▀▀░▀▀▀░░▀░░▀▀▀░▀░░
"

echo -e "\nSymlinking .emacs ..."
mkdir /home/$USER/.emacs.d &> /dev/null
rm /home/$USER/.emacs # sorry not sorry...
ln -sv /home/$USER/emacs-config/.emacs /home/$USER/.emacs

echo -e "\nRunning Emacs to finish installation!"
emacs
