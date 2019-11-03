echo -e "
░█▀▀░█▄█░█▀█░█▀▀░█▀▀░░░█▀▀░█▀▀░▀█▀░█░█░█▀█
░█▀▀░█░█░█▀█░█░░░▀▀█░░░▀▀█░█▀▀░░█░░█░█░█▀▀
░▀▀▀░▀░▀░▀░▀░▀▀▀░▀▀▀░░░▀▀▀░▀▀▀░░▀░░▀▀▀░▀░░
"

echo -e "\nSymlinking .emacs ..."
mkdir /home/zander/.emacs.d &> /dev/null
rm /home/zander/.emacs # sorry not sorry...
ln -sv /home/zander/emacs-config/.emacs /home/zander/.emacs

echo -e "\nRunning Emacs to finish installation!"
echo -e "After all of the packages are installed run: M-x all-the-icons-install-fonts to install icon fonts"
emacs
