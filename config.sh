#!/bin/bash

clear
echo
echo " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # "
echo " #                                                                                     # "
echo " #                                    Hello World!!                                    # "
echo " #                                                                                     # "
echo " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # "
echo

# Copy configuration files to respective directories
echo "Copying configuration files..."

cp -r ./tmux ~/.config/tmux
cp -r ./nvim ~/.config/nvim
cp -r ./fish ~/.config/fish
cp -r ./alacritty ~/.config/alacritty

echo "All configuration files copied successfully!! :)"
