# emacsconfig
My personal emacs configuration.  

Packages:
- multiple-cursors
- tabbar-mode
Configuration:
- Dark theme
- CUA mode
- Key bindings for multiple-cursors and tabbar
- Date/time insert keys
- Tab-style indents for python
- Several editing shortcuts

To use this configuration for a user, clone the repository into their home folder (I recommend the .emacs.d folder if you want this to be the main configuration) and symbolically link the user's ~/.emacs file to the .emacs file in the repository.  

This is my typical setup:
``` bash
emacsdir=~/.emacs.d
emacsconf=~/.emacs
git clone git@github.com:mysterioustrashninja/emacsconfig.git $emacsdir
[[ -L $emacsconf ]] && rm $emacsconf || mv $emacsconf $emacsconf.old
ln -s $emacsconf $emacsdir/.emacs
```
