# Dotfiles #

My dotfiles for use with GNU Stow

Konfiguratinsdatein für die Verwaltung mit GNU Stow


## Installation ##
First install Gnu Stow via packet manager.

Install applications that should be used, e.g. Emacs, mplayer, X server, …

Clone this repo in the home directory, i.e.

```bash
cd ~
git clone https://github.com/kipfstuhl/dotfiles.git

```

Then use stow for the desired applications.
```bash
cd dotfiles
stow emacs
```
Use the shorter command `stow .` if everything is to be installed.
