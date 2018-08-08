# Dotfiles #

My dotfiles for use with GNU Stow

Konfiguratinsdatein für die Verwaltung mit GNU Stow


## Installation ##
First install GNU Stow preferably via the packet manager of your
distribution.

Install applications that should be used, e.g. Emacs, mplayer, X
server, …

Clone this repository in the home directory, i.e.

```bash
cd ~
git clone https://github.com/kipfstuhl/dotfiles.git

```

Then use stow for the desired applications.
```bash
cd dotfiles
stow emacs
```
<!-- Use the shorter command `stow .` if everything is to be installed. -->

## Installation ##
Zuerst muss GNU Stow installiert werden, idealer Weise mit dem Paketmanager.

Danach die gewünschten Programme installieren, z.B. Emacs, mplayer, X
Server, …

Dieses Repository klonen:
```bash
cd ~
git clone https://github.com/kipfstuhl/dotfiles.git

```

Anschließend stow für die gewünschten Anwendungen ausführen:
```bash
cd dotfiles
stow emacs
```

Um manche Programme auch in einer XDG Umgebung verwenden zu können,
z.B. mit xdg-open, müssen diese noch registriert werden.\
Beispiel: zathura als standard für PDF Dateien verwenden.
```bash
xdg-mime default zathura.desktop application/pdf
```

<!-- Wenn alles installiert werden soll, funktioniert auch das kurze Kommando `stow .` -->


## Current Programs ##
Here is a short list of programs that are currently configured in this
repository.

  * **Emacs** best text editor
  * **kitty** terminal emulator
  * **zsh** Z shell: more user friendly than bash
  * **bash** Bourne again shell: still a good choice
  * **zathura** PDF reader: fully configurable, can be used with keyboard
    only, has synctex support
  * **awesome** Window manage: nomen est omen
  * **julia** scientific computing language
  * **kitty** Terminal emulator: just works as expected and supports Fira
    Code font
  * **mplayer** video player
  * **stow** software installation manager: very handy tool
  * **xsecurelock, xss-lock, xscreensaver** bundle for locking the screen

	  xss-lock provides handy features for timeout, locking upon request

	  xsecurelock does the work to securely lock the workspace

	  xscreensaver is the most useless tool for this task, it just
      displays some nice animations or images when the screen is locked
