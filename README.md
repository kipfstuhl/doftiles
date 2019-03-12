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

  * **Emacs** best text editor, see its [configuration](emacs/.emacs.d/config.org)
  * **kitty** Terminal emulator: works as expected and supports Fira
      Code font (xterm and urxvt don't always work as expected,
      likewise st)
  * **zsh** Z shell: more user friendly than bash
  * **bash** Bourne again shell: still a good choice
  * **zathura** PDF reader: fully configurable, can be used with
      keyboard only, has synctex support, can use either poppler or
      mupdf as backend
  * **awesome** Window manager: nomen est omen
  * **mplayer** video player
  * **stow** software installation manager: very handy tool
  * **xsecurelock, xss-lock, xscreensaver** bundle for locking the screen

	  xss-lock provides handy features for timeout, locking upon request,
	  lock when suspended\
	  xsecurelock does the actual work to securely lock the workspace\
	  xscreensaver is the most useless tool for this task, it just
      displays some nice animations or images when the screen is locked
  * **pwgen** password generator
  * **yay** AUR helper: install software from AUR, do not use yaourt,
      this is not maintained anymore
  * **cmark** markdown formatter: converts CommonMark formatted Text
      to HTML (or other output), lightweight but full featured
  * **TeXLive** TeX distribution, mainly for LuaTeX, a new and
      strongly improved TeX enginge
  * **Programming Tools**
	  * **gcc** good compiler suite, `gcc`: C, `g++`: `C++`, gfortran:
		  Fortran
	  * **Rust** good programming lanuage for low level code, prevents
        some mistakes
	  * **Python** go-to scripting language, it has also good support
		  for scientific programming
		  * **NumPy** fast arrays for numerical computing
		  * **Cython** if Python is not fast enough or integration
			  with C code is needed
		  * **Ipython** enhanced Python REPL, for using Python
			  interactively this is currently the best solution
	  * **Jupyter** Notebooks for Python, Julia, and many more. Like
		  Mathematica or Matlab notebooks.
	  * **llvm** second C and C++ compiler framework, has also some useful
		  libraries (`libclang`) to better support IDEs, often used as
		  a backend for other compilers or JIT compilers, e.g. Julia,
		  Rust. The C compiler is `clang`.
	  * **SBCL** very good Common Lisp implementation; Lisp is just
		  great, so having one installed is matter of system
		  maintainance
	  * **git** one of the two version control systems you should use
		  (the other is hg)
	  * **magit** integrates git into Emacs; killer feature of Emacs
	  * **Julia** scientific computing language, it feels like a
		  scripting language, but it is possible to write very fast
		  JIT-compiled code
	  * **go** useful to have installed, in fact this language is not
		  so bad, but I think there are better ones
	  * **OpenMPI** free implementation of the MPI standard
	  * **Lua** a nice scripting language, integrates very well with C
		  and C++ code, used to configure aweseome window manager,
		  also used in LuaTeX

