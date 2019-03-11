(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("LaTeXnoint" "%`%l -interaction=nonstopmode %(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode))
     ("LuaLaTeX" "%`lualatex --synctex=1%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode latex-mode)
      :help "Run LuaLaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-engine (quote luatex))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list nil)
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-pdf "Zathura")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("e6b532a86f14694d631b4b5e6f51b42bcdca5b3ee49466e25c1e87486748625d" "5c81684770e011d4d6a0b1661be470e0acf7918873c28a02e49d932cb69cd4d8" "9f53915e90dc34e6035e34a6267dc6166b15bb8fa2430c478b872bf4fe3d3d37" "2538910ec98a0f023714cd0b13e2337b7219ee48f19b5ec73b11abaf58f00f91" "e385b7e0dba6ef3bf631f7e3a08b891259b2563fcbf2b5b9a6438c51fa9f4566" "ada51151eb886bbe7f027e807bb1a02242d09a5a64b35eca8b6d8b9597f5d2d2" "e9cbee60387b249e622a961db3f51a77d11ce1a5d735c8f6264f9a3541faf60b" "b462e2411830a39699856dcce0a72f9b11e6952dd07be5c65ae5f2f91eea25f1" "bf21a33d9f35ee10c2378ce999424002836ac3f6bdc2c94f2396ad44ce32c998" "b587774bd67083d98778e40b093ba822d25b5f842aaf95116015d4dbd624b5d1" "00e0c2f0373582a2bf6df1e63eddc05d7eb2ba7a7688b175d13c7e9ef53eeef6" default)))
 '(display-time-mode t)
 '(exec-path
   (quote
    ("/home/jonas/bin" "/home/jonas/.local/bin" "/home/jonas/.cargo/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/bin" "/usr/lib/jvm/default/bin" "/usr/bin/site_perl" "/usr/bin/vendor_perl" "/usr/bin/core_perl" "/usr/lib/emacs/26.1/x86_64-pc-linux-gnu" "/home/jonas/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin")))
 '(font-latex-fontify-script nil)
 '(interleave-disable-narrowing t)
 '(org-agenda-files (quote ("~/example.org")))
 '(package-selected-packages
   (quote
    (github-review yasnippet-snippets yasnippet helm go-mode hideshow-org rust-playground interleave pdf-tools slime slime-company company company-auctex company-c-headers company-jedi company-racer fd-dired fzf auctex lua-mode magithub nyan-mode paradox multiple-cursors ac-c-headers ac-math toml-mode ac-octave auto-complete-c-headers ssh slime-volleyball slime-theme slime-ritz slime-docker slime-annot python3-info python-info python-docstring org matlab-mode markdown-mode magit jedi-direx google-maps german-holidays ess-view ess-smart-underscore ess-smart-equals ess-R-object-popup ess-R-data-view ein-mumamo cython-mode cuda-mode cargo calfw c-eldoc auctex-lua auctex-latexmk aes ac-slime)))
 '(paradox-github-token t)
 '(prettify-symbols-unprettify-at-point (quote right-edge))
 '(reftex-plug-into-AUCTeX nil)
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
