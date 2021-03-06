(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
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
     ("Biber" "biber --isbn-normalise %s" TeX-run-Biber nil t :help "Run Biber")
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
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(TeX-engine 'luatex)
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list nil)
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-pdf "Zathura")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open")))
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("b3787afc4caf209f0709a83a34a3da50eab8a1f1c1f22ced8f6ecfc4e60fcd50" "f83a99fcf716fbd4731c1e96ae205e3f660320785750436585729f3182f57d8b" "7e640c8179f1900677e810efd95ecc1d4866afa7ef64f29f576b73e3f1fc1440" "333b05527ba1f48b4df2a8c8f9aafc8788306eebbdf2777b29592d6af324760d" "9f26dd428b15f344a5a88989f3137d13fca25ec585d90e937820e3e59bf1197e" "ab0813bd4987b25397592638778c69a682ee7687906607d051e92a77ecec54a9" "7ad4c9281c8ee0603ff38d6ffa8a6bbcfddaabe020ad388c4bf963fd26eb90d8" "1535d8ba97284d41bf20f001b5cb508a97011f72117965deb88865cbe8b830c4" "5b159f9078dfb551d1517b78eee0c3d632a586eba73d5d4e3f4a7a4a0482b2eb" "62c8deb902dfe61d9d75e12f67d692cd2db0360f2d0b3806a894347d44487cf8" "d7f91312da0efee234427aab526b32301da58921e354a9f2a9af805913ec3bc4" "3f5cb1075a7b5cdb2dee23105f65fae441f326d9a2038d75d03bbb62fcb7cdaf" "57302c359d6aaa25eca262ad34bababbe4f92cf875a384eeccdb4ad687af2ff5" "8e32f00d9a99f5bd8ee74f9531fe6ef08ab9c52b9bf1bc9a9e571f53cddb931e" "cf80028daf592643993e0b892cc67bde4243f2d7d0fd55e6b98cd9871df1f14e" "b31c1e81ed22427a1d870cfcfe6a925e554c1d7330fe8bd2e30c27f455de2735" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "e6b532a86f14694d631b4b5e6f51b42bcdca5b3ee49466e25c1e87486748625d" "5c81684770e011d4d6a0b1661be470e0acf7918873c28a02e49d932cb69cd4d8" "9f53915e90dc34e6035e34a6267dc6166b15bb8fa2430c478b872bf4fe3d3d37" "2538910ec98a0f023714cd0b13e2337b7219ee48f19b5ec73b11abaf58f00f91" "e385b7e0dba6ef3bf631f7e3a08b891259b2563fcbf2b5b9a6438c51fa9f4566" "ada51151eb886bbe7f027e807bb1a02242d09a5a64b35eca8b6d8b9597f5d2d2" "e9cbee60387b249e622a961db3f51a77d11ce1a5d735c8f6264f9a3541faf60b" "b462e2411830a39699856dcce0a72f9b11e6952dd07be5c65ae5f2f91eea25f1" "bf21a33d9f35ee10c2378ce999424002836ac3f6bdc2c94f2396ad44ce32c998" "b587774bd67083d98778e40b093ba822d25b5f842aaf95116015d4dbd624b5d1" "00e0c2f0373582a2bf6df1e63eddc05d7eb2ba7a7688b175d13c7e9ef53eeef6" default))
 '(display-time-mode t)
 '(exec-path
   '("/home/jonas/bin" "/home/jonas/.local/bin" "/home/jonas/.cargo/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/bin" "/usr/lib/jvm/default/bin" "/usr/bin/site_perl" "/usr/bin/vendor_perl" "/usr/bin/core_perl" "/usr/lib/emacs/26.1/x86_64-pc-linux-gnu" "/home/jonas/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin"))
 '(interleave-disable-narrowing t)
 '(package-selected-packages
   '(elpy shell-pop org-noter zenburn-theme rainbow-mode magit pdf-tools eglot helm-lsp company-lsp lsp-mode keychain-environment forge pinentry helm-gtags visual-fill-column writegood-mode helm-bibtex htmlize github-review yasnippet-snippets go-mode hideshow-org rust-playground interleave company company-auctex company-c-headers company-jedi company-racer fd-dired fzf auctex lua-mode nyan-mode paradox multiple-cursors ac-c-headers ac-math toml-mode ac-octave auto-complete-c-headers ssh slime-volleyball slime-theme slime-ritz slime-docker slime-annot python3-info python-info python-docstring org matlab-mode markdown-mode jedi-direx google-maps german-holidays ess-view ess-smart-underscore ess-smart-equals ess-R-object-popup ess-R-data-view ein-mumamo cython-mode cuda-mode cargo calfw c-eldoc auctex-lua auctex-latexmk aes ac-slime))
 '(paradox-github-token t)
 '(prettify-symbols-unprettify-at-point 'right-edge))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
