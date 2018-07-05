;; Adjust the visual appearence at the beginning, this gives a feeling
;; of fast loading init file

; hide menu bar
;(menu-bar-mode 0)
; hide tool bar
;(tool-bar-mode 0)
; hide scroll bar on the side
;(scroll-bar-mode 0)
; do not blink
;(blink-cursor-mode 0)
; display current time
;(display-time-mode 1)
; load a nice theme
(load-theme 'tango-dark-new t)	 ;don't confirm the load
; remove the box around the mode line
; this has to be after loading the theme
; now it is in the new theme
;; (set-face-attribute 'mode-line nil
;; 		    :box nil)
;; ; again for the inactive "windows" (emacs jargon)
;; (set-face-attribute 'mode-line-inactive nil
;; 		    :box nil)
;; don't add a column for wrapped lines at the right side, only left
;(set-fringe-mode '(nil . 0))
; no startup screen
;(setq inhibit-startup-message t)


;(setq prettify-symbols-unprettify-at-point 'right-edge) ; set this via customize


;; non specific keybindings
(global-set-key (kbd "C-c r") 'replace-string) ; non-qery replace keybinding
(global-set-key (kbd "C-x C-b") 'ibuffer)      ; better overview of buffers
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; avoid long confirmations
(defalias 'yes-or-no-p 'y-or-n-p)

;; Setup package archives
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; improved package menu
(require 'paradox)
(paradox-enable)


(require 'auto-complete)
(global-auto-complete-mode t)
;; fix autocomplete
(define-key ac-completing-map [return] nil)
(define-key ac-completing-map "\r" nil)


;; Rust
; Cargo mode for rust files
(add-hook 'rust-mode-hook 'cargo-minor-mode)
; racer autocomletion using auto-comlete
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'ac-racer-setup)


;; Make Magit and GitHub work together like a charm
(require 'magithub)


;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-s-s C-s-s") 'mc/edit-lines)
(global-set-key (kbd "s-n") 'mc/mark-next-like-this)      
(global-set-key (kbd "s-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c s-n") 'mc/mark-all-like-this)




;; Set the font to Fira Code
;; this is not as easy as it sounds
; set font: Fira Code

;; (add-to-list 'default-frame-alist ; works, but size is difficult
;; 	     '(font . "Fira Code"))

;; Fira Code in size 10.5 pt
;; this is the height attribute of XFLD (X Logical Font Description)
(add-to-list 'default-frame-alist
	     '(font . "-*-Fira Code-*-*-*-*-*-105-*-*-*-*-*-*"))
;; font support: use the superior Fira Code font
; enable for daemon and emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
; enable without server/client
(set-fontset-font t '(#Xe100 . #Xe16f)
		  (font-spec :font "Fira Code Symbol"
			     :height 105))

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("[^%]\\(%%\\)[^%]"            #Xe16a) ;does not work at the
						;beginning of a line
					;anymore
	    ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
	    ("0\\(x\\)[0-9]"               #Xe16b) ; not exactly what we
						 ; want but a cheap
						 ; replacement for
						 ; main feature
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

;; this function enables the ligatures
(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

;; for the programming mode
(add-hook 'prog-mode-hook
	  #'add-fira-code-symbol-keywords)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;; SLIME
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))


;; Für den semantic-mode
(global-ede-mode t)                      ; Enable the Project management system
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(require 'semantic/bovine/gcc)
(semantic-mode 1)
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; semantic include paths for cuda
(semantic-add-system-include "/usr/include/nvidia-384" 'c-mode)


;; IPython
(require 'python)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

;; More Python
; Jedi for autocompletion
; does not work anymore
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
; EIN and Jedi
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
; Jedi with Python3
(setq jedi:environment-root "jedi")  ; or any other name you like
(setq py-python-command "/usr/bin/python3")

;; (setq jedi:environment-virtualenv
;;      (append python-environment-virtualenv
;;              ("--python" "/usr/bin/python3")))




;; Julia
;; make julia-mode and julia-shell-mode (e.g. run-julia) work together
(require 'julia-mode)
(require 'julia-shell)
;;(defun my-julia-mode-hooks ()
;;  (require 'julia-shell-mode))
;;(add-hook 'julia-mode-hook 'my-julia-mode-hooks)

;; these keybindings are inspired by SLIME, very similar to many other
;; packages that deal with interpreted languages
(define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
(define-key julia-mode-map (kbd "C-c C-r") 'julia-shell-run-region)
(define-key julia-mode-map (kbd "C-c C-k") 'julia-shell-eval-buffer)
(define-key julia-mode-map (kbd "C-c M-n") 'julia-shell-reset-julia)
(define-key inferior-julia-shell-mode-map (kbd "C-c M-n") 'julia-shell-reset-workspace) ;for use in shell buffer
;; this function should be in julia-shell.el
;; It resets the workspace, i.e. deletes all variables and functions,
;; then it loads the emacstools library for proper interaction with julia-shell.el
(defun julia-shell-reset-workspace ()
  "reset the Julia workspace, run worskpace() and reload the emacs-init file"
  (interactive)
  (let ((julia-emacsinit (expand-file-name "julia-shell-emacstools.jl" (file-name-directory (locate-library "julia-shell"))))
	(julia-shell-buffer (julia-shell-buffer-or-complain)))
    (comint-send-string (get-buffer-process julia-shell-buffer)
			(format "workspace();include(\"%s\")" julia-emacsinit))))
;; this function should be in julia-shell.el
;; It is quite the same as
;; julia-shell-save-and-go but as the name suggests without the save step
(defun julia-shell-eval-buffer ()
  "eval this buffer in a Julia shell."
  (interactive)
    (let ((julia-shell-buffer (julia-shell-buffer-or-complain))
        (filename (buffer-file-name))
        (last-cmd nil)
        (last-cmd-with-prompt nil)
        (inhibit-field-text-motion t))
    (with-current-buffer julia-shell-buffer
      (if (not (julia-shell-on-prompt-p))
          (error "Julia shell is busy!")
        (beginning-of-line)
        (setq last-cmd-with-prompt
              (buffer-substring (point) (line-end-position)))
        (setq last-cmd (replace-regexp-in-string
                        julia-shell-prompt-regexp "" last-cmd-with-prompt))
        (delete-region (point) (line-end-position))
        (comint-simple-send (get-buffer-process (current-buffer))
                            (format "include(\"%s\")" filename))
        (goto-char (point-max))
        (insert last-cmd)
        (goto-char (point-max))))))


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some maybe useful instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq make-backup-files nil) ; stop creating those backup~ files
;; (setq auto-save-default nil) ; stop creating those #autosave# files
;; (transient-mark-mode 1) ; highlight text selection
;; (delete-selection-mode 1) ; delete seleted text when typing


;auctex LuaLaTeX
;(eval-after-load "tex"
;  '(add-to-list 'Tex-command-list
;		'("LuaLaTex" "lualatex %s" TeX-run-comman t t :help "Run LuaLaTex") t))
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
     (output-pdf "Okular")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("e9cbee60387b249e622a961db3f51a77d11ce1a5d735c8f6264f9a3541faf60b" "b462e2411830a39699856dcce0a72f9b11e6952dd07be5c65ae5f2f91eea25f1" "bf21a33d9f35ee10c2378ce999424002836ac3f6bdc2c94f2396ad44ce32c998" "b587774bd67083d98778e40b093ba822d25b5f842aaf95116015d4dbd624b5d1" "00e0c2f0373582a2bf6df1e63eddc05d7eb2ba7a7688b175d13c7e9ef53eeef6" default)))
 '(display-time-mode t)
 '(font-latex-fontify-script nil)
 '(package-selected-packages
   (quote
    (magithub nyan-mode paradox multiple-cursors ac-c-headers ac-math ac-racer toml-mode ac-octave auto-complete-c-headers ssh slime-volleyball slime-theme slime-ritz slime-docker slime-annot python3-info python-info python-docstring org matlab-mode markdown-mode magit julia-shell jedi-direx google-maps german-holidays ess-view ess-smart-underscore ess-smart-equals ess-R-object-popup ess-R-data-view ein-mumamo cython-mode cuda-mode cargo calfw c-eldoc auctex-lua auctex-latexmk aes ac-slime)))
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
