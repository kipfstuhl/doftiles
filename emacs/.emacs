;; keep this here, as the package system is needed to find the
;; libraries

;; Add package archives and initialize the package system
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;fixes some problems with elpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Improved package menu with paradox
(require 'paradox)
(paradox-enable)



;; Julia
;; make julia-mode and julia-shell-mode (e.g. run-julia) work together



;; these keybindings are inspired by SLIME, very similar to many other
;; packages that deal with interpreted languages
;; (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; (define-key julia-mode-map (kbd "C-c C-r") 'julia-shell-run-region)
;; (define-key julia-mode-map (kbd "C-c C-k") 'julia-shell-eval-buffer)
;; (define-key julia-mode-map (kbd "C-c M-n") 'julia-shell-reset-julia)
;; (define-key inferior-julia-shell-mode-map
;;   (kbd "C-c M-n") 'julia-shell-reset-workspace) ;for use in shell buffer
;; (define-key julia-mode-map (kbd "C-c d") 'julia-shell-show-documentation)
;; (define-key inferior-julia-shell-mode-map (kbd "C-c d") 'julia-shell-show-documentation)

;; this function should be in julia-shell.el
;; It resets the workspace, i.e. deletes all variables and functions,
;; then it loads the emacstools library for proper interaction with julia-shell.el
;; (defun julia-shell-reset-workspace ()
;;   "reset the Julia workspace, run worskpace() and reload the emacs-init file"
;;   (interactive)
;;   (let ((julia-emacsinit
;; 	 (expand-file-name "julia-shell-emacstools.jl"
;; 			   (file-name-directory (locate-library "julia-shell"))))
;; 	(julia-shell-buffer (julia-shell-buffer-or-complain)))
;;     (comint-send-string (get-buffer-process julia-shell-buffer)
;; 			(format "workspace();include(\"%s\")" julia-emacsinit))))

;; this function should be in julia-shell.el
;; It is quite the same as
;; julia-shell-save-and-go but as the name suggests without the save step
;; (defun julia-shell-eval-buffer ()
;;   "eval this buffer in a Julia shell."
;;   (interactive)
;;     (let ((julia-shell-buffer (julia-shell-buffer-or-complain))
;;         (filename (buffer-file-name))
;;         (last-cmd nil)
;;         (last-cmd-with-prompt nil)
;;         (inhibit-field-text-motion t))
;;     (with-current-buffer julia-shell-buffer
;;       (if (not (julia-shell-on-prompt-p))
;;           (error "Julia shell is busy!")
;;         (beginning-of-line)
;;         (setq last-cmd-with-prompt
;;               (buffer-substring (point) (line-end-position)))
;;         (setq last-cmd (replace-regexp-in-string
;;                         julia-shell-prompt-regexp "" last-cmd-with-prompt))
;;         (delete-region (point) (line-end-position))
;;         (comint-simple-send (get-buffer-process (current-buffer))
;;                             (format "include(\"%s\")" filename))
;;         (goto-char (point-max))
;;         (insert last-cmd)
;;         (goto-char (point-max))))))

;; this function should be in julia-shell.el
;; it Displays the Julia documentation in a
;; temporary buffer
;; because I am quite lazy and don't want to create a new
;; temporary buffer that is handled the correct way (closing
;; after pressing enter and things like this) misuse the
;; buffer for completions -> this is not good code
;; (defun julia-shell-show-documentation ()
;;   "show documentaton for word at point"
;;   (interactive)
;;   ;; this code is more or less copied from julia-shell.el, i.e.
;;   ;; matlab.el
;;   (let ((doc-output (julia-shell-collect-command-output
;; 		    (format "@doc(%s)" (thing-at-point 'word 'no-properties)))))
;;     (when doc-output
;;       (if (get-buffer-window "*Completions*")
;; 	  nil
;; 	(setq julia-shell-window-exists-for-display-completion-flag
;; 	      (if (eq (next-window) (selected-window))
;; 		  'delete
;; 		'bury)))
;;       (with-output-to-temp-buffer "*Completions*"
;; 	(print doc-output)))))



;;auctex LuaLaTeX
;;(eval-after-load "tex"
;;  '(add-to-list 'Tex-command-list
;;		'("LuaLaTex" "lualatex %s" TeX-run-comman t t :help "Run LuaLaTex") t))

(setq vc-follow-symlinks t)             ;follow links without asking

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(org-babel-load-file (locate-user-emacs-file "config.org"))
