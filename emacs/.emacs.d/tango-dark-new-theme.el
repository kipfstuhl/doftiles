;;; tango-dark-theme.el --- Tango-based custom theme for faces

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.
;; Copyright (C) 2018 Jonas Kipfstuhl

;; Authors: Chong Yidong <cyd@stupidchicken>
;;          Jan Moringen <jan.moringen@uni-bielefeld.de>
;;          Jonas Kipfstuhl <jonas.kipfstuhl@t-online.de>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; The colors in this theme come from the Tango palette, which is in
;; the public domain: http://tango.freedesktop.org/

;;; Code:

(deftheme tango-dark-new
  "Face colors using the Tango palette (dark background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.
Without ugly boxes but with Magit.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#50c003") (cham-3 "#4e9a30")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#d080c7") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e9b2e3")
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526"))

  (custom-theme-set-variables
   'tango-dark-new
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
				      ,blue-1 ,plum-1 ,blue-0 ,alum-1])
   ;; '(display-time-mode t)
   ;; '(blink-cursor nil)
   ;; '(fringe-mode (quote (nil . 0)))
   ;; '(column-number-mode)
   ;; '(inhibit-startup-message t))
   )
  
  (custom-theme-set-faces
    'tango-dark-new
    ;; Ensure sufficient contrast on low-color terminals.
    `(default ((((class color) (min-colors 4096))
		 (:foreground ,alum-3 :background ,alum-6))
		(((class color) (min-colors 256))
		  (:foreground ,alum-1 :background "#222"))
		(,class
		  (:foreground ,alum-1 :background "black"))))
    `(cursor ((,class (:background ,butter-3))))
    ;; Highlighting faces
    ;; `(fringe ((,class (:background ,alum-7))))
    `(fringe ((,class (:background ,alum-6))))
    `(highlight ((,class (:foreground ,alum-6 :background ,butter-3))))
    `(region ((,class (:background ,alum-5))))
    `(secondary-selection ((,class (:background ,blue-3))))
    `(isearch ((,class (:foreground ,alum-1 :background ,orange-3))))
    `(lazy-highlight ((,class (:background ,choc-3))))
    `(trailing-whitespace ((,class (:background ,red-3))))
    `(shadow ((,class (:foreground ,alum-4))))
    ;; Mode line faces
    `(mode-line ((,class
		   (:background ,alum-4 :foreground ,alum-7))))
    `(mode-line-inactive ((,class
			    (:background ,alum-5 :foreground ,alum-3))))
    `(compilation-mode-line-fail ((,class (:foreground ,red-3))))
    `(compilation-mode-line-run  ((,class (:foreground ,orange-3))))
    `(compilation-mode-line-exit ((,class (:foreground ,cham-3))))
    ;; Escape and prompt faces
    `(minibuffer-prompt ((,class (:foreground ,cham-2))))
    `(escape-glyph ((,class (:foreground ,butter-3))))
    `(error ((,class (:foreground ,red-0))))
    `(warning ((,class (:foreground ,orange-1))))
    `(success ((,class (:foreground ,cham-1))))
    ;; Font lock faces
    `(font-lock-builtin-face ((,class (:foreground ,plum-1))))
    `(font-lock-comment-face ((,class (:foreground ,cham-3))))
    `(font-lock-constant-face ((,class (:foreground ,plum-0))))
    `(font-lock-function-name-face ((,class (:foreground ,butter-3))))
    `(font-lock-keyword-face ((,class (:foreground ,cham-2))))
    `(font-lock-string-face ((,class (:foreground ,choc-2))))
    `(font-lock-type-face ((,class (:foreground ,blue-1))))
    `(font-lock-variable-name-face ((,class (:foreground ,orange-2))))
    ;; mu4e faces
    `(mu4e-title-face                ((,class (:weight bold))))
    `(mu4e-unread-face               ((,class (:foreground ,butter-3
						:weight bold))))
    `(mu4e-moved-face                ((,class (:inherit shadow))))
    `(mu4e-trashed-face             ((,class (:inherit shadow
						:strike-through t))))
    `(mu4e-draft-face                ((,class (:inherit font-lock-string-face))))
    `(mu4e-flagged-face              ((,class (:foreground ,red-3 :weight bold))))
    `(mu4e-replied-face              ((,class (:inherit shadow))))
    `(mu4e-forward-face              ((,class (:inherit shadow))))
    `(mu4e-header-face               ((,class (:inherit default))))
    `(mu4e-header-title-face         ((,class (:inherit default))))
    `(mu4e-header-highlight-face     ((,class (:background ,alum-5))))
    `(mu4e-header-marks-face         ((,class (:inherit shadow))))
    `(mu4e-header-key-face           ((,class (:inherit font-lock-builtin-face))))
    `(mu4e-header-value-face         ((,class (:inherit default))))
    `(mu4e-special-header-value-face ((,class (:inherit default))))
    `(mu4e-contact-face              ((,class (:inherit default :weight bold))))
    `(mu4e-highlight-face            ((,class (:inherit default :bold t))))
    `(mu4e-context-face              ((,class (:foreground ,butter-3 :bold t))))
    `(mu4e-modeline-face             ((,class (:inherit mode-line))))
    `(mu4e-compose-separator-face    ((,class (:inherit shadow))))
    `(mu4e-cited-1-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-cited-2-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-cited-3-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-cited-4-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-cited-5-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-cited-6-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-cited-7-face              ((,class (:inherit font-lock-comment-face))))
    `(mu4e-region-code               ((,class (:inherit mu4e-header-highlight-face))))
    `(mu4e-attach-number-face        ((,class (:inherit font-lock-string-face :bold nil))))
    ;; Helm
    `(helm-source-header             ((,class (:inherit default :weight bold
						:height 1.3))))
    `(helm-selection                 ((,class (:inherit highlight))))
    `(helm-match                     ((,class (:inherit lazy-highlight))))
    `(helm-action                    ((,class ())))
    `(helm-candidate-number          ((,class (:inherit mode-line))))
    `(helm-header-line-left-margin   ((,class (:inherit highlight))))
    `(helm-swoop-target-word-face    ((,class (:inherit lazy-highlight))))
    `(helm-swoop-target-line-face    ((,class (:inherit highlight))))
    `(helm-swoop-target-line-block-face
       ((,class (:inherit highlight))))
    ;; Button and link faces
    `(link ((,class (:underline t :foreground ,blue-1))))
    `(link-visited ((,class (:underline t :foreground ,blue-2))))
    ;; Gnus faces
    `(gnus-group-news-1 ((,class (:foreground ,plum-1))))
    `(gnus-group-news-1-low ((,class (:foreground ,plum-2))))
    `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
    `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
    `(gnus-group-news-3 ((,class (:foreground ,cham-1))))
    `(gnus-group-news-3-low ((,class (:foreground ,cham-2))))
    `(gnus-group-news-4 ((,class (:foreground ,plum-0))))
    `(gnus-group-news-4-low ((,class (:foreground ,choc-2))))
    `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
    `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
    `(gnus-group-news-low ((,class (:foreground ,butter-2))))
    `(gnus-group-mail-1 ((,class (:foreground ,plum-1))))
    `(gnus-group-mail-1-low ((,class (:foreground ,plum-2))))
    `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
    `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
    `(gnus-group-mail-3 ((,class (:foreground ,cham-1))))
    `(gnus-group-mail-3-low ((,class (:foreground ,cham-2))))
    `(gnus-group-mail-low ((,class (:foreground ,butter-2))))
    `(gnus-header-content ((,class (:weight normal :foreground ,butter-3))))
    `(gnus-header-from ((,class (:foreground ,butter-2))))
    `(gnus-header-subject ((,class (:foreground ,cham-1))))
    `(gnus-header-name ((,class (:foreground ,blue-1))))
    `(gnus-header-newsgroups ((,class (:foreground ,choc-2))))
    ;; Message faces
    `(message-header-name ((,class (:foreground ,blue-1))))
    `(message-header-cc ((,class (:foreground ,butter-3))))
    `(message-header-other ((,class (:foreground ,choc-2))))
    `(message-header-subject ((,class (:foreground ,cham-1))))
    `(message-header-to ((,class (:foreground ,butter-2))))
    `(message-cited-text ((,class (:foreground ,cham-1))))
    `(message-separator ((,class (:foreground ,plum-1))))
    ;; SMerge faces
    `(smerge-refined-change ((,class (:background ,blue-3))))
    ;; Ediff faces
    `(ediff-current-diff-A ((,class (:background ,alum-5))))
    `(ediff-fine-diff-A ((,class (:background ,blue-3))))
    `(ediff-even-diff-A ((,class (:background ,alum-5.5))))
    `(ediff-odd-diff-A ((,class (:background ,alum-5.5))))
    `(ediff-current-diff-B ((,class (:background ,alum-5))))
    `(ediff-fine-diff-B ((,class (:background ,choc-3))))
    `(ediff-even-diff-B ((,class (:background ,alum-5.5))))
    `(ediff-odd-diff-B ((,class (:background ,alum-5.5))))
    ;; Flyspell faces
    `(flyspell-duplicate ((,class (:underline ,orange-1))))
    `(flyspell-incorrect ((,class (:underline ,red-1))))
    ;; Semantic faces
    `(semantic-decoration-on-includes ((,class (:underline ,alum-4))))
    `(semantic-decoration-on-private-members-face
       ((,class (:background ,plum-3))))
    `(semantic-decoration-on-protected-members-face
       ((,class (:background ,choc-3))))
    `(semantic-decoration-on-unknown-includes
       ((,class (:background ,red-3))))
    `(semantic-decoration-on-unparsed-includes
       ((,class (:background ,alum-5.5))))
    `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
    `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))
    `(semantic-highlight-func-current-tag-face
       ((,class (:background ,alum-5.5))))
    ;; Magit faces
    `(magit-section-highlight ((,class (:background ,alum-5.5))))
    ;; Company
    `(company-tooltip ((,class (:inherit highlight))))
    `(company-tooltip-selection ((,class (:inherit lazy-highlight))))
    `(company-template-field ((,class (:inherit highlight))))
    `(company-scrollbar-bg ((,class (:background ,butter-2))))
    `(company-scrollbar-fg ((,class (:background ,red-3))))
    ;; Sly
    `(sly-mrepl-output-face ((,class (:inherit default))))
    `(sly-mode-line ((,class (:foreground ,plum-3 :weight bold))))
    `(sly-part-button-face ((,class (:foreground ,plum-1))))
    `(sly-stickers-empty-face ((,class (:background ,plum-2))))
    `(sly-stickers-armed-face ((,class (:background ,blue-2))))
    `(sly-stickers-recordings-face ((,class (:background ,cham-3))))
    `(sly-mrepl-prompt-face ((,class (:foreground ,choc-2))))
    `(sly-mrepl-note-face ((,class (:foreground ,cham-3))))
    ;; term colors
    `(term-color-black ((,class (:foreground ,alum-7))))
    `(term-color-red ((,class (:foreground ,red-0))))
    `(term-color-green ((,class (:foreground ,cham-2))))
    `(term-color-yellow ((,class (:foreground ,butter-2))))
    `(term-color-blue ((,class (:foreground ,blue-0))))
    `(term-color-magenta ((,class (:foreground ,plum-1))))
    `(term-color-cyan ((,class (:foreground ,blue-1))))
    `(term-color-white ((,class (:foreground ,alum-3))))
    ;; pulse faces, used also by semantic for jumps
    ;; `(pulse-highlight-face ((,class (:inherit highlight))))
    `(pulse-highlight-start-face ((,class (:inherit next-error))))
    ;; Org mode faces
    `(org-agenda-structure ((,class (:foreground ,choc-2))))
    `(org-agenda-clocking ((,class (:foreground ,choc-3))))
    ;; `(org-agenda-done ((,class (:foreground ,cham-3)))) ; maybe have to adjust this
    `(org-done ((,class (:foreground ,cham-1 :weight bold))))
    `(org-date ((,class (:foreground ,blue-0))))
    `(org-footnote ((,class (:foreground ,blue-0))))
    `(org-scheduled ((,class (:foreground ,cham-2))))
    `(org-scheduled-previously ((,class (:foreground ,choc-2))))
    `(org-scheduled-today ((,class (:foreground ,cham-2))))
    `(org-sexp-date ((,class (:foreground ,blue-0))))
    `(org-table ((,class (:foreground ,cham-2))))
    )
  ;; PDF View / PDF Tools
  ;; this is only a variable, but it is something that belongs to a theme
  (setf pdf-view-midnight-colors `(,alum-3 . ,alum-6))

  
  ;; set the variables at beginning for better appearance
  ;; (custom-theme-set-variables
  ;;  'tango-dark-new
  ;;  `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
  ;; 				      ,blue-1 ,plum-1 ,blue-0 ,alum-1])
  ;; '(display-time-mode t)
  ;; '(blink-cursor nil)
  ;; '(scroll-bar-mode nil)
  ;; '(tool-bar-mode nil)
  ;; '(menu-bar-mode nil)
  ;; '(fringe-mode (quote (nil . 0)))
  ;; '(inhibit-startup-message t)))
  )

(provide-theme 'tango-dark-new)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tango-dark-new-theme.el ends here
