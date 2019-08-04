


(require 'outline)
;;;###autoload
(define-derived-mode my-outline-mode outline-mode "enhanced outline"
  "my very own enhanced text mode

This is based on outline mode. So all commands from outline mode
work as expected. It adjusts the outline regexp for better text
production with normal numbering. In addition it activates some
useful minor modes.

The following commands are available:
\\{my-outline-mode-map}"
  ;; Each paragraph is one line, a new line character separates the paragraphs.
  (setq-local paragraph-start ".*$")
  (setq-local paragraph-separate "[ \t\f]*$")
  ;; Use numbering according to german rules. This means at least one dot, but at
  ;; the end there is no dot, except for the top Level. The level of the
  ;; numbering is equivalent to the number of dot-separated parts.
  (setq-local outline-regexp "^[0-9]+\\.\\([0-9]+\\(\\.[0-9]+\\)*\\)?[[:space:]\\n]")
  (setq outline-level (lambda ()
                        (let* ((line-str (thing-at-point 'line t))
                               (number-str (first (split-string line-str nil t nil))))
                          (length (split-string number-str "\\." t)))))
  
  
  )

(add-hook 'my-outline-mode-hook 'flyspell-mode)
(add-hook 'my-outline-mode-hook 'flyspell-buffer t)
(add-hook 'my-outline-mode-hook 'visual-line-mode)
(add-hook 'my-outline-mode-hook 'visual-fill-column-mode)

(defcustom outline-cycle-emulate-tab nil
  "Where should `outline-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'outlines
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Everywhere except in headlines" t)))

;;;###autoload
(defun outline-cycle (&optional arg)
  "Visibility cycling for outline(-minor)-mode.
- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 3 states:
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states:
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does."
  (interactive "P")
  (setq deactivate-mark t)
  (cond
   ((equal arg '(4))       ; Run `outline-cycle' as if at the top of the buffer.
    (save-excursion
      (goto-char (point-min))
      (let ((current-prefix-argument nil))
        (outline-cycle nil))))
   (t
    (cond
     ((bobp) ;; Beginning of buffer: Global cycling
      (cond
       ((eq last-command 'outline-cycle-overview)
	;; We just created the overview - now do table of contents
	;; This can be slow in very large buffers, so indicate action
	(message "CONTENTS...")
	(save-excursion
	  ;; Visit all headings and show their offspring
	  (goto-char (point-max))
	  (catch 'exit
	    (while (and (progn (condition-case nil
				   (outline-previous-visible-heading 1)
				 (error (goto-char (point-min))))
			       t)
			(looking-at outline-regexp))
	      (show-branches)
	      (if (bobp) (throw 'exit nil))))
	  (message "CONTENTS...done"))
	(setq this-command 'outline-cycle-toc))
       ((eq last-command 'outline-cycle-toc)
	;; We just showed the table of contents - now show everything
	(show-all)
	(message "SHOW ALL")
	(setq this-command 'outline-cycle-showall))
       (t
	;; Default action: go to overview
	(let ((toplevel (cond
			 (current-prefix-arg (prefix-numeric-value current-prefix-arg))
			 ((save-excursion (beginning-of-line)
					  (looking-at outline-regexp))
			  (max 1 (funcall outline-level)))
			 (t 1))))
	  (hide-sublevels toplevel))
	(message "OVERVIEW")
	(setq this-command 'outline-cycle-overview))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) beg eoh eol eos)
	;; First, some boundaries
	(save-excursion
	  (outline-back-to-heading)           (setq beg (point))
	  (save-excursion (outline-next-line) (setq eol (point)))
	  (outline-end-of-heading)            (setq eoh (point))
	  (outline-end-of-subtree)            (setq eos (point)))
	;; Find out what to do next and set `this-command'
	(cond
	 ((= eos eoh)
	  ;; Nothing is hidden behind this heading
	  (message "EMPTY ENTRY"))
	 ((>= eol eos)
	  ;; Entire subtree is hidden in one line: open it
	  (show-entry)
	  (show-children)
	  (message "CHILDREN")
	  (setq this-command 'outline-cycle-children))
	 ((eq last-command 'outline-cycle-children)
	  ;; We just showed the children, now show everything.
	  (show-subtree)
	  (message "SUBTREE"))
	 (t
	  ;; Default action: hide the subtree.
	  (hide-subtree)
	  (message "FOLDED")))))

     ;; TAB emulation
     ((outline-cycle-emulate-tab)
      (indent-relative))
     (t
      ;; Not at a headline: Do indent-relative
      (outline-back-to-heading))))))

(defun outline-cycle-emulate-tab ()
  "Check if TAB should be emulated at the current position."
  ;; This is called after the check for point in a headline,
  ;; so we can assume we are not in a headline
  (if (and (eq outline-cycle-emulate-tab 'white)
	   (save-excursion
	     (beginning-of-line 1) (looking-at "[ \t]+$")))
      t
    outline-cycle-emulate-tab))

;;;###autoload
(defun outline-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
	      (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

(defun outline-cycle-prev-vis-heading ()
  "Cycle with point at previous visible heading"
  (interactive)
  (save-excursion
    (outline-previous-visible-heading 1)
    (outline-cycle)))


(define-key my-outline-mode-map (kbd "TAB") #'outline-cycle)
(define-key my-outline-mode-map (kbd "<backtab>") #'outline-cycle-prev-vis-heading)

(provide 'my-outline)
