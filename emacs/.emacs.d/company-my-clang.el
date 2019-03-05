(require 'cl)
(require 'company-clang)

(defun company-my-clang (command &optional arg &rest ignored)
  ""
  (interactive (list 'interactive))
  (cl-case command
    (interactive (comapny-begin-backend 'company-my-clang))
    (init (company-clang command arg ignored))
    (prefix (company-clang command arg ignored))
    (candidates (company-clang command arg ignored))
    (meta (company-clang command arg ignored))
    (annotation (company-clang command arg ignored))))

(provide 'company-my-clang)
