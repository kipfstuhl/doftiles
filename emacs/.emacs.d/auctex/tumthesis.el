
(TeX-add-style-hook
 "tumthesis"
 (function (lambda ()
             (TeX-run-style-hooks "scrbook" "scrbase" "ragged2e"
                                  "polyglossia" "graphicx" "tikz"
                                  "csquotes" "biblatex" "amsmath"
                                  "amssymb" "ntheorem" "tabularx"
                                  "booktabs" "xspace" "listings"
                                  "imakeidx" "footmics" "caption"
                                  "hyperref" "cleverref" "commath" "fixme")
             (if (boundp 'reftex-ref-style-alist)
                 (add-to-list
                  'reftex-ref-style-alist
                  '("Cleveref" "cleveref"
                    (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
             (reftex-ref-style-activate "Cleveref")
             (TeX-add-symbols
              '("cref" TeX-arg-ref)
              '("Cref" TeX-arg-ref)
              '("cpageref" TeX-arg-ref)
              '("Cpageref" TeX-arg-ref))
             (LaTeX-add-environments
              '("theorem" LaTeX-env-label)
              '("lemma"   LaTeX-env-label)
              '("corollary" LaTeX-env-label)
              '("conjecture" LaTeX-env-label)
              '("remark"  LaTeX-env-label)
              '("definition" LaTeX-env-label)
              '("problem" LaTeX-env-label)
              '("example" LaTeX-env-label)
              '("proof"   LaTeX-env-label))
             (dolist (el '(("theorem" . "th:")
                           ("lemma"   . "th:")))
               (add-to-list 'LaTeX-label-alist el))
             (dolist (el '(("theorem" ?h "th:" "~\\ref{%s}" nil ("Theorem" "th.") nil)
                           ("lemma"   ?h "th:" "~\\ref{%s}" nil ("Theorem" "th.") nil)))
               (add-to-list 'reftex-label-alist el))
             ;; (add-to-list
             ;;  'reftex-label-alist
             ;;  '(("theorem" ?h "th:" "~\\ref{%s}" t ("theorem" "th."))))
             )))



