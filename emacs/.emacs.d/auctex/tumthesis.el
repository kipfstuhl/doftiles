
(TeX-add-style-hook
 "tumthesis"
 (function (lambda ()
             (TeX-run-style-hooks "scrbook" "scrbase" "ragged2e"
                                  "polyglossia" "graphicx" "tikz"
                                  "csquotes" "biblatex" "amsmath"
                                  "amssymb" "ntheorem" "tabularx"
                                  "booktabs" "xspace" "listings"
                                  "imakeidx" "footmics" "caption"
                                  "hyperref" "cleverref" "commath")
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
              '("Cpageref" TeX-arg-ref)))))
