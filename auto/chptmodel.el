(TeX-add-style-hook "chptmodel"
 (lambda ()
    (TeX-add-symbols
     "bthet"
     "bOne"
     "lambdaT"
     "bTheta")
    (TeX-run-style-hooks
     "amsmath"
     "psfig"
     "natbib"
     "sort"
     "longnamesfirst"
     "latex2e"
     "art11"
     "article"
     "11pt")))

