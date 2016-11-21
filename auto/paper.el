(TeX-add-style-hook
 "paper"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1in")))
   (TeX-run-style-hooks
    "latex2e"
    "report"
    "rep10"
    "geometry"
    "amsmath")
   (LaTeX-add-labels
    "cpresult"
    "npresult"
    "ppresult"
    "AICs"
    "MSEs"))
 :latex)

