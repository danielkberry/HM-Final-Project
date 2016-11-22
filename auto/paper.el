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
    "amsmath"
    "graphicx")
   (LaTeX-add-labels
    "fig:desert"
    "fig:black"
    "fig:white"
    "fig:income"
    "fig:vacant"
    "cpresult"
    "npresult"
    "ppresult"
    "mlm"
    "AICs"
    "MSEs"))
 :latex)

