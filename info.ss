(module info (lib "infotab.ss" "setup")
  (define name "DrMathematica")
  (define blurb
    '("DrMathematica -- Union of Mathematica and Scheme."))
  (define homepage "http://www.neilvandyke.org/mrmathematica/")
  (define help-desk-message
    "Mz/Mr: (require (lib \"mathematica.ss\" \"mrmathematica\"))")
  (define compile-omit-files '("installer.ss"))
  (define pre-install-collection "installer.ss")
  (define tools '(("tool.ss")))
  (define tool-names '("DrMathematica"))
  (define tool-icons '(("mathematica.png" "drmathematica")))
  (define tool-urls '("http://www.neilvandyke.org/mrmathematica/")))
