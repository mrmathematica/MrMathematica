(module installer mzscheme
  (require (lib "setup-extension.ss" "make"))
  (provide pre-installer)
  (define (pre-installer PLTHOME)
    (pre-install PLTHOME
                 (collection-path "mrmathematica")
                 "ml.c"
                 PLTHOME
                 '()
                 '("ML")
                 '()
                 '()
                 '("ML32I2M.lib")
                 '()
                 (lambda (t) (t)))))