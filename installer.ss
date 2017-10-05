(module installer mzscheme
  (require (lib "setup-extension.ss" "make"))
  (provide pre-installer)
  (define (pre-installer PLTHOME directory-path)
    (pre-install PLTHOME
                 directory-path
                 "ml.c"
                 PLTHOME
                 '()
                 '("ML")
                 '("ml32i2m")
                 '()
                 '()
                 '()
                 (lambda (t) (t))
                 #f)))