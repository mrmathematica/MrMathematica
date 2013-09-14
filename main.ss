#lang racket/base

(require "light.ss"
         racket/gui/base
         racket/class)

(provide (all-from-out "light.ss")
         Mexp->image)

(define (Mexp->image exp . lp)
  (let ((r (apply MathEval `(ExportString ,exp "PNG") lp)))
    (if (string? r)
        (make-object image-snip% (open-input-bytes (string->bytes/latin-1 r)) 'png)
        r)))
