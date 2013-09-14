#lang racket/base

(require "mathlink.ss"
         "translation.ss"
         racket/contract
         ffi/unsafe/custodian)

(provide/contract (MathKernel
                   (->* () () #:rest (listof string?) any))
                  (MathEval
                   (->* ((flat-rec-contract Mexp
                           number? boolean? symbol? string? void? eof-object?
                           (vectorof Mexp #:flat? #t)
                           (cons/c Mexp (listof Mexp))))
                        (MathLink?)
                        any))
                  (MathExit
                   (->* () (MathLink?) any))
                  (current-mathlink
                   (parameter/c (or/c false/c MathLink?))))
(provide MathLink?
         (struct-out exn:fail:mathlink))

(define current-mathlink
  (make-parameter #f))

(define flush-input
  (let ([buf (make-bytes 64)])
    (lambda ()
      (do ()
        ((zero? (read-bytes-avail!* buf)))))))

(define (MLPutSymbol lp sym)
  (let* ((str (symbol->string sym))
         (lst (string->list str)))
    (if (and (not (null? lst))
             (or (char-alphabetic? (car lst))
                 (char=? (car lst) #\$))
             (andmap (lambda (c)
                       (or (char-alphabetic? c)
                           (char=? c #\$)
                           (char-numeric? c)))
                     (cdr lst)))
        (MLPutNext lp 35)
        (MLPutFunction lp #"Symbol" 1))
    (MLPutString lp str)))

(define (MLPutNumber lp num)
  (if (real? num)
      (if (exact? num)
          (if (integer? num)
              (and (MLPutNext lp 43)
                   (MLPutString lp (number->string num)))
              (and (MLPutFunction lp #"Rational" 2)
                   (MLPutNumber lp (numerator num))
                   (MLPutNumber lp (denominator num))))
          (MLPutReal lp num))
      (and (MLPutFunction lp #"Complex" 2)
           (MLPutNumber lp (real-part num))
           (MLPutNumber lp (imag-part num)))))

(define (MLGetReal lp)
  (string->number
   (regexp-replace #rx"\\*|\\^"
                   (regexp-replace #rx"(\nul|\\`)[^(e|\\^)]*"
                                   (MLGetString lp)
                                   "")
                   "e")))

(define (MLPut lp exp)
  (cond ((boolean? exp)
         (MLPutSymbol lp (if exp 'True 'False)))
        ((number? exp)
         (MLPutNumber lp exp))
        ((symbol? exp)
         (MLPutSymbol lp exp))
        ((string? exp)
         (MLPutString lp exp))
        ((void? exp)
         (MLPutSymbol lp 'Null))
        ((eof-object? exp)
         (MLPutSymbol lp 'EndOfFile))
        ((vector? exp)
         (MLPutFunction lp #"List" (vector-length exp))
         (andmap (lambda (arg) (MLPut lp arg)) (vector->list exp)))
        (else
         (let ((mexp (Scheme->Mathematica exp)))
           (MLPutNext lp 70)
           (MLPutArgCount lp (sub1 (length mexp)))
           (andmap (lambda (arg) (MLPut lp arg)) mexp)))))

(define (MLGet lp)
  (with-handlers ((exn:break?
                   (lambda _ (MLPutMessage lp 3))))
    (MLFlush lp)
    (MLWait lp (break-enabled)))
  (unless (zero? (MLError lp))
    (mathlink-error (MLErrorMessage lp)))
  (case (MLNextPacket lp)
    ((0)
     (unless (MLClearError lp)
       (mathlink-error "MathEval: MathLink fatal error"))
     (MLNewPacket lp)
     (MLGet lp))
    ((1)
     (display (MLGetString lp))
     (flush-input)
     (MLPut lp (read))
     (MLGet lp))
    ((2)
     (display (MLGetString lp))
     (MLNewPacket lp)
     (MLGet lp))
    ((3)
     (MLGetExp lp))
    ((5)
     (MLNewPacket lp)
     (MLNextPacket lp)
     (warning (MLGetString lp))
     (MLNewPacket lp)
     (MLGet lp))
    ((21)
     (display (MLGetString lp))
     (flush-input)
     (MLPut lp (read-line))
     (MLNewPacket lp)
     (MLGet lp))
    (else
     (MLNewPacket lp)
     (MLGet lp))))

(define (MLGetExp lp)
  (case (MLGetNext lp)
    ((35)
     (let ((sym (MLGetSymbol lp)))
       (case sym
         ((True) #t)
         ((False) #f)
         ((Null) (void))
         ((Indeterminate) +nan.0)
         ((EndOfFile) eof)
         (else sym))))
    ((34)
     (MLGetString lp))
    ((43)
     (MLGetInteger lp))
    ((42)
     (MLGetReal lp))
    ((70)
     (Mathematica->Scheme
      (build-list (add1 (MLGetArgCount lp))
                  (lambda _ (MLGetExp lp)))))))

(define MathKernel
  (case-lambda
    (()
     (MathKernel "-linkname"
                 (case (system-type 'os)
                   ((unix)
                    "math -mathlink")
                   ((windows)
                    "MathKernel -mathlink")
                   ((macosx)
                    "/Applications/Mathematica.app/Contents/MacOS/MathKernel -mathlink"))))
    (arg
     (let ((link (apply MLOpen arg)))
       (set-MathLink-ref! link (register-custodian-shutdown link MLClose #:at-exit? #t))
       (current-mathlink link)
       link))))

(define (MathEval exp (link (or (current-mathlink) (MathKernel))))
  (call-with-semaphore (MathLink-sema link)
                       (lambda ()
                         (unless (MathLink-ref link)
                           (mathlink-error "MathEval: MathLink is closed"))
                         (let ((lp (MathLink-lp link)))
                           (MLPutFunction lp #"EvaluatePacket" 1)
                           (MLPut lp exp)
                           (MLEndPacket lp)
                           (MLGet lp)))))

(define MathExit
  (case-lambda
    (()
     (cond ((current-mathlink) => MathExit)))
    ((link)
     (call-with-semaphore (MathLink-sema link)
                          (lambda ()
                            (when (eq? link (current-mathlink))
                              (current-mathlink #f))
                            (let ((ref (MathLink-ref link)))
                              (when ref
                                (set-MathLink-ref! link #f)
                                (unregister-custodian-shutdown link ref)
                                (MLClose link))))))))
