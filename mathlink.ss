#lang racket/base

(require ffi/unsafe)

(provide (except-out (all-defined-out)
                     mathlink))

(define-struct MathLink (ep lp (ref #:mutable) sema))

(define warning
  (get-ffi-obj "scheme_warning" #f
               (_fun (_bytes = #"%T") _scheme -> _void)))

(define-struct (exn:fail:mathlink exn:fail) () #:transparent)
(define-syntax-rule (mathlink-error str)
  (raise (make-exn:fail:mathlink str (current-continuation-marks))))

(define mathlink
  (ffi-lib (case (system-type 'os)
             ((unix)
              (string-append "libML"
                             (number->string (* (compiler-sizeof '*) 8))
                             "i3"))
             ((windows)
              (string-append "ml"
                             (number->string (* (compiler-sizeof '*) 8))
                             "i3"))
             ((macosx)
              "mathlink.framework/mathlink"))))

(define MLOpen
  (let ((MLInitialize
         (get-ffi-obj "MLInitialize" mathlink
                      (_fun (_pointer = #f) -> (p : _pointer)
                            -> (or p (mathlink-error "MathKernel: MathLink Initialize Error"))))))
    (get-ffi-obj "MLOpenArgcArgv" mathlink
                 (_fun args ::
                       (ep : _pointer = (MLInitialize))
                       (_int = (add1 (length args)))
                       ((_list i _string/locale) = (cons "MrMathematica" args))
                       (_ptr o _int)
                       -> (lp : _pointer)
                       -> (if lp
                              (begin (MLNextPacket lp)
                                     (MLNewPacket lp)
                                     (make-MathLink ep lp #t (make-semaphore 1)))
                              (mathlink-error "MathKernel: MathLink Open Error"))))))

(define MLClose
  (let ((MLClose
         (let ((close (get-ffi-obj "MLClose" mathlink
                                   (_fun _pointer -> _void))))
           (lambda (link)
             (MLPutMessage link 1)
             (close link))))
        (MLDeinitialize
         (get-ffi-obj "MLDeinitialize" mathlink
                      (_fun _pointer -> _void))))
    (lambda (link)
      (MLClose (MathLink-lp link))
      (MLDeinitialize (MathLink-ep link)))))

(define MLPutFunction
  (get-ffi-obj "MLPutFunction" mathlink
               (_fun _pointer _bytes _int -> _bool)))

(define MLPutArgCount
  (get-ffi-obj "MLPutArgCount" mathlink
               (_fun _pointer _int -> _bool)))

(define MLPutString
  (get-ffi-obj "MLPutUTF32String" mathlink
               (_fun _pointer (s : _string/ucs-4) (_int = (string-length s)) -> _bool)))

(define MLPutReal
  (get-ffi-obj "MLPutReal" mathlink
               (_fun _pointer _double -> _bool)))

(define MLPutNext
  (get-ffi-obj "MLPutNext" mathlink
               (_fun _pointer _int -> _bool)))

(define MLNextPacket
  (get-ffi-obj "MLNextPacket" mathlink
               (_fun _pointer -> _int)))

(define MLEndPacket
  (get-ffi-obj "MLEndPacket" mathlink
               (_fun _pointer -> _bool)))

(define MLNewPacket
  (get-ffi-obj "MLNewPacket" mathlink
               (_fun _pointer -> _bool)))

(define MLGetString
  (let ((release (get-ffi-obj "MLReleaseUTF32String" mathlink
                              (_fun _pointer _pointer _int -> _void)))
        (make (get-ffi-obj "scheme_make_sized_char_string" #f
                           (_fun _pointer _intptr _bool -> _scheme))))
    (get-ffi-obj "MLGetUTF32String" mathlink
                 (_fun (l : _pointer) (s : (_ptr o _pointer)) (len : (_ptr o _int)) -> _bool
                       -> (begin0 (make s len #t)
                                  (release l s len))))))

(define MLGetSymbol
  (let ((release (get-ffi-obj "MLReleaseUTF8Symbol" mathlink
                              (_fun _pointer _pointer _int -> _void)))
        (make (get-ffi-obj "scheme_intern_exact_symbol" #f
                           (_fun _pointer _int -> _scheme))))
    (get-ffi-obj "MLGetUTF8Symbol" mathlink
                 (_fun (l : _pointer) (s : (_ptr o _pointer)) (b : (_ptr o _int)) (_ptr o _int) -> _bool
                       -> (begin0 (make s b)
                                  (release l s b))))))

(define MLGetInteger
  (let ((release (get-ffi-obj "MLReleaseString" mathlink
                              (_fun _pointer _pointer -> _void)))
        (make (get-ffi-obj "scheme_read_bignum_bytes" #f
                           (_fun _pointer (_int = 0) (_int = 10) -> _scheme))))
    (get-ffi-obj "MLGetString" mathlink
                 (_fun (l : _pointer) (s : (_ptr o _pointer)) -> _bool
                       -> (begin0 (make s)
                                  (release l s))))))

(define MLGetNext
  (get-ffi-obj "MLGetNext" mathlink
               (_fun _pointer -> _int)))

(define MLGetArgCount
  (get-ffi-obj "MLGetArgCount" mathlink
               (_fun _pointer (n : (_ptr o _int)) -> _bool
                     -> n)))

(define MLFlush
  (get-ffi-obj "MLFlush" mathlink
               (_fun _pointer -> _bool)))

(define MLWait
  (let ((MLReady (ffi-obj-ref "MLReady" mathlink)))
    (get-ffi-obj "scheme_block_until_enable_break" #f
                 (_fun (_fpointer = MLReady) (_fpointer = #f) _pointer (_float = 0.0) _bool
                       -> _bool))))

(define MLPutMessage
  (get-ffi-obj "MLPutMessage" mathlink
               (_fun _pointer _int -> _bool)))

(define MLError
  (get-ffi-obj "MLError" mathlink
               (_fun _pointer -> _int)))

(define MLErrorMessage
  (get-ffi-obj "MLErrorMessage" mathlink
               (_fun _pointer -> _string/latin-1)))

(define MLClearError
  (get-ffi-obj "MLClearError" mathlink
               (_fun _pointer -> _bool)))
