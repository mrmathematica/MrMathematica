#lang racket/base

(require ffi/unsafe)

(provide (except-out (all-defined-out)
                     mathlink))

(define-struct MathLink (ep (lp #:mutable) sema))

(define-struct (exn:fail:mathlink exn:fail) () #:transparent)
(define-syntax-rule (mathlink-error str)
  (raise (make-exn:fail:mathlink str (current-continuation-marks))))

(define mathlink
  (ffi-lib (case (system-type 'os)
             ((unix)
              (string-append "libWSTP"
                             (number->string (* (compiler-sizeof '*) 8))
                             "i4"))
             ((windows)
              (string-append "wstp"
                             (number->string (* (compiler-sizeof '*) 8))
                             "i4"))
             ((macosx)
              "wstp.framework/wstp"))))

(define MLOpen
  (let ((MLInitialize
         (get-ffi-obj 'WSInitialize mathlink
                      (_fun (_pointer = #f) -> (p : _pointer)
                            -> (or p (mathlink-error "MathKernel: MathLink Initialize Error"))))))
    (get-ffi-obj 'WSOpenArgcArgv mathlink
                 (_fun args ::
                       (ep : _pointer = (MLInitialize))
                       (_int = (add1 (length args)))
                       ((_list i _string/locale) = (cons "MrMathematica" args))
                       (_ptr o _int)
                       -> (lp : _pointer)
                       -> (if lp
                              (begin (MLNextPacket lp)
                                     (MLNewPacket lp)
                                     (make-MathLink ep lp (make-semaphore 1)))
                              (mathlink-error "MathKernel: MathLink Open Error"))))))

(define MLClose
  (let ((close (get-ffi-obj 'WSClose mathlink
                              (_fun _pointer -> _void)))
        (deinitialize
         (get-ffi-obj 'WSDeinitialize mathlink
                      (_fun _pointer -> _void))))
    (lambda (link)
      (let ((lp (MathLink-lp link)))
        (when lp
          (MLPutMessage (MathLink-lp link) 1)
          (close (MathLink-lp link))
          (deinitialize (MathLink-ep link))
          (set-MathLink-lp! link #f))))))

(define MLPutFunction
  (get-ffi-obj 'WSPutFunction mathlink
               (_fun _pointer _bytes _int -> _bool)))

(define MLPutArgCount
  (get-ffi-obj 'WSPutArgCount mathlink
               (_fun _pointer _int -> _bool)))

(define MLPutString
  (get-ffi-obj 'WSPutUTF32String mathlink
               (_fun (l s) ::
                     (l : _pointer)
                     (_string/ucs-4 = (string-append "\uFEFF" s))
                     (_int = (add1 (string-length s)))
                     -> _bool)))

(define MLPutReal
  (get-ffi-obj 'WSPutReal mathlink
               (_fun _pointer _double -> _bool)))

(define MLPutNext
  (get-ffi-obj 'WSPutNext mathlink
               (_fun _pointer _int -> _bool)))

(define MLNextPacket
  (get-ffi-obj 'WSNextPacket mathlink
               (_fun _pointer -> _int)))

(define MLEndPacket
  (get-ffi-obj 'WSEndPacket mathlink
               (_fun _pointer -> _bool)))

(define MLNewPacket
  (get-ffi-obj 'WSNewPacket mathlink
               (_fun _pointer -> _bool)))

(define MLGetString
  (case (system-type 'vm)
    ((racket)
     (let ((release (get-ffi-obj 'WSReleaseUTF32String mathlink
                                 (_fun _pointer _pointer _int -> _void)))
           (make (get-ffi-obj 'scheme_make_sized_char_string #f
                              (_fun _pointer _intptr _bool -> _scheme))))
       (get-ffi-obj 'WSGetUTF32String mathlink
                    (_fun (l : _pointer) (s : (_ptr o _pointer)) (len : (_ptr o _int)) -> _bool
                          -> (begin0 (make (ptr-add s 4) (sub1 len) #t)
                                     (release l s len))))))
    ((chez-scheme)
     (let ((release (get-ffi-obj 'WSReleaseUTF8String mathlink
                                 (_fun _pointer _pointer _int -> _void)))
           (make (get-ffi-obj 'Sstring_utf8 #f
                              (_fun _pointer _intptr -> _scheme))))
       (get-ffi-obj 'WSGetUTF8String mathlink
                    (_fun (l : _pointer) (s : (_ptr o _pointer)) (len : (_ptr o _int)) (_ptr o _int) -> _bool
                          -> (begin0 (make s len)
                                     (release l s len))))))))

(define MLGetNext
  (get-ffi-obj 'WSGetNext mathlink
               (_fun _pointer -> _int)))

(define MLGetArgCount
  (get-ffi-obj 'WSGetArgCount mathlink
               (_fun _pointer (n : (_ptr o _int)) -> _bool
                     -> n)))

(define MLWait
  (let ((MLFlush (get-ffi-obj 'WSFlush mathlink
                              (_fun _pointer -> _bool)))
        (MLReady (get-ffi-obj 'WSReady mathlink
                              (_fun _pointer -> _bool))))
    (lambda (lp)
      (MLFlush lp)
      (let loop ()
        (unless (MLReady lp)
          (sleep 0.01)
          (loop))))))

(define MLPutMessage
  (get-ffi-obj 'WSPutMessage mathlink
               (_fun _pointer _int -> _bool)))

(define MLError
  (get-ffi-obj 'WSError mathlink
               (_fun _pointer -> _int)))

(define MLErrorMessage
  (get-ffi-obj 'WSErrorMessage mathlink
               (_fun _pointer -> _string/latin-1)))

(define MLClearError
  (get-ffi-obj 'WSClearError mathlink
               (_fun _pointer -> _bool)))
