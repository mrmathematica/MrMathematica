#cs(module mathematica mzscheme
     
     (require "ml.ss"
              "translation.ss")
     
     (provide MathKernel
              MathEval
              MathExit
              (rename mathlink? MathLink?)
              current-mathlink
              math-break-enabled
              exn:mathlink)
     
     (define-struct (exn:mathlink exn) (link))
     
     (define current-mathlink
       (make-parameter #f
                       (lambda (lp)
                         (unless (or (not lp)
                                     (mathlink? lp))
                           (raise-type-error 'current-mathlink "MathLink/#f" lp))
                         lp)))
     
     (define math-break-enabled
       (make-parameter #t))
     
     (define (MathPutSymbol sym lp)
       (let* ((str (symbol->string sym))
              (lst (string->list str)))
         (if (and (not (null? lst))
                  (char-alphabetic? (car lst))
                  (andmap (lambda (c)
                            (or (char-alphabetic? c)
                                (char-numeric? c)))
                          (cdr lst)))
             (mathputnext 35 lp)
             (mathputfunction 'Symbol 1 lp))
         (mathputstring str lp)))
     
     (define (MathPut exp lp)
       (cond ((boolean? exp)
              (MathPutSymbol (if exp 'True 'False) lp))
             ((number? exp)
              (MathPutNumber exp lp))
             ((symbol? exp)
              (MathPutSymbol exp lp))
             ((string? exp)
              (mathputstring exp lp))
             ((void? exp)
              (MathPutSymbol 'Null lp))
             ((and (list? exp)
                   (not (null? exp)))
              (let ((mexp (Scheme->Mathematica exp)))
                (mathputnext 70 lp)
                (mathputargcount (sub1 (length mexp)) lp)
                (andmap (lambda (arg) (MathPut arg lp)) mexp)))
             (else
              (mathendpacket lp)
              (MathGet lp
                       (lambda (dummy)
                         (raise-type-error 'MathEval "number/boolean/symbol/string/void/list" exp))))))
     
     (define (MathPutInteger num lp)
       (mathputnext 43 lp)
       (mathputstring (number->string num) lp))
     
     (define (MathPutNumber num lp)
       (cond ((and (integer? num)
                   (exact? num))
              (MathPutInteger num lp))
             ((and (rational? num)
                   (exact? num))
              (mathputfunction 'Rational 2 lp)
              (MathPutInteger (numerator num) lp)
              (MathPutInteger (denominator num) lp))
             ((real? num)
              (mathputreal num lp))
             (else
              (mathputfunction 'Complex 2 lp)
              (MathPutNumber (real-part num) lp)
              (MathPutNumber (imag-part num) lp))))
     
     (define (MathGet lp k)
       (unless (zero? (matherror lp))
         (raise (make-exn:mathlink (string-append "MathLink Error: "
                                                  (matherrormessage lp))
                                   (current-continuation-marks)
                                   lp)))
       (case (mathnextpacket lp)
         ((0)
          (unless (mathclearerror lp)
            (raise (make-exn:mathlink "MathLink fatal error"
                                      (current-continuation-marks)
                                      lp))))
         ((1)
          (display (mathgetstring lp))
          (mathputfunction 'EnterExpressionPacket 1 lp)
          (MathPut (read) lp))
         ((2)
          (display (mathgetstring lp)))
         ((16 3)
          (let ((r (MathGetExp lp)))
            (MathGet lp
                     (lambda (dummy) (k r)))))
         ((5)
          (mathnewpacket lp)
          (mathnextpacket lp)
          (warning (mathgetstring lp)))
         ((6)
          (let ((InterruptMenu (string->number (mathgetstring lp))))
            (display (mathgetstring lp))
            (mathnewpacket lp)
            (when (zero? InterruptMenu)
              (mathnextpacket lp)
              (display (mathgetstring lp))))
          (MathPut (read-line) lp))
         ((8)
          (mathnewpacket lp)
          (k (void)))
         ((17)
          (display "MathKernel suspended\n"))
         ((18)
          (display "MathKernel resumed\n"))
         ((19)
          (printf "entering dialog: ~a\n" (string->number (mathgetstring lp))))
         ((20)
          (printf "leaving dialog: ~a\n" (string->number (mathgetstring lp))))
         ((21)
          (display (mathgetstring lp))
          (mathputfunction 'TextPacket 1 lp)
          (MathPut (read-line) lp)))
       (mathnewpacket lp)
       (MathGet lp k))
     
     (define (MathGetExp lp)
       (let ((next (mathgetnext lp)))
         (cond ((= next 35)
                (let ((sym (mathgetsymbol lp)))
                  (cond
                    ((eq? sym 'True) #t)
                    ((eq? sym 'False) #f)
                    ((eq? sym 'Null) (void))
                    ((eq? sym 'Indeterminate) +nan.0)
                    (else sym))))
               ((= next 34)
                (mathgetstring lp))
               ((= next 43)
                (string->number (mathgetstring lp)))
               ((= next 42)
                (mathgetreal lp))
               ((= next 70)
                (Mathematica->Scheme
                 (let* ((n (mathgetargcount lp))
                        (head (list (MathGetExp lp))))
                   (let loop ((i n) (p head))
                     (if (zero? i)
                         head
                         (begin
                           (set-cdr! p (list (MathGetExp lp)))
                           (loop (sub1 i) (cdr p)))))))))))
     
     (define MathKernel
       (case-lambda
         (()
          (MathKernel "-linkname" "math -mathlink"))
         (arg
          (unless (andmap string? arg)
            (raise-type-error 'MathKernel "strings" arg))
          (let ((lp (apply init_and_openlink arg)))
            (current-mathlink lp)
            (mathnextpacket lp)
            (mathnewpacket lp)
            lp))))
     
     (define MathEval
       (case-lambda
         ((exp)
          (MathEval exp (current-mathlink)))
         ((exp lp)
          (unless (mathlink? lp)
            (raise-type-error 'MathEval "MathLink" lp))
          (mathputfunction 'EnterExpressionPacket 1 lp)
          (MathPut exp lp)
          (mathendpacket lp)
          (if (math-break-enabled)
              (with-handlers ((exn:break?
                               (lambda (x)
                                 (mathputmessage (if (eq? (math-break-enabled)
                                                          'Interrupt)
                                                     2
                                                     3)
                                                 lp))))
                (let loop ()
                  (unless (mathready lp)
                    (sleep)
                    (loop)))))
          (let/cc k
            (MathGet lp k)))))
     
     (define MathExit
       (case-lambda
         (()
          (MathExit (current-mathlink)))
         ((lp)
          (unless (mathlink? lp)
            (raise-type-error 'MathExit "MathLink" lp))
          (if (eq? lp (current-mathlink))
              (current-mathlink #f))
          (mathputmessage 1 lp)
          (mathclose lp)))))