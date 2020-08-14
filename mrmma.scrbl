#lang scribble/manual
@(require scribble/eval
          scheme/sandbox
          (for-syntax scheme/base)
          (for-label scheme
                     mrmathematica
                     scheme/gui/base
                     setup/dirs))

@title{@bold{MrMathematica}: Calling Mathematica from Scheme}
@author{Chongkai Zhu}

@margin-note{Mathematica is a registered trademark of @link["http://www.wolfram.com/"]{Wolfram Research, Inc.}}

MrMathematica allows you to call
@link["http://www.wolfram.com/products/mathematica/"]{Mathematica} from
@link["http://www.plt-scheme.org/"]{PLT Scheme}.

@section{Install MrMathematica}

Before installing MrMathematica, you need to do the following two things:

@itemize[@item{Install the WSTP dynamic library by copying it into the PLT
          libraries (see @scheme[get-lib-search-dirs]), or into a systemwide
          location such as @filepath{C:\Windows\SysWOW64}, @filepath{/lib},
          @filepath{/Library/Frameworks} or @filepath{~/Library/Frameworks}.
          @link["https://reference.wolfram.com/language/tutorial/WSTPDeveloperGuide-Windows.html#19602"]{On Windows}
          it is @filepath{wstp32i4.dll} or @filepath{wstp64i4.dll}, depending
          on whether you are on a 32-bit or 64-bit platform.
          @link["https://reference.wolfram.com/language/tutorial/WSTPDeveloperGuide-Unix.html#1726529900"]{On Unix/Linux},
          it is @filepath{libWSTP32i4.so} or @filepath{libWSTP64i4.so}.
          @link["https://reference.wolfram.com/language/tutorial/WSTPDeveloperGuide-Macintosh.html#669747147"]{On Mac OS X},
          it is @filepath{wstp.framework}.}
         
         @item{Make sure that @link["http://reference.wolfram.com/legacy/v8/ref/program/MathKernel.html"]{@exec{MathKernel}}
          or @link["http://reference.wolfram.com/legacy/v8/ref/program/math.html"]{@exec{math}}
          will launch Mathematica kernel. On Unix/Linux the Mathematica
          installer should have done that. On Windows setting @envvar{PATH} to
          include the directory where Mathematica executable files locate
          should be enough. On Mac OS, MrMathematica looks for
          @filepath{/Applications/Mathematica.app/Contents/MacOS/MathKernel},
          which should be the correct place.}]

Install @link["https://github.com/mrmathematica/MrMathematica/releases"]{mrmathematica.plt}
from File menu in DrScheme.

@section{Use MrMathematica}

@defmodule[mrmathematica]

@defproc[(MathKernel (arg string?) ...) MathLink?]{

Opens a MathLink connection. The arguments are passed to MathLink function
@link["http://reference.wolfram.com/language/ref/c/MLOpenArgcArgv.html"]{MLOpenArgcArgv}.
If no argument is given, @scheme["-linkname" "math -mathlink"] will be used as
default, which in general will launch a new local Mathematica kernel. Remote
MathKernel is supported.}

@defproc[(MathLink? (v any/c)) boolean?]{

Returns @scheme[#t] if @scheme[v] is a MathLink connection, @scheme[#f] otherwise.}

@defproc*[([(current-mathlink) (or false? MathLink?)]
           [(current-mathlink (v (or false? MathLink?))) void?])]{

A @tech["parameter" #:doc '(lib "scribblings/reference/reference.scrbl")] that
determines the default MathLink connection to use. @scheme[#f] indicates no
current connection. Each time @scheme[MathKernel] is called, the return value
will be automatically set as @scheme[current-mathlink].}

@defproc[(MathEval (Mexp (flat-rec-contract Mexp
                           number? boolean? symbol? string? void? eof-object?
                           (vectorof Mexp)
                           (cons/c Mexp (listof Mexp))))
                   (ml MathLink? (current-mathlink))) Mexp]{

@margin-note{@bold{Caution}: For floating point numbers, PLT Scheme only supports machine precision.}

Uses Mathematica to evaluate. You should write @scheme[Mexp] as an S-exp and it
will be translated to Mathematica style automatically. Only number, boolean,
symbol, string, void, eof, vector of Mexps, or none empty list of Mexps is
recognized as Mexp. See @secref{translation} for details.

The optional argument @scheme[ml] specifies which Mathematica kernel to be used
to do the computation. If no connection is given and @scheme[(current-mathlink)]
is @scheme[#f], @scheme[(MathKernel)] will be called to create one.}

@examples[#:eval (call-with-trusted-sandbox-configuration
                  (lambda ()
                    (parameterize ((sandbox-output 'string)
                                   (sandbox-error-output 'string))
                      (make-evaluator 'scheme/base #:requires
                                      '(mrmathematica/light)))))
          (MathEval '(Integrate (/ 1 (+ (expt x 2) 1)) x))
          (MathEval '(Integrate (/ 1 (+ (expt x 2) -1)) x))
          (MathEval '(Integrate (expt x 2) #(x 0 1)))
          (define f (MathEval '(Integrate (expt x 2) x)))
          f
          (define s (eval `(lambda (x) ,f) (make-base-namespace)))
          (- (s 1) (s 0))
          (define (factor-integer n)
            (MathEval `(FactorInteger ,n)))
          (factor-integer 111111111111111111)]

@scheme[MathEval] is thread-safe.
@tech["Breaks" #:doc '(lib "scribblings/reference/reference.scrbl")] during
@scheme[MathEval] are forwarded to Mathematica.

@defproc[(Mexp->image (Mexp Mexp) (ml MathLink? (current-mathlink)))
         (is-a?/c image-snip%)]{

Converts @scheme[Mexp] to an image. For example:}

@schemeinput[(Mexp->image '(Plot (sin x) #(x 0 (* 2 Pi))))]

@defstruct[(exn:fail:mathlink exn:fail) () #:transparent]{

Raised for MathLink error.}

@defproc[(MathExit (ml MathLink? (current-mathlink))) void?]{

Closes MathLink connection @scheme[ml]. If @scheme[ml] is already closed,
@scheme[MathExit] has no effect.}

@section{Use MrMathematica without MrEd}

@defmodule[mrmathematica/light]

The same as @scheme[(require #,(schememodname mrmathematica))], for command line
use in case @other-manual['(lib "scribblings/gui/gui.scrbl")] is not available.
It doesn't provide @scheme[Mexp->image].

@section[#:tag "translation"]{Translation between Scheme and Mathematica}

S-exp such as @scheme[(f x y)] is automatically translated to @litchar{f[x,y]}
and send to Mathematica Kernel. Scheme vector such as @scheme[#(1 2)] is
translated to Mathematica list @litchar{{1,2}}. The return expression of
Mathematica is translated back into Scheme. Besides, MrMathematica also use the
following dictionary to translate functions between Scheme and Mathematica:

@(define-syntax (md stx)
   (syntax-case stx ()
     ((_ id)
      #`(link #,(string-append "http://reference.wolfram.com/mathematica/ref/"
                               (symbol->string (syntax-e #'id))
                               ".html")
              (scheme id)
              #:underline? #f))))

@schemeblock[
 ((* . #,(md Times))
  (- . #,(md Minus))
  (+ . #,(md Plus))
  (/ . #,(md Divide))
  (< . #,(md Less))
  (<= . #,(md LessEqual))
  (= . #,(md Equal))
  (> . #,(md Greater))
  (>= . #,(md GreaterEqual))
  (abs . #,(md Abs))
  (acos . #,(md ArcCos))
  (and . #,(md And))
  (angle . #,(md Arg))
  (asin . #,(md ArcSin))
  (atan . #,(md ArcTan))
  (begin . #,(md CompoundExpression))
  (ceiling . #,(md Ceiling))
  (cos . #,(md Cos))
  (denominator . #,(md Denominator))
  (exp . #,(md Exp))
  (expt . #,(md Power))
  (floor . #,(md Floor))
  (gcd . #,(md GCD))
  (if . #,(md If))
  (imag-part . #,(md Im))
  (lcm . #,(md LCM))
  (list . #,(md List))
  (log . #,(md Log))
  (magnitude . #,(md Abs))
  (max . #,(md Max))
  (min . #,(md Min))
  (modulo . #,(md Mod))
  (negative? . #,(md Negative))
  (not . #,(md Not))
  (number? . #,(md NumberQ))
  (numerator . #,(md Numerator))
  (or . #,(md Or))
  (positive? . #,(md Positive))
  (quotient . #,(md Quotient))
  (rationalize . #,(md Rationalize))
  (round . #,(md Round))
  (sin . #,(md Sin))
  (sqrt . #,(md Sqrt))
  (string-length . #,(md StringLength))
  (tan . #,(md Tan))
  (truncate . #,(md IntegerPart)))]

The translation table is defined in @filepath{translation.ss}. You can change
this file to adapt for your special needs, say you just want no translation, or,
you want some more functions that are similar in Mathematica and Scheme also to
be automatically translated. Please also note that the right hand side of the
table can also be a function that takes the all the arguments as input, thus
enable arbitrary translation rule. See the @scheme[-] and @md[Rational] rule in
@filepath{translation.ss} as examples.
