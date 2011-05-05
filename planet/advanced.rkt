;; Typed Advanced Student.

;; Copyright (c) 2008 David Van Horn
;; Licensed under the Academic Free License version 3.0

;; (at dvanhorn (dot ccs neu edu))

;; A typed sister language for the Advanced Student language of HtDP.

;; 2008.10.26 : Initial, incomplete, release.

#lang typed/racket

(require/typed lang/htdp-advanced
  [empty '()]
  #;[empty? (Any -> Boolean : '())]
  ;; Can't convert to a contract.
  #;[first (∀ (α β) ((Pair α (Listof β)) -> α))])

(: first (∀ (α β) ((Pair α (Listof β)) -> α)))
(define (first p)
  ;; A new age type system with 1950s-style names.
  (car p))

(: rest (∀ (α β) ((Pair α (Listof β)) -> (Listof β))))
(define (rest p)
  (cdr p))

(provide empty empty? first rest)
#|
(provide memq memv
         null null? pair?
         reverse add1)

;(provide first)
(: first (∀ (α β) ((Pair α (Listof β)) -> α)))
(define (first p)
  (car p))

;(provide rest)
(: rest (∀ (α β) ((Pair α (Listof β)) -> (Listof β))))
(define (rest p)
  (cdr p))

(provide first rest empty empty?)
|#

(require-typed-struct posn ([x : Number] [y : Number]) lang/posn)
(provide (struct-out posn))
(define-type-alias Posn posn)
(provide Posn)

(provide (rename-out [ta-define define]))
(define-syntax ta-define
  (syntax-rules ()
    [(ta-define (f x ...) e)
     (define (f x ...) e)]
    [(ta-define x e)
     (define x e)]))

(provide (rename-out [ta-define-struct define-struct]))
(define-syntax ta-define-struct
  (syntax-rules (:)
    [(ta-define-struct n ([x : t] ...))
     (define-struct: n ([x : t] ...) #:transparent)]))

;; See http://list.cs.brown.edu/pipermail/plt-scheme/2008-October/028008.html
(provide (rename-out [ta-if if]))
(define-syntax ta-if
  (syntax-rules ()
    [(ta-if e0 e1 e2)
     (if e0 ;(ann e0 Boolean)
         e1
         e2)]))

(provide (rename-out [ta-begin begin]))
(define-syntax ta-begin
  (syntax-rules ()
    [(ta-begin e0 e1 ...)
     (begin e0 e1 ...)]))

(provide (rename-out [ta-begin0 begin0]))
(define-syntax ta-begin0
  (syntax-rules ()
    [(ta-begin0 e0 e1 ...)
     (begin0 e0 e1 ...)]))

(provide (rename-out [ta-set! set!]))
(define-syntax ta-set!
  (syntax-rules ()
    [(ta-set! x e)
     (set! x e)]))

(provide (rename-out [ta-delay delay]))
(define-syntax ta-delay
  (syntax-rules ()
    [(ta-delay e)
     (delay e)]))

(provide (rename-out [ta-lambda lambda]))
(define-syntax ta-lambda
  (syntax-rules ()
    [(ta-lambda (x ...) e)
     (lambda (x ...) e)]))

(provide (rename-out [ta-lambda: lambda:]))
(define-syntax ta-lambda:
  (syntax-rules (:)
    [(ta-lambda: ([x : t] ...) e)
     (lambda: ([x : t] ...) e)]))

; Punt on local for the time being.
; (define-syntax local

(provide (rename-out [ta-letrec letrec]))
(define-syntax ta-letrec
  (syntax-rules ()
    [(ta-letrec: ((x e) ...) e0)
     (letrec ((x e) ...) e0)]))

(provide (rename-out [ta-letrec: letrec:]))
(define-syntax ta-letrec:
  (syntax-rules (:)
    [(ta-letrec: ([x : t e] ...) e0)
     (letrec: ([x : t e] ...) e0)]))

; Punt on shared for the time being.
; (define-syntax ta-shared

(provide (rename-out [ta-let let]))
(define-syntax ta-let
  (syntax-rules ()
    [(ta-let ((x e) ...) e0)
     (let ((x e) ...) e0)]
    [(ta-let f ((x e) ...) e0)
     (let f ((x e) ...) e0)]))
 
(provide (rename-out [ta-let: let:]))
(define-syntax ta-let:
  (syntax-rules (:)
    [(ta-let: ([x : t e] ...) e0)
     (let: ([x : t e] ...) e0)]
    [(ta-let: f : t0 ([x : t e] ...) e0)
     (let: f : t0 ([x : t e] ...) e0)]))
    
(provide (rename-out [ta-let* let*]))
(define-syntax ta-let*
  (syntax-rules ()
    [(ta-let* ((x e) ...) e0)
     (let* ((x e) ...) e0)]))
     
(provide (rename-out [ta-let*: let*:]))
(define-syntax ta-let*:
  (syntax-rules (:)
    [(ta-let*: ([x : t e] ...) e0)
     (let*: ([x : t e] ...) e0)]))

(provide (rename-out [ta-recur recur]))
(define-syntax ta-recur
  (syntax-rules ()
    [(ta-recur f ((x e) ...) e0)
     (let f ((x e) ...) e0)]))

(provide (rename-out [ta-recur: recur:]))
(define-syntax ta-recur:
  (syntax-rules (:)
    [(ta-recur: f ([x : t e] ...) e0)
     (let: f ([x : t e] ...) e0)]))

(provide (rename-out [ta-cond cond]))
(define-syntax ta-cond
  (syntax-rules (else)
    [(ta-cond (else a)) a]
    [(ta-cond (q0 a0) (q1 a1) ... (else a))
     (ta-if q0 a0 (ta-cond (q1 a1) ... (else a)))]
    [(ta-cond (q0 a0) ... (qn an))
     (ta-cond (q0 a0) ... (qn an) 
              (else 
               (error "cond: all question results were false")))]))

;; Should really do a compile time identifier? / number? check
;; on all of the cases.
(provide (rename-out [ta-case case]))
(define-syntax ta-case
  (syntax-rules (else)
    [(ta-case e ((c0 c1 ...) e0) ... (else en))
     (case (ann e (U Symbol Number))
       ((c0 c1 ...) e0) ... (else en))]
    [(ta-case e ((c0 c1 ...) e0) ...)
     (ta-case e ((c0 c1 ...) e0) ... 
              (else (error "case: the expression matched none of the choices")))]))

(provide (rename-out [ta-when when]))
(define-syntax ta-when
  (syntax-rules ()
    [(ta-when e0 e1)
     (when (ann e0 Boolean)
       e1)]))

(provide (rename-out [ta-unless unless]))
(define-syntax ta-unless
  (syntax-rules ()
    [(ta-unless e0 e1)
     (unless (ann e0 Boolean)
       e1)]))

(provide (rename-out [ta-and and]))
(define-syntax ta-and
  (syntax-rules ()
    [(ta-and e0 e1 e2 ...)
     (and (ann e0 Boolean) 
          (ann e1 Boolean)
          (ann e2 Boolean) ...)]))

(provide (rename-out [ta-or or]))
(define-syntax ta-or
  (syntax-rules ()
    [(ta-or e0 e1 e2 ...)
     (or (ann e0 Boolean)
         (ann e1 Boolean)
         (ann e2 Boolean) ...)]))

(define-syntax require/provide/typed
  (syntax-rules ()
    [(require/provide/typed m [x t] ...)
     (begin (provide x ...)
            (require/typed m (x t) ...))]))

(require/provide/typed lang/htdp-advanced
  [true #t]
  [false #f]
  [* (Number Number Number * -> Number)]
  [+ (Number Number Number * -> Number)]
  [- (Number Number * -> Number)]
  [/ (Number Number Number * -> Number)]
  [< (Number Number Number * -> Boolean)]
  [<= (Number Number Number * -> Boolean)]
  [= (Number Number Number * -> Boolean)]
  [> (Number Number Number * -> Boolean)]
  [>= (Number Number Number * -> Boolean)]
  [abs (Number -> Number)]
  [acos (Number -> Number)]
  [add1 (Number -> Number)]
  [angle (Number -> Number)]
  [asin (Number -> Number)]
  [atan (Number -> Number)]
  [ceiling (Number -> Integer)]
  [complex? (Any -> Boolean)]
  [conjugate (Number -> Number)]
  [cos (Number -> Number)]
  [cosh (Number -> Number)]
  [current-seconds (-> Integer)]
  [denominator (Number -> Integer)]
  [e Number]
  [even? (Integer -> Boolean)]
  [exact->inexact (Number -> Number)]
  [exact? (Number -> Boolean)]
  [exp (Number -> Number)]
  [expt (Number Number -> Number)]
  [floor (Number -> Integer)]
  [gcd (Integer Integer * -> Integer)]
  [imag-part (Number -> Number)]
  [inexact->exact (Number -> Number)]
  [inexact? (Number -> Boolean)]
  [integer->char (Integer -> Char)]
  [integer? (Any -> Boolean : Integer)]
  [lcm (Integer Integer * -> Integer)]
  [log (Number -> Number)]
  [magnitude (Number -> Number)]
  [make-polar (Number Number -> Number)]
  [max (Number Number * -> Number)]
  [min (Number Number * -> Number)]
  [modulo (Integer Integer -> Integer)]
  [negative? (Number -> Boolean)]
  [number->string (Number -> String)]
  [number? (Any -> Boolean : Number)]
  [numerator (Number -> Integer)]
  [odd? (Integer -> Boolean)]
  [pi Number]
  [positive? (Number -> Boolean)]
  [quotient (Integer Integer -> Integer)]
  [random (Integer -> Integer)]
  [rational? (Any -> Boolean)]
  [real-part (Number -> Number)]
  [real? (Any -> Boolean)]
  [remainder (Integer Integer -> Integer)]
  [round (Number -> Integer)]
  [sgn (Number -> Number)]
  [sin (Number -> Number)]
  [sinh (Number -> Number)]
  [sqr (Number -> Number)]
  [sqrt (Number -> Number)]
  [sub1 (Number -> Number)]
  [tan (Number -> Number)]
  [zero? (Number -> Boolean)]
  [boolean=? (Boolean Boolean -> Boolean)]
  [boolean? (Any -> Boolean : Boolean)]
  [false? (Any -> Boolean : #f)]
  [not (Boolean -> Boolean)]
  [symbol->string (Symbol -> String)]
  [symbol=? (Symbol Symbol -> Boolean)]
  [symbol? (Any -> Boolean : Symbol)]
  ;; [append (∀ (α) ((Listof α) * -> (Listof α)))]
  ;; [assq (∀ (α β) (α (Listof (Pair α β)) -> (U #f (Pair α β))))]
  
  ;; Ignore c{ad}*r for now.
  
  ;; What to do about cons, car, cdr and the rest of the polymorphic functions?
  
  [cons? (Any -> Boolean : (Pair Any Any))]
;;[empty? (Any -> Boolean : '())]
;;[length (∀ (α) (Listof α) -> Number)]
;;[list (∀ (α) (α * -> (Listof α)))]
;;[list* (∀ (α) (α * (Listof α))  -> (Listof α)))
;;[list-ref (All (a) ((Listof a) Integer -> a))]
  
  [char->integer (Char -> Integer)]
  [char-alphabetic? (Char -> Boolean)]
  [char-ci<=? (Char Char Char * -> Boolean)]
  [char-ci<? (Char Char Char * -> Boolean)]
  [char-ci=? (Char Char Char * -> Boolean)]
  [char-ci>=? (Char Char Char * -> Boolean)]
  [char-ci>? (Char Char Char * -> Boolean)]
  [char-downcase (Char -> Char)]
  [char-lower-case? (Char -> Boolean)]
  [char-numeric? (Char -> Boolean)]
  [char-upcase (Char -> Char)]
  [char-upper-case? (Char -> Boolean)]
  [char-whitespace? (Char -> Boolean)]
  [char<=? (Char Char Char * -> Boolean)]
  [char<? (Char Char Char * -> Boolean)]
  [char=? (Char Char Char * -> Boolean)]
  [char>=? (Char Char Char * -> Boolean)]
  [char>? (Char Char Char * -> Boolean)]
  [char? (Any -> Boolean : Char)]
  
  ;; Strings
  
  ;; ...
  [=~ (Number Number Number -> Boolean)]
  
  )

(provide member)
(: member (∀ (α) (α (Listof α) -> Boolean)))
(define (member x xs)
  (ormap (lambda: ([y : α]) (equal? x y)) xs))

; Punt on time for the time being (bugs in TS).
; (define-syntax ta-time 
        
;; 
