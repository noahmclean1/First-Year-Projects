#lang typed/racket

(: uchicago151-version : String)
(define uchicago151-version "A3.0")
(provide uchicago151-version)

(: uchicago151-date : String)
(define uchicago151-date "September 23, 2016")
(provide uchicago151-date)

(define-syntax uchicago151-define-struct
  (syntax-rules ()
    [(uchicago151-define-struct (name A ...) (fields ...))
     (define-struct (A ...) name (fields ...) #:transparent)]
    [(uchicago151-define-struct (name A ...) (fields ...) #:mutable)
     (define-struct (A ...) name (fields ...) #:transparent #:mutable)]
    [(uchicago151-define-struct name (fields ...) #:mutable)
     (define-struct name (fields ...) #:transparent #:mutable)]
    [(uchicago151-define-struct name (fields ...))
     (define-struct name (fields ...) #:transparent)]))
(provide (rename-out [uchicago151-define-struct define-struct]))
;; NOTE Make sure the CS tutors know about this change to define-struct

;; error reporting
;;
(: uchicago151-error : String -> Nothing)
(define (uchicago151-error msg) (error msg))
(provide (rename-out [uchicago151-error error]))

(: uchicago151-cons : (All (A) (-> A (Listof A) (Listof A))))
(define (uchicago151-cons hd tl) (cons hd tl))
(provide (rename-out [uchicago151-cons cons]))

(: uchicago151-first : (All (A) (Listof A) -> A))
(define (uchicago151-first xs) (first xs))
(provide (rename-out [uchicago151-first first]))

(: uchicago151-map : (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (uchicago151-map f xs) (map f xs))
(provide (rename-out [uchicago151-map map]))

(: uchicago151-filter : (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (uchicago151-filter f xs) (filter f xs))
(provide (rename-out [uchicago151-filter filter]))

(: uchicago151-foldl : (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (uchicago151-foldl f acc xs) (foldl f acc xs))
(provide (rename-out [uchicago151-foldl foldl]))

(: uchicago151-foldr : (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (uchicago151-foldr f acc xs) (foldr f acc xs))
(provide (rename-out [uchicago151-foldr foldr]))

(: uchicago151-partition :
   (All (A) (-> (-> A Boolean) (Listof A) (values (Listof A) (Listof A)))))
(define (uchicago151-partition f xs) (partition f xs))
(provide (rename-out [uchicago151-partition partition]))

(: uchicago151-andmap : (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (uchicago151-andmap f xs) (andmap f xs))
(provide (rename-out [uchicago151-andmap andmap]))

(: uchicago151-ormap : (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (uchicago151-ormap f xs) (ormap f xs))
(provide (rename-out [uchicago151-ormap ormap]))

(: uchicago151-vector-map : (All (A B) (A -> B) (Vectorof A) -> (Vectorof B)))
(define (uchicago151-vector-map f v) (vector-map f v))
(provide (rename-out [uchicago151-vector-map vector-map]))

(: uchicago151-vector-filter : (All (A) (A -> Boolean) (Vectorof A) -> (Vectorof A)))
(define (uchicago151-vector-filter f v) (vector-filter f v))
(provide (rename-out [uchicago151-vector-filter vector-filter]))

(: uchicago151-vector-count : (All (A) (A -> Boolean) (Vectorof A) -> Integer))
(define (uchicago151-vector-count pred v) (vector-count pred v))
(provide (rename-out [uchicago151-vector-count vector-count]))

(: uchicago151-sqrt : Real -> Real)
(define (uchicago151-sqrt x)
  (if (not (negative? x))
      (sqrt x)
      (error
       (string-append "sqrt expects a nonnegative real; given "
                      (number->string x)))))
(provide (rename-out [uchicago151-sqrt sqrt]))

;; forbidden builtin functions
;;

(: forbidden-function : String -> String)
(define (forbidden-function f)
  (string-append "uchicago151: " f
                 ": You may not use the built-in function " f
                 " in this course; you must write your own such function."))

(: uchicago151-argmax : (All (A) (A -> Real) (Listof A) -> A))
(define (uchicago151-argmax f xs)
  (error (forbidden-function "argmax")))
(provide (rename-out [uchicago151-argmax argmax]))

(: uchicago151-argmin : (All (A) (A -> Real) (Listof A) -> A))
(define (uchicago151-argmin f xs)
  (error (forbidden-function "argmin")))
(provide (rename-out [uchicago151-argmin argmin]))

(: uchicago151-apply : (All (a b) (a * -> b) (Listof a) -> b))
(define (uchicago151-apply f xs)
  (error (forbidden-function "apply")))
(provide (rename-out [uchicago151-apply apply]))

(: uchicago151-vector-argmin : (All (X) (X -> Real) (Vectorof X) -> X))
(define (uchicago151-vector-argmin f xs)
  (error (forbidden-function "vector-argmin")))
(provide (rename-out [uchicago151-vector-argmin vector-argmin]))

(: uchicago151-vector-argmax : (All (X) (X -> Real) (Vectorof X) -> X))
(define (uchicago151-vector-argmax f xs)
  (error (forbidden-function "vector-argmax")))
(provide (rename-out [uchicago151-vector-argmax vector-argmax]))

(: uchicago151-object-name : Any -> Any)
(define (uchicago151-object-name x)
  (error (forbidden-function "object-name")))
(provide (rename-out [uchicago151-object-name object-name]))

(: uchicago151-list-set : (All (A) (Listof A) Integer A -> (Listof A)))
(define (uchicago151-list-set xs i y)
  (error (forbidden-function "list-set")))
(provide (rename-out [uchicago151-list-set list-set]))

(: uchicago151-list-update : (All (A) (Listof A) Integer (-> A A) ->
  (Listof A)))
(define (uchicago151-list-update xs i fn)
  (error (forbidden-function "list-update")))
(provide (rename-out [uchicago151-list-update list-update]))

(: uchicago151-append* : (All (a) (-> (Listof (Listof a)) (Listof a))))
(define (uchicago151-append* lst)
  (error (forbidden-function "append*")))
(provide (rename-out [uchicago151-append* append*]))
