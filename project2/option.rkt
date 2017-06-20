#lang typed/racket
(require "../uchicago151/uchicago151.rkt")

(define-struct (Some T)
  ([value : T]))

(define-type (Option T) 
  (U 'None (Some T)))

(provide (struct-out Some))
(provide Option)