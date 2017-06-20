#lang typed/racket
(require "../uchicago151/uchicago151.rkt")

(define-struct Loc
  ([row : Integer]
   [col : Integer]))
(provide (struct-out Loc))