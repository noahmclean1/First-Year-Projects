#lang typed/racket

(require "../uchicago151/uchicago151.rkt")
(require "../project2/option.rkt")
(require "../project2/loc.rkt")

(define-type PieceType (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))
(define-type Player (U 'Black 'White))
(define-struct Piece
  ([type  : PieceType]
   [color : Player]))
(provide PieceType Player (struct-out Piece))

(define-type Square (Option Piece))
(provide Square)

(define-type Board (Listof (Listof Square)))
(provide Board)

(define-struct StdMove
  ([src : Loc]
   [dst : Loc]
   [moved : Piece]
   [captured : (Option Piece)]))
(provide (struct-out StdMove))

(define-struct CastleMove
  ([king-src : Loc]
   [king-dst : Loc]
   [rook-src : Loc]
   [rook-dst : Loc]
   [moved : Player]))
(provide (struct-out CastleMove))

(define-type PromoteTo (U 'Queen 'Rook 'Bishop 'Knight))
(define-struct PromoMove
  ([src : Loc]
   [dst : Loc]
   [moved : Piece]
   [captured : (Option Piece)]
   [promote-to : PromoteTo]))
(provide PromoteTo (struct-out PromoMove))

(define-type Move (U StdMove CastleMove PromoMove))
(provide Move)

(define-struct Castles
  ([black-toward-0-0 : Boolean]
   [black-toward-0-7 : Boolean]
   [white-toward-7-0 : Boolean]
   [white-toward-7-7 : Boolean]))
(provide (struct-out Castles))

(define-struct ChessGame
  ([board : Board]
   [turn : Player]
   [history : (Listof Move)]
   [cas : Castles]))
(provide (struct-out ChessGame))

(: opt-map (All (T U) ((T -> U) (Option T) U -> U)))
;; apply f to value if there is one, otherwise return default value
;; ex: (opt-map add1 'None 0)    => 0
;; ex: (opt-map add1 (Some 4) 0) => 5
(define (opt-map f opt def)
  (match opt
    ['None def]
    [(Some x) (f x)]))
(provide opt-map)

(: move->str (Move -> String))
;; build a string version of the move, for purposes of comparison
;; note: there is a bijection between moves and strings (and must be)
(define (move->str m)
  (match m
    [(StdMove src dst moved captured)
     (pipes (list "StdMove"
                  (loc->str src)
                  (loc->str dst)
                  (piece->str moved)
                  (opt-map piece->str captured "None")))]
    [(CastleMove ks kd rs rd moved)
     (pipes (list "CastleMove"
                  (loc->str ks)
                  (loc->str kd)
                  (loc->str rs)
                  (loc->str rd)
                  (symbol->string moved)))]
    [(PromoMove src dst moved captured pro)
     (pipes (list "PromoMove"
                  (loc->str src)
                  (loc->str dst)
                  (piece->str moved)
                  (opt-map piece->str captured "None")
                  (symbol->string pro)))]))
(provide move->str)

(: loc->str (Loc -> String))
;; return string representation of location
(define (loc->str loc)
  (match loc
    [(Loc r c)
     (string-append "Loc:" (number->string r) "," (number->string c))]))
(provide loc->str)

(: piece->str (Piece -> String))
;; return string representation of piece
(define (piece->str p)
  (match p
    [(Piece t pl)
     (string-append "Piece:"
                    (symbol->string t)
                    ","
                    (symbol->string pl))]))
(provide piece->str)

(: pipes ((Listof String) -> String))
;; connect strings with | character in between
;; ex: (pipes (list "a" "bb" "ccc")) ==> "a|bb|ccc"
(define (pipes ss)
  (match ss
    ['() ""]
    [(list s) s]
    [(cons s r) (string-append s "|" (pipes r))]))
(provide pipes)

(: move<? (Move Move -> Boolean))
;; move comparison for the purposes of sorting
(define (move<? m1 m2)
  (string<? (move->str m1) (move->str m2)))
(provide move<?)

(: sort-moves : (Listof Move) -> (Listof Move))
;; sort a list of moves into a canonical order
;; allowing for comparison with check-expect
;; note: uses the built-in sort operation
(define (sort-moves moves)
  (sort moves move<?))
(provide sort-moves)