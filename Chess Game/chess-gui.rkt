#lang typed/racket

(require "../uchicago151/uchicago151.rkt")
(require "../uchicago151/uc151image.rkt")
(require "../uchicago151/uc151universe.rkt")
(require "chess-logic.rkt")
(require "../project2/option.rkt")
(require "../project2/loc.rkt")
(require "../uchicago151/chess-logic-checks.rkt")
(require typed/test-engine/racket-tests)

(define-type (Reporter T)
  (T -> String))

;; The quit? Boolean in a ChessWorld is false initially.
;; A "q" key action flips quit? to true
;; and indicates that the user wants to quit.
(define-struct ChessWorld
  ([game : (Sized ChessGame)]
   [info  : (Sized (Reporter ChessWorld))]
   [select : (Option Loc)]
   [promote : (Option Loc)]
   [quit? : Boolean]))

(: chessrep (ChessWorld -> String))
;; Reporter function for a chess world, stating in-check? and
;; whose turn it is, also what keys to promote
(define (chessrep cw)
  (local
    {(define game (Sized-t (ChessWorld-game cw)))}
   (string-append
   (symbol->string (ChessGame-turn game))
   "'s Turn"
   (match (ChessWorld-select cw)
     ['None ""]
     [(Some loc)
        (match (board-ref (ChessGame-board game) loc)
          ['None " | Nothing Selected"]
          [(Some piece) (string-append " | "
                                       (symbol->string (Piece-type piece))
                                       " Selected")])])
   (cond
     [(checkmate? game) " | Checkmate!"]
     [(stalemate? game) " | Stalemate!"]
     [(in-check? game) " | In Check"]
     [else ""]))))
(check-expect (chessrep (new-chessworld new-game (Dims 300 300) (Dims 300 120)))
              "White's Turn")
(check-expect (chessrep (new-chessworld testgame2 (Dims 30 30) (Dims 30 30)))
              "Black's Turn")
(check-expect (chessrep (new-chessworld cmgame (Dims 30 30) (Dims 30 30)))
              "White's Turn | Checkmate!")


(: new-chessworld (ChessGame Dims Dims -> ChessWorld))
;; Create a new chess world given the dimensions in pixels of the cell field
;; and the dimensions in pixels of the report area.
(define (new-chessworld game sizeb sizer)
  (ChessWorld
   (Sized game sizeb)
   (Sized chessrep sizer)
   'None 'None #f))

(: deselect (ChessWorld -> ChessWorld))
;; Deselects whatever is selected in the world
(define (deselect cw)
  (ChessWorld
   (ChessWorld-game cw)
   (ChessWorld-info cw)
   'None
   (ChessWorld-promote cw)
   (ChessWorld-quit? cw)))

(: select (ChessWorld Loc -> ChessWorld))
;; Selects the piece at the given location
(define (select cw loc)
  (ChessWorld
   (ChessWorld-game cw)
   (ChessWorld-info cw)
   (Some loc)
   (ChessWorld-promote cw)
   (ChessWorld-quit? cw)))

(: determine-move (Loc Loc Board Player -> Move))
;; Given the source, destination, and player, determine which move type this is
(define (determine-move src dest board p)
  (match (board-ref board src)
    [(Some piece)
     (cond
       [(and (or
              (and (= (Loc-row src) 0) (= (Loc-col src) 4))
              (and (= (Loc-row src) 7) (= (Loc-col src) 4)))
             (equal? (board-ref board src) (Some (Piece 'King p)))
             (= (abs (- (Loc-col src) (Loc-col dest))) 2))
        (match p
          ['White
           (if (and (= (Loc-row dest) 7) (= (Loc-col dest) 2))
               (CastleMove src dest (Loc 7 0) (Loc 7 3) p)
               (CastleMove src dest (Loc 7 7) (Loc 7 5) p))]
          ['Black
           (if (and (= (Loc-row dest) 0) (= (Loc-col dest) 2))
               (CastleMove src dest (Loc 0 0) (Loc 0 3) p)
               (CastleMove src dest (Loc 0 7) (Loc 0 5) p))])]
       [else (StdMove src dest piece (board-ref board dest))])]
    ['None (error "determine move given an empty piece")]))
(check-expect (determine-move (Loc 6 7) (Loc 5 7) starting-board 'White)
              (StdMove (Loc 6 7) (Loc 5 7) (Piece 'Pawn 'White) 'None))
(check-expect (determine-move (Loc 0 4) (Loc 0 2) starting-board 'Black)
              (CastleMove (Loc 0 4) (Loc 0 2) (Loc 0 0) (Loc 0 3) 'Black))
(check-expect (determine-move (Loc 7 6) (Loc 5 5) starting-board 'White)
              (StdMove (Loc 7 6) (Loc 5 5) (Piece 'Knight 'White) 'None))

(: promotable? : Piece Loc -> Boolean)
;; Is the given piece at the given location grounds for a promotion?
(define (promotable? p loc)
  (match (Piece-type p)
    ['Pawn
     (match (Piece-color p)
       ['White (= (Loc-row loc) 0)]
       ['Black (= (Loc-row loc) 7)])]
    [_ #f]))
(check-expect (promotable? (Piece 'Pawn 'White) (Loc 0 7)) #t)
(check-expect (promotable? (Piece 'Pawn 'Black) (Loc 0 7)) #f)
(check-expect (promotable? (Piece 'Knight 'White) (Loc 0 5)) #f)

(: handle-click (ChessWorld Integer Integer Mouse-Event -> ChessWorld))
;; Select a piece for moving, move a piece, or deselect a piece
;; You need only react to the "button-down" mouse event.
(define (handle-click cw x y me)
  (local
    {(define board (ChessGame-board (Sized-t (ChessWorld-game cw))))
     (define loc-click (Loc
                        (quotient y (quotient
                                     (Dims-height
                                      (Sized-dims (ChessWorld-game cw))) 8))
                        (quotient x (quotient
                                     (Dims-width
                                      (Sized-dims (ChessWorld-game cw))) 8))))}
    (match* (cw me)
      [((ChessWorld sgame srep selected promote q) "button-down")
       (match selected
         [(Some loc)
         (match (board-ref board loc)
            [(Some piece)
             (if
              (equal?
               (board-ref board loc-click)
               (Some piece))
              (deselect cw)
              (if
               (and
                (promotable? piece loc-click)
                (legal-move? (Sized-t sgame)
                             (PromoMove loc loc-click piece 'None 'Queen)))
               (ChessWorld sgame srep selected (Some loc-click) q)
               (ChessWorld
                (Sized
                 (apply-move (Sized-t (ChessWorld-game cw))
                           (determine-move
                            (Some-value selected)
                            loc-click
                            board
                            (ChessGame-turn (Sized-t (ChessWorld-game cw)))))
                 (Sized-dims (ChessWorld-game cw)))
                srep
                'None
                'None q)))]
            ['None cw])]
             
         ['None
          (if (symbol=?
               (ChessGame-turn (Sized-t (ChessWorld-game cw)))
               (match (board-ref board loc-click)
                 [(Some piece) (Piece-color piece)]
                 ['None 'None]))
               (select cw loc-click)
               cw)])]
      [(_ _) cw])))

;; Handle-key: If ChessWorld-promote (Some Loc)
;; then on "u" replace pawn w/ queen and set ChessWorld-promote to 'None

(: handle-key : ChessWorld String -> ChessWorld)
(define (handle-key cw s)
  (match cw
    [(ChessWorld sgame srep sel promote q)
     (if (string=? s "q") (ChessWorld sgame srep sel promote #t)
     (match promote
       ['None cw]
       [(Some _)
        (ChessWorld
         (Sized
          (apply-move (Sized-t sgame)
                      (PromoMove
                       (if (symbol=? (ChessGame-turn
                                      (Sized-t (ChessWorld-game cw))) 'White)
                           (Loc 1 (Loc-col (Some-value promote)))
                           (Loc 6 (Loc-col (Some-value promote))))
                       (Some-value promote)
                       (Piece 'Pawn (ChessGame-turn (Sized-t sgame)))
                       (board-ref (ChessGame-board (Sized-t sgame))
                                  (Some-value promote))
                       (match s
                         ["u" 'Queen]
                         ["k" 'Knight]
                         ["b" 'Bishop]
                         [_ 'Rook])))
          (Sized-dims sgame))
         (Sized (Sized-t srep) (Sized-dims srep))
         'None 'None q)]))]))
       
(: draw-chess-world (ChessWorld -> Image))
;; Draw the chess board
(define (draw-chess-world cw)
  (match cw
    [(ChessWorld sgame srep _ _ q)
     (above/align "left"
      (board->image (ChessGame-board (Sized-t sgame))
                    (Sized-dims sgame))
      (overlay
       (if (equal? (ChessWorld-promote cw) 'None)
       (text ((Sized-t srep) cw) 12 "black")
       (above
        (text ((Sized-t srep) cw) 12 "black")
   (match (ChessWorld-promote cw)
     ['None empty-image]
     [(Some loc)
      (above
       (text "Press u for a queen, k for a knight," 14 "black")
(text " anything for a rook, or b for a bishop" 14 "black"))])))
       (rectangle
        (Dims-width (Sized-dims srep))
        (Dims-height (Sized-dims srep))
        "solid" "gray")))]))
   
(: main (ChessGame Dims Dims -> ChessWorld))
;; Runs big-bang and the program
(define (main game game-d rep-d)
     (big-bang (new-chessworld game game-d rep-d) : ChessWorld
      [to-draw draw-chess-world]
      [on-mouse handle-click]
      [on-key handle-key]
      [name "Chess-tacular 3000"]
      [stop-when ChessWorld-quit?]))
                  
(test)