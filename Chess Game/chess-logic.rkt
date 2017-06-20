#lang typed/racket

(require "../uchicago151/uchicago151.rkt")
(require "../uchicago151/uc151image.rkt")
(require "../project2/option.rkt")
(require "../project2/loc.rkt")
(require "../uchicago151/chess-logic-checks.rkt")
(require typed/test-engine/racket-tests)

(define-struct Dims
  ([width  : Integer]
   [height : Integer]))
(provide (struct-out Dims))

(define-struct (Sized T)
  ([t    : T]
   [dims : Dims]))
(provide (struct-out Sized))

;; Shorthand for pieces
(define bpawn
  (Piece 'Pawn 'Black))
(define bking
  (Piece 'King 'Black))
(define bqueen
  (Piece 'Queen 'Black))
(define bbishop
  (Piece 'Bishop 'Black))
(define brook
  (Piece 'Rook 'Black))
(define bknight
  (Piece 'Knight 'Black))
(define wpawn
  (Piece 'Pawn 'White))
(define wking
  (Piece 'King 'White))
(define wqueen
  (Piece 'Queen 'White))
(define wrook
  (Piece 'Rook 'White))
(define wknight
  (Piece 'Knight 'White))
(define wbishop
  (Piece 'Bishop 'White))

(: opp (Player -> Player))
;; Flips the player
(define (opp p)
  (match p
    ['White 'Black]
    ['Black 'White]))

(: starting-board : Board)
;; Standard layout of chess board, black on top
(define starting-board
  (list
   (list (Some brook)
         (Some bknight)
         (Some bbishop)
         (Some bqueen)
         (Some bking)
         (Some bbishop)
         (Some bknight)
         (Some brook))
   (list (Some bpawn)
         (Some bpawn)
         (Some bpawn)
         (Some bpawn)
         (Some bpawn)
         (Some bpawn)
         (Some bpawn)
         (Some bpawn))
   (list 'None 'None 'None 'None 'None 'None 'None 'None)
   (list 'None 'None 'None 'None 'None 'None 'None 'None)
   (list 'None 'None 'None 'None 'None 'None 'None 'None)
   (list 'None 'None 'None 'None 'None 'None 'None 'None)
   (list (Some wpawn)
         (Some wpawn)
         (Some wpawn)
         (Some wpawn)
         (Some wpawn)
         (Some wpawn)
         (Some wpawn)
         (Some wpawn))
   (list (Some wrook)
         (Some wknight)
         (Some wbishop)
         (Some wqueen)
         (Some wking)
         (Some wbishop)
         (Some wknight)
         (Some wrook))))
(provide starting-board)

(: strings->board : ((Listof String) -> Board))
;; Creates a board given a list of strings
(define (strings->board ss)
  (local
    {(: string->row : String -> (Listof Square))
     ;; Creates one row of a board using a string
     (define (string->row s)
       (if (string=? s "") '()
           (match (string-ref s 0)
             [#\- (cons 'None (string->row (substring s 1)))]
             [#\P (cons (Some bpawn) (string->row (substring s 1)))]
             [#\R (cons (Some brook) (string->row (substring s 1)))]
             [#\B (cons (Some bbishop) (string->row (substring s 1)))]
             [#\N (cons (Some bknight) (string->row (substring s 1)))]
             [#\Q (cons (Some bqueen) (string->row (substring s 1)))]
             [#\K (cons (Some bking) (string->row (substring s 1)))]
             [#\p (cons (Some wpawn) (string->row (substring s 1)))]
             [#\r (cons (Some wrook) (string->row (substring s 1)))]
             [#\b (cons (Some wbishop) (string->row (substring s 1)))]
             [#\n (cons (Some wknight) (string->row (substring s 1)))]
             [#\q (cons (Some wqueen) (string->row (substring s 1)))]
             [#\k (cons (Some wking) (string->row (substring s 1)))]
             [_ '()])))
     (: s->b : (Listof String) -> Board)
     ;; Does most of strings->board's work
     (define (s->b slist)
       (match slist
         ['() '()]
         [(cons hd tl)
          (cons (string->row hd) (s->b tl))]))
     }
    (if (not (or
              (= (length ss) 8)
              (= (string-length (first ss)) 8)))
        (error "Incorrect size")
        (s->b ss))))
#|(check-expect (strings->board
               (list
                "RNBQKBNR"
                "PPPPPPPP"
                "--------"
                "--------"
                "--------"
                "--------"
                "pppppppp"
                "rnbqkbnr")) starting-board)|#

(define testboard1
  (strings->board
   (list
    "-NB-KB-R"
    "RPP-PPP-"
    "P--Q-N-P"
    "---P----"
    "----p-q-"
    "bpn-----"
    "p-pp-ppp"
    "r---kbnr")))
(define testgame1 (ChessGame testboard1 'White '() (Castles #f #t #t #t)))
(define testboard2
  (strings->board
   (list
    "-RB--B-R"
    "PP-PKP-P"
    "-----N--"
    "-QP-P-P-"
    "---b-r-n"
    "--p----b"
    "pp--pp-p"
    "r---k---")))
(define testgame2 (ChessGame testboard2 'Black '() (Castles #f #f #t #f)))
(provide testgame2)
(define cmboard
  (strings->board
   (list
    "--------"
    "--------"
    "---K----"
    "------R-"
    "---NQ--B"
    "--------"
    "-----k--"
    "--R-----")))
(define cmgame (ChessGame cmboard 'White '() (Castles #f #f #f #f)))
(provide cmgame)
(define smboard
  (strings->board
   (list
    "--------"
    "--------"
    "---K----"
    "------R-"
    "---NQ---"
    "--------"
    "-----k--"
    "--R-----")))
(define smgame (ChessGame smboard 'White '() (Castles #f #f #f #f)))
(provide smgame)

(: new-game : ChessGame)
;; Starting chess game details
(define new-game
  (ChessGame starting-board 'White '()
             (Castles #t #t #t #t)))
(provide new-game)

(: board-ref : Board Loc -> Square)
;; Given a board + location, what's in the square?
(define (board-ref b l)
  (if (or (> (Loc-row l) 7) (> (Loc-col l) 7)) (error "out of bounds")
      (list-ref (list-ref b (Loc-row l)) (Loc-col l))))
#|(check-expect (board-ref starting-board (Loc 0 2)) (Some bbishop))
(check-expect (board-ref starting-board (Loc 3 4)) 'None)
(check-error (board-ref testboard1 (Loc 0 8)) "out of bounds")|#
(provide board-ref)

(: update-list : (All (A) (Listof A) Integer A -> (Listof A)))
;; Helper function that updates list
(define (update-list sqs n s)
  (match sqs
    ['() '()]
    [(cons hd tl)
     (cond
       [(<= n 0) (cons s tl)]
       [else (cons hd
                   (update-list tl (sub1 n) s))])]))
#|(check-expect (update-list (list
                            (Some brook)
                            (Some bpawn)
                            'None
                            'None
                            (Some bking)) 2 (Some bqueen))
              (list
               (Some brook)
               (Some bpawn)
               (Some bqueen)
               'None
               (Some bking)))
(check-expect (update-list (list 'None 'None (Some wpawn)) 1 (Some bking))
              (list 'None (Some bking) (Some wpawn)))|#

(: board-update : Board Loc Square -> Board)
;; Updates given board at Loc with new Square
(define (board-update b l s)
  (update-list b
               (Loc-row l)
               (update-list
                (list-ref b (Loc-row l))
                (Loc-col l) s)))
#|(check-expect (board-update starting-board (Loc 5 2) (Some wknight))
              (strings->board (list
                               "RNBQKBNR"
                               "PPPPPPPP"
                               "--------"
                               "--------"
                               "--------"
                               "--n-----"
                               "pppppppp"
                               "rnbqkbnr")))
(check-expect (board-update testboard1 (Loc 1 0) 'None)
              (strings->board (list
                               "-NB-KB-R"
                               "-PP-PPP-"
                               "P--Q-N-P"
                               "---P----"
                               "----p-q-"
                               "bpn-----"
                               "p-pp-ppp"
                               "r---kbnr")))|#

(: get-index : (All (A) (Listof A) A Integer -> Integer))
;; Returns the index number of the given object if it is in the list
;; Returns -1 if it's not even in the list
(define (get-index xs target count)
  (match xs
    ['() -1]
    [(cons _ _)
     (cond
       [(>= count (length xs)) -1]
       [(equal? (list-ref xs count) target) count]
       [else (get-index xs target (+ count 1))])]))
;;(check-expect (get-index (list 1 2 3 4 5) 3 0) 2)
;;(check-expect (get-index (list "a" "b" "c" "d" "e") "e" 0) 4)

(: piece-location : Board Square Integer -> Loc)
;; Returns the location of the given square, using a counter variable
(define (piece-location b p count)
  (match b
    ['() (error "piece not found")]
    [(cons hd tl)
     (if (= (get-index hd p 0) -1) (piece-location tl p (+ count 1))
         (Loc count (get-index hd p 0)))]))
;(check-expect (piece-location starting-board (Some wking) 0) (Loc 7 4))
;(check-expect (piece-location testboard1 (Some bking) 0) (Loc 0 4))
;(check-expect (piece-location testboard2 (Some bking) 0) (Loc 1 4))

(: bishop-range? (Loc Loc Board Player -> Boolean))
;; Checks if a location is within a bishop's range, l1 being bishop
(define (bishop-range? l1 l2 b p)
  (local
    {(: lower-left (Loc Loc Board -> Boolean))
     ;; Checks if a lower-left location is in range of another
     (define (lower-left loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (lower-left
                     (Loc (add1 (Loc-row loc1)) (sub1 (Loc-col loc1)))
                     loc2 board)]
             [_ #f])))
     (: lower-right (Loc Loc Board -> Boolean))
     ;; Checks if a lower-right location is in range of another
     (define (lower-right loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (lower-right
                     (Loc (add1 (Loc-row loc1)) (add1 (Loc-col loc1)))
                     loc2 board)]
             [_ #f])))
     (: upper-left (Loc Loc Board -> Boolean))
     ;; Checks if a upper-left location is in range of another
     (define (upper-left loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (upper-left
                     (Loc (sub1 (Loc-row loc1)) (sub1 (Loc-col loc1)))
                     loc2 board)]
             [_ #f])))
     (: upper-right (Loc Loc Board -> Boolean))
     ;; Checks if a upper-right location is in range of another
     (define (upper-right loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (upper-right
                     (Loc (sub1 (Loc-row loc1)) (add1 (Loc-col loc1)))
                     loc2 board)]
             [_ #f])))}
    (match* (l1 l2)
      [((Loc r1 c1) (Loc r2 c2))
       (cond
         [(match (board-ref b l2)
            ['None #f]
            [(Some piece)
             (symbol=? (Piece-color piece) p)]) #f]
         [(=
           (abs (- r1 r2))
           (abs (- c1 c2)))
          (cond
            [(and
              (< r1 r2)
              (< c1 c2)) (lower-right
                          (Loc
                           (add1 (Loc-row l1))
                           (add1 (Loc-col l1)))
                          l2 b)]
            [(and
              (< r1 r2)
              (> c1 c2)) (lower-left
                          (Loc
                           (add1 (Loc-row l1))
                           (sub1 (Loc-col l1)))
                          l2 b)]
            [(and
              (> r1 r2)
              (< c1 c2)) (upper-right
                          (Loc
                           (sub1 (Loc-row l1))
                           (add1 (Loc-col l1)))
                          l2 b)]
            [else (upper-left
                   (Loc
                    (sub1 (Loc-row l1))
                    (sub1 (Loc-col l1)))
                   l2 b)])]
         [else #f])])))
;(check-expect (bishop-range? (Loc 5 7) (Loc 1 3) testboard2 'White) #t)
;(check-expect (bishop-range? (Loc 0 2) (Loc 2 4) starting-board 'Black) #f)
;(check-expect (bishop-range? (Loc 5 0) (Loc 2 3) testboard1 'White) #t)
;(check-expect (bishop-range? (Loc 0 2) (Loc 1 1) starting-board 'Black) #f)

(: rook-range? (Loc Loc Board Player -> Boolean))
;; Checks if a location is within a rook's range
(define (rook-range? l1 l2 b p)
  (local
    {(: left? (Loc Loc Board -> Boolean))
     ;; Is a leftward location in range of another?
     (define (left? loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (left?
                     (Loc (Loc-row loc1) (sub1 (Loc-col loc1)))
                     loc2 board)]
             [_ #f])))
     (: right? (Loc Loc Board -> Boolean))
     ;; Is a rightward location in range of another?
     (define (right? loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (right?
                     (Loc (Loc-row loc1) (add1 (Loc-col loc1)))
                     loc2 board)]
             [_ #f])))
     (: up? (Loc Loc Board -> Boolean))
     ;; Is a upward location in range of another?
     (define (up? loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (up?
                     (Loc (sub1 (Loc-row loc1)) (Loc-col loc1))
                     loc2 board)]
             [_ #f])))
     (: down? (Loc Loc Board -> Boolean))
     ;; Is a downward location in range of another?
     (define (down? loc1 loc2 board)
       (if (equal? loc1 loc2) #t
           (match (board-ref board loc1)
             ['None (down?
                     (Loc (add1 (Loc-row loc1)) (Loc-col loc1))
                     loc2 board)]
             [_ #f])))}
    (match* (l1 l2)
      [((Loc r1 c1) (Loc r2 c2))
       (cond
         [(match (board-ref b l2)
            ['None #f]
            [(Some piece)
             (symbol=? (Piece-color piece) p)]) #f]
         [(= r1 r2)
          (if (> c1 c2)
              (left?
               (Loc (Loc-row l1) (sub1 (Loc-col l1)))
               l2 b)
              (right?
               (Loc (Loc-row l1) (add1 (Loc-col l1)))
               l2 b))]
         [(= c1 c2)
          (if (> r1 r2)
              (up?
               (Loc (sub1 (Loc-row l1)) (Loc-col l1))
               l2 b)
              (down?
               (Loc (add1 (Loc-row l1)) (Loc-col l1))
               l2 b))]
         [else #f])])))
;(check-expect (rook-range? (Loc 7 0) (Loc 5 0) starting-board 'White) #f)
;(check-expect (rook-range? (Loc 0 7) (Loc 0 5) starting-board 'Black) #f)
;(check-expect (rook-range? (Loc 7 0) (Loc 7 2) testboard1 'White) #t)
;(check-expect (rook-range? (Loc 4 5) (Loc 2 5) testboard2 'White) #t)

(: knight-range? (Loc Loc Board Player -> Boolean))
;; Checks if a location is within another's knight range, player is whose knight
(define (knight-range? l1 l2 b p)
  (match* (l1 l2)
    [((Loc r1 c1) (Loc r2 c2))
     (cond
       [(or
         (and
          (= (abs (- r1 r2)) 1)
          (= (abs (- c1 c2)) 2))
         (and
          (= (abs (- r1 r2)) 2)
          (= (abs (- c1 c2)) 1)))
        (match (board-ref b l2)
          ['None #t]
          [(Some piece)
           (not (symbol=? (Piece-color piece) p))])]
       [else #f])]))
;(check-expect (knight-range? (Loc 0 1) (Loc 2 2) starting-board 'Black) #t)
;(check-expect (knight-range? (Loc 7 1) (Loc 6 3) starting-board 'White) #f)
;(check-expect (knight-range? (Loc 5 2) (Loc 3 3) testboard1 'White) #t)
;(check-expect (knight-range? (Loc 4 7) (Loc 5 4) testboard2 'White) #f)

(: pawn-attack-range? (Loc Player Loc Board (Listof Move) -> Boolean))
;; Checks if a location is within another's pawn attack range
;; given whose pawn it is
;; l1 is location of the pawn
(define (pawn-attack-range? l1 p l2 b hist)
  (match (board-ref b l2)
    ['None
     (cond
       [(and
         (= (Loc-row l1) 3)
         (= (Loc-row l2) 2)
         (symbol=? p 'White))
        (and
         (equal? (first hist)
                 (StdMove
                  (Loc (sub1 (Loc-row l2)) (Loc-col l2))
                  (Loc (add1 (Loc-row l2)) (Loc-col l2))
                  bpawn
                  'None))
         (or
          (= (Loc-col l2) (sub1 (Loc-col l1)))
          (= (Loc-col l2) (add1 (Loc-col l1))))
         (equal? (board-ref b (Loc (add1 (Loc-row l2)) (Loc-col l2)))
                 (Some bpawn)))]
       [(and
         (= (Loc-row l1) 4)
         (= (Loc-row l2) 5)
         (symbol=? p 'Black))
        (and
         (equal? (first hist)
                 (StdMove
                  (Loc (add1 (Loc-row l2)) (Loc-col l2))
                  (Loc (sub1 (Loc-row l2)) (Loc-col l2))
                  wpawn
                  'None))
         (or
          (= (Loc-col l2) (sub1 (Loc-col l1)))
          (= (Loc-col l2) (add1 (Loc-col l1))))
         (equal? (board-ref b (Loc (sub1 (Loc-row l2)) (Loc-col l2)))
                 (Some wpawn)))]
       [else #f])]
    [(Some piece)
     (if (symbol=? (Piece-color piece) p) #f
         (match* (l1 l2)
           [((Loc r1 c1) (Loc r2 c2))
            (cond
              [(and
                (= (abs (- c1 c2)) 1)
                (= (- r1 r2) 1)
                (symbol=? 'White p)) #t]
              [(and
                (= (abs (- c1 c2)) 1)
                (= (- r1 r2) -1)
                (symbol=? 'Black p)) #t]
              [else #f])]))]))
#|(check-expect
 (pawn-attack-range? (Loc 6 0) 'White (Loc 5 1) starting-board '()) #f)
(check-expect
 (pawn-attack-range? (Loc 4 4) 'White (Loc 3 3) testboard1 '()) #t)
(check-expect
 (pawn-attack-range? (Loc 3 2) 'Black (Loc 4 3) testboard2 '()) #t)|#

(: pawn-move-range? (Loc Player Loc Board -> Boolean))
;; Checks if a location is within another's pawn move range
;; l1 is location of the pawn
(define (pawn-move-range? l1 p l2 b)
  (if (symbol=? 'White p)
      (cond
        [(and
          (= (Loc-row l1) (+ 2 (Loc-row l2)))
          (equal? (board-ref b l2) 'None)
          (= (Loc-col l1) (Loc-col l2))
          (equal? (board-ref b
                             (Loc
                              (add1 (Loc-row l2))
                              (Loc-col l2))) 'None)
          (= (Loc-row l1) 6)) #t]
        [(and
          (= (Loc-row l1) (add1 (Loc-row l2)))
          (= (Loc-col l1) (Loc-col l2))
          (equal? (board-ref b l2) 'None)) #t]
        [else #f])
      (or
        (and
          (= 1 (- (Loc-row l2) 2))
          (equal? (board-ref b l2) 'None)
          (= (Loc-col l1) (Loc-col l2))
          (equal? (board-ref b
                             (Loc 2 (Loc-col l1))) 'None)
          (= (Loc-row l1) 1))
        (and
          (= (Loc-row l1) (sub1 (Loc-row l2)))
          (= (Loc-col l1) (Loc-col l2))
          (equal? (board-ref b l2) 'None)))))
#|(check-expect (pawn-move-range? (Loc 6 1) 'White (Loc 4 1) starting-board) #t)
(check-expect (pawn-move-range? (Loc 5 1) 'White (Loc 3 1) testboard1) #f)
(check-expect (pawn-move-range? (Loc 1 5) 'Black (Loc 3 5) testboard2) #f)|#

(: run-through (Loc Board Player -> Boolean))
;; Runs through the whole board, checking if in check
(define (run-through l b p)
  (local
    {(: king : Loc)
     ;; Location of the king
     (define king
       (piece-location b (Some (Piece 'King p)) 0))}
    (cond
      [(> (Loc-row l) 7) #f]
      [(> (Loc-col l) 7) (run-through
                          (Loc (add1 (Loc-row l)) 0) b p)]
      [else
       (match (board-ref b l)
         ['None (run-through
                 (Loc (Loc-row l) (add1 (Loc-col l))) b p)]
         [(Some piece)
          (if (symbol=? (Piece-color piece) p)
              (run-through
               (Loc (Loc-row l) (add1 (Loc-col l))) b p)
              (if (match (Piece-type piece)
                    ['Queen (or
                             (rook-range? king l b p)
                             (bishop-range? king l b p))]
                    ['Bishop (bishop-range? king l b p)]
                    ['Knight (knight-range? king l b p)]
                    ['Rook (rook-range? king l b p)]
                    ['Pawn (pawn-attack-range? l (opp p) king b '())]
                    ['King #f])
                  #t
                  (run-through
                   (Loc (Loc-row l) (add1 (Loc-col l))) b p)))])])))
;(check-expect (run-through (Loc 0 0) testboard1 'White) #f)
;(check-expect (run-through (Loc 0 0) cmboard 'White) #t)

(: in-check? : ChessGame -> Boolean)
;; Is the player whose turn it is in check?
(define (in-check? game)
  (run-through (Loc 0 0) (ChessGame-board game) (ChessGame-turn game)))
#|(check-expect (in-check? (ChessGame
                          starting-board 'Black '() (Castles #t #t #t #t))) #f)
(check-expect (in-check? (ChessGame
                          cmboard 'White '() (Castles #f #f #f #f))) #t)|#
(provide in-check?)

(: king-range? (Loc Loc Player Board -> Boolean))
;; Checks if a king at l1 of the player's can move to l2
(define (king-range? l1 l2 p b)
  (match* (l1 l2)
    [((Loc r1 c1) (Loc r2 c2))
     (cond
       [(and
         (or
          (= (abs (- r1 r2)) 1)
          (= (abs (- r1 r2)) 0))
         (or
          (= (abs (- c1 c2)) 1)
          (= (abs (- c1 c2)) 0)))
        (match (board-ref b l2)
          ['None
           (not (in-check? (ChessGame
                            (board-update b l2 (Some (Piece 'King p)))
                            p '() (Castles #t #t #t #t))))]
          [(Some piece)
           (and
            (not (symbol=? (Piece-color piece) p))
            (not (in-check? (ChessGame
                             (board-update b l2 (Some (Piece 'King p)))
                             p '() (Castles #t #t #t #t)))))])]
       [else #f])]))
;(check-expect (king-range? (Loc 7 4) (Loc 7 3) 'White starting-board) #f)
;(check-expect (king-range? (Loc 7 4) (Loc 6 4) 'White testboard1) #t)
;(check-expect (king-range? (Loc 1 4) (Loc 2 3) 'Black testboard2) #t)

(: castle? : ChessGame CastleMove -> Boolean)
;; Is the given CastleMove legal?
(define (castle? game m)
  (local
    {(define board (ChessGame-board game))
     (define player (ChessGame-turn game))
     (: w7-0 : Loc Board -> Boolean)
     ;; Are all the in-between squares 'None?
     (define (w7-0 loc b)
       (if (= (Loc-col loc) 0) #t
           (match (board-ref b loc)
             [(Some _) #f]
             ['None (w7-0 (Loc (Loc-row loc) (sub1 (Loc-col loc))) b)])))
     (: w7-7 : Loc Board -> Boolean)
     ;; Are all the in-between squares 'None?
     (define (w7-7 loc b)
       (if (= (Loc-col loc) 7) #t
           (match (board-ref b loc)
             [(Some _) #f]
             ['None (w7-7 (Loc (Loc-row loc) (add1 (Loc-col loc))) b)])))
     (: b0-0 : Loc Board -> Boolean)
     ;; Are all the in-between squares 'None?
     (define (b0-0 loc b)
       (if (= (Loc-col loc) 0) #t
           (match (board-ref b loc)
             [(Some _) #f]
             ['None (b0-0 (Loc (Loc-row loc) (sub1 (Loc-col loc))) b)])))
     (: b0-7 : Loc Board -> Boolean)
     ;; Are all the in-between squares 'None?
     (define (b0-7 loc b)
       (if (= (Loc-col loc) 7) #t
           (match (board-ref b loc)
             [(Some _) #f]
             ['None (b0-7 (Loc (Loc-row loc) (add1 (Loc-col loc))) b)])))}
    (match m
      [(CastleMove k-src k-dest r-src r-dest player)
       (and
        (if (symbol=? player 'White)
            (or
             (and
              (= (Loc-row r-src) 7)
              (= (Loc-col r-src) 0)
              (Castles-white-toward-7-0 (ChessGame-cas game))
              (w7-0 (Loc 7 3) board))
             (and
              (= (Loc-row r-src) 7)
              (= (Loc-col r-src) 7)
              (Castles-white-toward-7-7 (ChessGame-cas game))
              (w7-7 (Loc 7 5) board)))
            (or
             (and
              (= (Loc-row r-src) 0)
              (= (Loc-col r-src) 0)
              (Castles-black-toward-0-0 (ChessGame-cas game))
              (b0-0 (Loc 0 3) board))
             (and
              (= (Loc-row r-src) 0)
              (= (Loc-col r-src) 7)
              (Castles-black-toward-0-7 (ChessGame-cas game))
              (b0-7 (Loc 0 5) board))))
        (not (in-check? game))
        (not (in-check? (ChessGame
                         (board-update
                          (board-update
                           (board-update
                            (board-update board k-src 'None)
                            k-dest (Some (Piece 'King player)))
                           r-src 'None)
                          r-dest (Some (Piece 'Rook player)))
                         (opp player) '() (ChessGame-cas game)))))])))
     
(: promo? : ChessGame PromoMove -> Boolean)
;; Is the given PromoMove legal?
(define (promo? game m)
  (local
    {(: player : Player)
     (define player (ChessGame-turn game))
     (: board : Board)
     (define board (ChessGame-board game))}
  (match m
    [(PromoMove src dest piece cap prom)
      (and
       (symbol=? (Piece-type piece) 'Pawn)
       (if (symbol=? player 'White)
           (and
            (= (Loc-row dest) 0)
            (= (Loc-row src) 1)
            (if (equal? cap 'None)
                (= (Loc-col dest) (Loc-col src))
                (= (abs (- (Loc-col dest) (Loc-col src))) 1)))
           (and
            (= (Loc-row dest) 7)
            (= (Loc-row src) 6)
            (if (equal? cap 'None)
                (= (Loc-col dest) (Loc-col src))
                (= (abs (- (Loc-col dest) (Loc-col src))) 1))))
       (not (in-check?
             (ChessGame
              (board-update
               (board-update board src 'None) dest (Some piece))
              player '() (Castles #t #t #t #t)))))])))
           
(: legal-move? : ChessGame Move -> Boolean)
;; Is the proposed move legal?
(define (legal-move? game m)
  (local
    {(: player : Player)
     ;; Whoever's turn
     (define player (ChessGame-turn game))
     (: board : Board)
     ;; The board
     (define board (ChessGame-board game))}
    (match m
      [(CastleMove _ _ _ _ _) (castle? game m)]
      [(PromoMove _ _ _ _ _) (promo? game m)]
      [(StdMove src dest piece _)
       (and (match (Piece-type piece)
              ['King (king-range? src dest player board)]
              ['Queen (or
                       (bishop-range? src dest board player)
                       (rook-range? src dest board player))]
              ['Knight (knight-range? src dest board player)]
              ['Bishop (bishop-range? src dest board player)]
              ['Rook (rook-range? src dest board player)]
              ['Pawn (or
                      (pawn-move-range? src player dest board)
                      (pawn-attack-range? src player dest board
                                          (ChessGame-history game)))])
            (not (in-check? (ChessGame
                             (board-update
                              (board-update board src 'None)
                              dest (Some piece))
                             player '() (Castles #t #t #t #t)))))])))
(provide legal-move?)
;(check-expect (legal-move? new-game (StdMove
;                                     (Loc 7 1) (Loc 5 0) wknight 'None)) #t)
;(check-expect (legal-move? testgame1 (StdMove
 ;                                     (Loc 5 2) (Loc 3 2) wpawn 'None)) #f)
;(check-expect (legal-move? testgame2 (StdMove
 ;                                     (Loc 3 5) (Loc 4 4)
  ;                                    bpawn (Some bbishop))) #t)
;(check-expect (legal-move? cmgame (StdMove
 ;                                  (Loc 6 5) (Loc 7 4) wking 'None)) #f)


(: available-moves-piece : ChessGame Loc -> (Listof Move))
;; Given a game and piece (by loc), prints list of legal moves
(define (available-moves-piece game l)
  (local
    {(: mover : Square)
     ;; Piece to be moved
     (define mover (board-ref (ChessGame-board game) l))
     (: list-move (Integer Integer -> (Listof Move)))
     ;; Using row and column, checks for moves
     (define (list-move row col)
       (match mover
         ['None '()]
         [(Some piece)
          (cond
            [(> row 7) '()]
            [(> col 7) (list-move (add1 row) 0)]
            [(legal-move? game (StdMove l (Loc row col) piece
                                        (board-ref (ChessGame-board game)
                                                   (Loc row col))))
             (cons (StdMove l (Loc row col) piece
                            (board-ref (ChessGame-board game) (Loc row col)))
                   (list-move row (add1 col)))]
            [else (list-move row (add1 col))])]))}
    (list-move 0 0)))
(provide available-moves-piece)
#|(check-expect (sort-moves (available-moves-piece new-game (Loc 6 3)))
              (list
               (StdMove (Loc 6 3) (Loc 4 3) wpawn 'None)
               (StdMove (Loc 6 3) (Loc 5 3) wpawn 'None)))
(check-expect (sort-moves (available-moves-piece testgame1 (Loc 7 0)))
              (list
               (StdMove (Loc 7 0) (Loc 7 1) wrook 'None)
               (StdMove (Loc 7 0) (Loc 7 2) wrook 'None)
               (StdMove (Loc 7 0) (Loc 7 3) wrook 'None)))
(check-expect (sort-moves (available-moves-piece testgame2 (Loc 1 4)))
              (list
               (StdMove (Loc 1 4) (Loc 0 3) bking 'None)
               (StdMove (Loc 1 4) (Loc 0 4) bking 'None)
               (StdMove (Loc 1 4) (Loc 2 3) bking 'None)))|#

(: current-pieces : Board Player Loc -> (Listof Loc))
;; Gives a list of locations of pieces the current player has
(define (current-pieces b p l)
  (cond
    [(> (Loc-row l) 7) '()]
    [(> (Loc-col l) 7) (current-pieces b p (Loc (add1 (Loc-row l)) 0))]
    [else
     (match (board-ref b l)
       ['None (current-pieces b p (Loc (Loc-row l) (add1 (Loc-col l))))]
       [(Some piece)
        (cond
          [(symbol=? (Piece-color piece) p) (cons
                                             l
                                             (current-pieces
                                              b
                                              p
                                              (Loc
                                               (Loc-row l)
                                               (add1 (Loc-col l)))))]
          [else (current-pieces b p (Loc
                                     (Loc-row l)
                                     (add1 (Loc-col l))))])])]))
#|(check-expect (current-pieces starting-board 'White (Loc 0 0))
              (list (Loc 6 0) (Loc 6 1) (Loc 6 2) (Loc 6 3) (Loc 6 4) (Loc 6 5)
                    (Loc 6 6) (Loc 6 7) (Loc 7 0) (Loc 7 1) (Loc 7 2) (Loc 7 3)
                    (Loc 7 4) (Loc 7 5) (Loc 7 6) (Loc 7 7)))
(check-expect (current-pieces cmboard 'Black (Loc 0 0))
              (list (Loc 2 3) (Loc 3 6) (Loc 4 3) (Loc 4 4) (Loc 4 7)
                    (Loc 7 2)))|#

(: available-moves-player : ChessGame -> (Listof Move))
;; List all legal moves for the current player
(define (available-moves-player game)
  (local
    {(: append-moves : (Listof (Listof Move))-> (Listof Move))
     ;; Turns a list of move lists into a single list
     (define (append-moves listms)
       (match listms
         ['() '()]
         [(cons hd tl)
          (append hd (append-moves tl))]))}
    (append-moves
     (map
      (lambda ([x : Loc])
        (available-moves-piece game x))
      (current-pieces
       (ChessGame-board game)
       (ChessGame-turn game)
       (Loc 0 0))))))
#|(check-expect (available-moves-player new-game)
              (list
               (StdMove (Loc 6 0) (Loc 4 0) wpawn 'None)
               (StdMove (Loc 6 0) (Loc 5 0) wpawn 'None)
               (StdMove (Loc 6 1) (Loc 4 1) wpawn 'None)
               (StdMove (Loc 6 1) (Loc 5 1) wpawn 'None)
               (StdMove (Loc 6 2) (Loc 4 2) wpawn 'None)
               (StdMove (Loc 6 2) (Loc 5 2) wpawn 'None)
               (StdMove (Loc 6 3) (Loc 4 3) wpawn 'None)
               (StdMove (Loc 6 3) (Loc 5 3) wpawn 'None)
               (StdMove (Loc 6 4) (Loc 4 4) wpawn 'None)
               (StdMove (Loc 6 4) (Loc 5 4) wpawn 'None)
               (StdMove (Loc 6 5) (Loc 4 5) wpawn 'None)
               (StdMove (Loc 6 5) (Loc 5 5) wpawn 'None)
               (StdMove (Loc 6 6) (Loc 4 6) wpawn 'None)
               (StdMove (Loc 6 6) (Loc 5 6) wpawn 'None)
               (StdMove (Loc 6 7) (Loc 4 7) wpawn 'None)
               (StdMove (Loc 6 7) (Loc 5 7) wpawn 'None)
               (StdMove (Loc 7 1) (Loc 5 0) wknight 'None)
               (StdMove (Loc 7 1) (Loc 5 2) wknight 'None)
               (StdMove (Loc 7 6) (Loc 5 5) wknight 'None)
               (StdMove (Loc 7 6) (Loc 5 7) wknight 'None)))
(check-expect (available-moves-player cmgame) '())|#

(: checkmate? : ChessGame -> Boolean)
;; Is the player in checkmate?
(define (checkmate? game)
  (if (and
       (in-check? game)
       (equal? (available-moves-player game) '()))
      #t
      #f))
#|(check-expect (checkmate? testgame1) #f)
(check-expect (checkmate? smgame) #f)
(check-expect (checkmate? cmgame) #t)|#
(provide checkmate?)

(: stalemate? : ChessGame -> Boolean)
;; Is the game a stalemate?
(define (stalemate? game)
  (if (and
       (not (in-check? game))
       (equal? (available-moves-player game) '()))
      #t
      #f))
#|(check-expect (stalemate? cmgame) #f)
(check-expect (stalemate? smgame) #t)
(check-expect (stalemate? testgame2) #f)|#
(provide stalemate?)


(: apply-move : ChessGame Move -> ChessGame)
;; Make the given move, update the board, history, castles, and player turn
(define (apply-move game m)
  (local
    {(: black-0-0? : Boolean)
     ;; Is the upper left corner able to castle?
     (define black-0-0?
       (cond
         [(not (Castles-black-toward-0-0
                (ChessGame-cas game))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 0 0))
                (Some brook))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 0 4))
                (Some bking))) #f]
         [else #t]))
     (: black-0-7? : Boolean)
     ;; Is the upper right corner able to castle?
     (define black-0-7?
       (cond
         [(not (Castles-black-toward-0-7
                (ChessGame-cas game))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 0 7))
                (Some brook))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 0 4))
                (Some bking))) #f]
         [else #t]))
     (: white-7-0? : Boolean)
     ;; Is the lower left corner able to castle?
     (define white-7-0?
       (cond
         [(not (Castles-white-toward-7-0
                (ChessGame-cas game))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 7 0))
                (Some wrook))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 7 4))
                (Some wking))) #f]
         [else #t]))
     (: white-7-7? : Boolean)
     ;; Is the lower right corner able to castle?
     (define white-7-7?
       (cond
         [(not (Castles-white-toward-7-7
                (ChessGame-cas game))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 7 7))
                (Some wrook))) #f]
         [(not (equal?
                (board-ref (ChessGame-board game) (Loc 7 4))
                (Some wking))) #f]
         [else #t]))}
    (if (not (legal-move? game m)) game
        (match m
          [(StdMove src dest piece cap)
           (if
             (and
              (symbol=? (Piece-type piece) 'Pawn)
              (match (Piece-color piece)
                ['White
                  (match (board-ref (ChessGame-board game)
                                    (Loc (add1 (Loc-row dest)) (Loc-col dest)))
                    ['None #f]
                    [(Some piece) #t])]
                ['Black
                  (match (board-ref (ChessGame-board game)
                          (Loc (sub1 (Loc-row dest)) (Loc-col dest)))
                    ['None #f]
                    [(Some piece) #t])]))
             (ChessGame
              (board-update
               (board-update
                (board-update
                 (ChessGame-board game) src 'None)
                dest (Some piece))
               (Loc (if
                     (symbol=? (Piece-color piece) 'White)
                     (add1 (Loc-row dest))
                     (sub1 (Loc-row dest))) (Loc-col dest)) 'None)
              (opp (ChessGame-turn game))
              (cons m (ChessGame-history game))
              (Castles
               black-0-0?
               black-0-7?
               white-7-0?
               white-7-7?))
           (ChessGame
             (board-update
             (board-update
              (ChessGame-board game) src 'None)
             dest (Some piece))
            (opp (ChessGame-turn game))
            (cons m (ChessGame-history game))
            (Castles
             black-0-0?
             black-0-7?
             white-7-0?
             white-7-7?)))]
          [(PromoMove src dest piece cap prom)
           (ChessGame
            (board-update
             (board-update
              (ChessGame-board game) src 'None)
             dest (Some (Piece prom (ChessGame-turn game))))
            (opp (ChessGame-turn game))
            (cons m (ChessGame-history game))
            (ChessGame-cas game))]
          [(CastleMove k-src k-dest r-src r-dest player)
           (ChessGame
            (board-update
             (board-update
              (board-update
               (board-update
                (ChessGame-board game) k-src 'None)
               k-dest (Some (Piece 'King player)))
              r-src 'None)
             r-dest (Some (Piece 'Rook player)))
            (opp (ChessGame-turn game))
            (cons m (ChessGame-history game))
            (Castles black-0-0? black-0-7? white-7-0? white-7-7?))]))))
(provide apply-move)
#|(check-expect (apply-move new-game (StdMove (Loc 6 3) (Loc 4 3) wpawn 'None))
              (ChessGame
               (strings->board
                (list
                 "RNBQKBNR"
                 "PPPPPPPP"
                 "--------"
                 "--------"
                 "---p----"
                 "--------"
                 "ppp-pppp"
                 "rnbqkbnr"))
               'Black
               (list (StdMove (Loc 6 3) (Loc 4 3) wpawn 'None))
               (Castles #t #t #t #t)))
(check-expect (apply-move testgame2
                          (StdMove (Loc 3 1) (Loc 6 4) bqueen (Some wpawn)))
              (ChessGame
               (strings->board
                (list
                 "-RB--B-R"
                 "PP-PKP-P"
                 "-----N--"
                 "--P-P-P-"
                 "---b-r-n"
                 "--p----b"
                 "pp--Qp-p"
                 "r---k---"))
               'White
               (list (StdMove (Loc 3 1) (Loc 6 4) bqueen (Some wpawn)))
               (Castles #f #f #t #f)))|#

(: p-img : Square -> String)
;; Applies the various unicode text to a piece
(define (p-img s)
  (match s
    ['None ""]
    [(Some piece)
     (match piece
       [(Piece 'Pawn 'Black) "♟"]
       [(Piece 'Rook 'Black) "♜"]
       [(Piece 'Bishop 'Black) "♝"]
       [(Piece 'Knight 'Black) "♞"]
       [(Piece 'Queen 'Black) "♛"]
       [(Piece 'King 'Black) "♚"]
       [(Piece 'Pawn 'White) "♙"]
       [(Piece 'Rook 'White) "♖"]
       [(Piece 'Bishop 'White) "♗"]
       [(Piece 'Knight 'White) "♘"]
       [(Piece 'Queen 'White) "♕"]
       [_ "♔"])]))
#|(check-expect (p-img 'None) "")
(check-expect (p-img (Some wknight)) "♘")
(check-expect (p-img (Some bking)) "♚")|#

(: squares->row : (Listof Square) Image-Color Dims -> Image)
;; Turns a list of squares into an image of the row given the color
;; of the first square
(define (squares->row sqs col d)
  (match sqs
    ['() empty-image]
    [(cons hd tl)
     (beside
      (overlay
       (rectangle (quotient (Dims-width d) 8)
                  (quotient (Dims-height d) 8) "outline" "black")
       (text (p-img hd) 20 "black")
       (rectangle (quotient (Dims-width d) 8)
                  (quotient (Dims-height d) 8) "solid" col))
      (squares->row tl (if (equal? col "beige") "brown" "beige") d))]))

(: board->image : Board Dims -> Image)
;; Draws a chess board
(define (board->image b d)
  (match b
    ['() empty-image]
    [(cons hd tl)
     (match tl
       ['() empty-image]
       [(cons head tail)
        (above
         (squares->row hd "beige" d)
         (squares->row head "brown" d)
         (board->image tail d))])]))
(provide board->image)

(test)