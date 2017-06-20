#lang typed/racket

(require "../uchicago151/uchicago151.rkt")
(require "../uchicago151/uc151image.rkt")
(require "../uchicago151/uc151universe.rkt")
(require typed/test-engine/racket-tests)

;; Dims is a structure to contain integer dimensions.
(define-struct Dims
  ([width  : Integer]
   [height : Integer]))

(define-struct (Sized T)
  ([t    : T]
   [dims : Dims]))

(define-type (Reporter T)
  (T -> String))

(define-type CellColor
  (U "ivory" "lightgray" "darkgray"))

(define-struct Cell
  ([color : CellColor]))

;; A cell field is a list of rows of cells.
;; It is expected, but not enforced in the code
;; that all rows are the same length.
(define-type CellField
  (Listof (Listof Cell)))

;; The quit? Boolean in a CellWorld is false initially.
;; A "q" key action flips quit? to true
;; and indicates that the user wants to quit.
(define-struct CellWorld
  ([cells : (Sized CellField)]
   [info  : (Sized (Reporter CellField))]
   [quit? : Boolean]))

     
(: new-cell-field (Integer Integer -> CellField))
;; Given number of rows and number of columns,
;; build a new field of "ivory" cells.
(define (new-cell-field r c)
  (make-list r
             (make-list c (Cell "ivory"))))
(check-expect (new-cell-field 2 4)
              (list
               (list
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory"))
               (list
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory"))))
(check-expect (new-cell-field 3 3)
              (list
               (list
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory"))
               (list
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory"))
               (list
                (Cell "ivory")
                (Cell "ivory")
                (Cell "ivory"))))

(: advance-cell (Cell -> Cell))
;; Advance the cell to its next state: from "ivory" to "lightgray", 
;; from "lightgray" to "darkgray", from "darkgray" to "ivory".
(define (advance-cell c)
  (cond
    [(string=? "ivory"
      (Cell-color c)) (Cell "lightgray")]
    [(string=? "lightgray"
               (Cell-color c)) (Cell "darkgray")]
    [else (Cell "ivory")]))
(check-expect (advance-cell (Cell "ivory"))
              (Cell "lightgray"))
(check-expect (advance-cell (Cell "lightgray"))
              (Cell "darkgray"))
(check-expect (advance-cell (Cell "darkgray"))
              (Cell "ivory"))

(: advance-in-list ((Listof Cell) Integer -> (Listof Cell)))
;; Advances a single cell in one list index starts at 0
(define (advance-in-list cs i)
  (match cs
    ['() '()]
    [(cons hd tl)
     (cond
       [(or (>= i (length cs))
            (< i 0)) (error "Index out of range")]
       [(= i 0) (cons (advance-cell hd) tl)]
       [else (cons hd (advance-in-list tl (sub1 i)))])]))
(check-expect (advance-in-list
               (list
                (Cell "ivory")
                (Cell "lightgray")
                (Cell "darkgray")
                (Cell "ivory")) 2)
              (list
               (Cell "ivory")
               (Cell "lightgray")
               (Cell "ivory")
               (Cell "ivory")))
  


(: advance-cell-in-field (CellField Integer Integer -> CellField))
;; In the given cell field
;; and given the row and column in that order, advance that cell.
;; Index rows and columns starting at 0, not 1.
;; Raise an error if the row or column is out of bounds.
(define (advance-cell-in-field cf r c)
  (match cf
    ['() '()]
    [(cons hd tl)
     (cond
       [(or (< r 0)
            (>= r (length cf))) (error "row out of bounds")]
       [(= r 0) (cons (advance-in-list hd c) tl)]
       [else (cons hd (advance-cell-in-field tl (sub1 r) c))])]))
(check-expect (advance-cell-in-field
               (list
                (list
                 (Cell "darkgray")
                 (Cell "lightgray"))
                (list
                 (Cell "ivory")
                 (Cell "darkgray"))
                (list
                 (Cell "lightgray")
                 (Cell "ivory"))) 2 1)
              (list
               (list
                (Cell "darkgray")
                (Cell "lightgray"))
               (list
                (Cell "ivory")
                (Cell "darkgray"))
               (list
                (Cell "lightgray")
                (Cell "lightgray"))))
                 
(: set-field (CellField CellColor -> CellField))
;; Set the whole cell field to the given color.
(define (set-field cf col)
  (make-list (length cf)
             (make-list (length (list-ref cf 0)) (Cell col))))
(check-expect (set-field
               (list
                (list
                 (Cell "ivory")
                 (Cell "lightgray")
                 (Cell "darkgray"))
                (list
                 (Cell "ivory")
                 (Cell "darkgray")
                 (Cell "ivory"))) "lightgray")
              (list
               (list
                (Cell "lightgray")
                (Cell "lightgray")
                (Cell "lightgray"))
               (list
                (Cell "lightgray")
                (Cell "lightgray")
                (Cell "lightgray"))))

(: advance-list ((Listof Cell) -> (Listof Cell)))
;; Advances an entire list
(define (advance-list cs)
  (match cs
    ['() '()]
    [(cons hd tl)
     (cons (advance-cell hd) (advance-list tl))]))
(check-expect (advance-list
               (list
                (Cell "darkgray")
                (Cell "lightgray")
                (Cell "ivory")
                (Cell "ivory")))
              (list
               (Cell "ivory")
               (Cell "darkgray")
               (Cell "lightgray")
               (Cell "lightgray")))

(: advance-field (CellField -> CellField))
;; Advance every cell in the field, according to notion of "advancing" above.
(define (advance-field cf)
  (match cf
    ['() '()]
    [(cons hd tl)
     (cons (advance-list hd) (advance-field tl))]))
(check-expect (advance-field
               (list
                (list
                 (Cell "darkgray")
                 (Cell "lightgray")
                 (Cell "ivory"))
                (list
                 (Cell "ivory")
                 (Cell "ivory")
                 (Cell "lightgray"))
                (list
                 (Cell "darkgray")
                 (Cell "ivory")
                 (Cell "lightgray"))))
              (list
               (list
                (Cell "ivory")
                (Cell "darkgray")
                (Cell "lightgray"))
               (list
                (Cell "lightgray")
                (Cell "lightgray")
                (Cell "darkgray"))
               (list
                (Cell "ivory")
                (Cell "lightgray")
                (Cell "darkgray"))))

(: integer-colors (CellField CellColor -> Integer))
;; Counts the given color within a CellField
(define (integer-colors cf col)
  (match cf
    ['() 0]
    [(cons hd tl)
     (+
      (length
      (filter (lambda ([x : CellColor])
               (string=? x col))
             (map Cell-color hd)))
      (integer-colors tl col))]))
(check-expect (integer-colors
               (list
                (list
                 (Cell "darkgray")
                 (Cell "lightgray"))
                (list
                 (Cell "ivory")
                 (Cell "darkgray"))
                (list
                 (Cell "lightgray")
                 (Cell "darkgray"))
                (list
                 (Cell "ivory")
                 (Cell "darkgray"))) "darkgray") 4)

(: count-colors-string (CellField -> String))
;; Inspect the cell field, count the colors, and build a string 
;; like "ivory: 10, light gray: 2, dark gray: 8".
(define (count-colors-string cf)
  (match cf
    ['() "ivory: 0, light gray: 0, dark gray: 0"]
    [(cons hd tl)
     (string-append
      "ivory: "
      (number->string (integer-colors cf "ivory"))
      ", light gray: "
      (number->string (integer-colors cf "lightgray"))
      ", dark gray: "
      (number->string (integer-colors cf "darkgray")))]))
(check-expect (count-colors-string
               (list
                (list
                 (Cell "lightgray")
                 (Cell "darkgray")
                 (Cell "darkgray")
                 (Cell "ivory"))
                (list
                 (Cell "ivory")
                 (Cell "lightgray")
                 (Cell "ivory")
                 (Cell "darkgray"))))
              "ivory: 3, light gray: 2, dark gray: 3")

;; CellWorld for Testing
(define cw1
  (CellWorld
   (Sized
    (list
     (list
      (Cell "ivory")
      (Cell "ivory")
      (Cell "ivory")
      (Cell "ivory"))
     (list
      (Cell "ivory")
      (Cell "ivory")
      (Cell "ivory")
      (Cell "ivory")))
    (Dims 260 260))
   (Sized
    count-colors-string
    (Dims 260 40))
   #f))

(: new-cell-world (Integer Integer (Reporter CellField) Dims Dims -> CellWorld))
;; Create a new cell world given the number of rows, the number of columns,
;; a cell field reporter, the dimensions in pixels of the cell field
;; and the dimensions in pixels of the report area.
(define (new-cell-world r c rcf scf srcf)
  (CellWorld
   (Sized (new-cell-field r c) scf)
   (Sized rcf srcf)
   #f))
(check-expect
 (CellWorld-cells (new-cell-world 2 4
                                  count-colors-string
                                  (Dims 100 100) (Dims 100 40)))
 (Sized
  (list
   (list
    (Cell "ivory")
    (Cell "ivory")
    (Cell "ivory")
    (Cell "ivory"))
   (list
    (Cell "ivory")
    (Cell "ivory")
    (Cell "ivory")
    (Cell "ivory")))
  (Dims 100 100)))

(: handle-click (CellWorld Integer Integer Mouse-Event -> CellWorld))
;; Advance the clicked-on cell, if one has been clicked upon.
;; Call advance-cell-in-field to do this.
;; You need only react to the "button-down" mouse event.
(define (handle-click cw x y me)
  (match* (cw me)
    [((CellWorld scf rscf q) "button-down")
     (CellWorld
      (Sized
     (advance-cell-in-field
      (Sized-t scf)
      (quotient y (quotient (Dims-height (Sized-dims scf))
                     (length (Sized-t scf))))
      (quotient x (quotient (Dims-width (Sized-dims scf))
                     (length (first (Sized-t scf))))))
     (Sized-dims scf))
      rscf q)]
    [(_ _) cw]))
(check-expect (handle-click cw1 40 20 "button-down")
              (CellWorld
               (Sized
                (list
                 (list
                  (Cell "lightgray")
                  (Cell "ivory")
                  (Cell "ivory")
                  (Cell "ivory"))
                 (list
                  (Cell "ivory")
                  (Cell "ivory")
                  (Cell "ivory")
                  (Cell "ivory")))
                (Dims 260 260))
               (CellWorld-info cw1)
               #f))

(: handle-key (CellWorld String -> CellWorld))
;; Take the following three key actions:
;; "r" means reset the cell field: set every cell to "ivory"
;; "a" means advance the whole cell field
;; "q" means quit
(define (handle-key cw s)
  (match* (cw s)
    [((CellWorld scf rscf q) "r")
     (CellWorld
      (Sized
       (set-field (Sized-t scf) "ivory")
       (Sized-dims scf)) rscf q)]
    [((CellWorld scf rscf q) "a")
     (CellWorld
      (Sized
       (advance-field (Sized-t scf))
       (Sized-dims scf)) rscf q)]
    [((CellWorld scf rscf q) "q")
     (CellWorld scf rscf #t)]
    [(_ _) cw]))
(check-expect (handle-key cw1 "r") cw1)
(check-expect (handle-key cw1 "a")
              (CellWorld
               (Sized
                (set-field
                 (new-cell-field 2 4) "lightgray")
                (Dims 260 260))
               (CellWorld-info cw1)
               #f))
(check-expect (handle-key cw1 "q")
              (CellWorld
               (CellWorld-cells cw1)
               (CellWorld-info cw1)
               #t))

(: draw-cell-row ((Listof Cell) Dims Integer Integer -> Image))
;; Draws a list (or row) of cells given the dimensions of the field
;; and the rows and columns
(define (draw-cell-row cs d r c)
  (match cs
    ['() empty-image]
    [(cons hd tl)
     (cond
       [(string=? "ivory" (Cell-color hd))
        (beside
        (overlay
         (rectangle
         (/ (Dims-width d) c)
         (/ (Dims-height d) r)
         "outline"
         "black")
         (rectangle
         (/ (Dims-width d) c)
         (/ (Dims-height d) r)
         "solid"
         "ivory"))
        (draw-cell-row tl d r c))]
       [(string=? "lightgray" (Cell-color hd))
        (beside
        (overlay
         (rectangle
         (/ (Dims-width d) c)
         (/ (Dims-height d) r)
         "outline"
         "black")
        (rectangle
         (/ (Dims-width d) c)
         (/ (Dims-height d) r)
         "solid"
         "lightgray"))
        (draw-cell-row tl d r c))]
       [else
        (beside
        (overlay
         (rectangle
         (/ (Dims-width d) c)
         (/ (Dims-height d) r)
         "outline"
         "black")
        (rectangle
         (/ (Dims-width d) c)
         (/ (Dims-height d) r)
         "solid"
         "darkgray"))
        (draw-cell-row tl d r c))])]))
(check-expect (draw-cell-row
               (list
                (Cell "ivory")
                (Cell "lightgray")
                (Cell "ivory")
                (Cell "darkgray")) (Dims 100 100) 2 4)
              (beside
               (overlay
               (rectangle 25 50 "outline" "black")
               (rectangle 25 50 "solid" "ivory"))
               (overlay
               (rectangle 25 50 "outline" "black")
               (rectangle 25 50 "solid" "lightgray"))
               (overlay
               (rectangle 25 50 "outline" "black")
               (rectangle 25 50 "solid" "ivory"))
               (overlay
               (rectangle 25 50 "outline" "black")
               (rectangle 25 50 "solid" "darkgray"))))


(: draw-cellfield (CellField Dims Integer Integer -> Image))
;; Draws an entire CellField
(define (draw-cellfield cf d r c)
  (match cf
    ['() empty-image]
    [(cons hd tl)
     (above
      (draw-cell-row hd d r c)
      (draw-cellfield tl d r c))]))

(: draw-cell-world (CellWorld -> Image))
;; Draw the cell field, and immediately underneath it the report area.
;; Use the cell field reporter to generate a fresh report (string)
;; on every draw action.
(define (draw-cell-world cw)
  (match cw
    [(CellWorld scf rscf q)
     (above/align "left"
     (draw-cellfield
      (Sized-t scf)
      (Sized-dims scf)
      (length (Sized-t scf))
      (length (first (Sized-t scf))))
     (overlay
      (text ((Sized-t rscf) (Sized-t scf)) 12 "black")
      (rectangle
       (Dims-width (Sized-dims rscf))
       (Dims-height (Sized-dims rscf))
       "solid" "gray")))]))

(: main (Integer Integer (Reporter CellField) Dims Dims -> CellWorld))
;; Runs big-bang and the program
(define (main r c rcf dcf dr)
  (cond
    [(or
      (< r 1)
      (< c 1)) (error "rows/columns out of bounds")]
    [else
     (big-bang (new-cell-world r c rcf dcf dr) : CellWorld
      [to-draw draw-cell-world]
      [on-mouse handle-click]
      [on-key handle-key]
      [name "Cell World Simulator"]
      [stop-when CellWorld-quit?])]))
                  
(test)