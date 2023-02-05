;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "lib-tubes.rkt")

;;
;; ***************************************************
;;  Aaryan Patel (21008018)
;;  CS 135 Fall 2022
;;  Assignment 10, Problem 1
;; ***************************************************
;;

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))
             ))

;; stubs for finished-game? and next-games
;; Students will need to modify these functions to make solve work correctly
;; These stubs will at least allow the entire program to run without error

;;; (define (finished-game? gm)
;;;   false)

;;; (define (next-games gm)
;;;   empty)

;;;;;

;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Examples:
;; students should provide some here, or just in tests

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool

(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))                
     (define (solve-helper to-visit visited)
        (cond
         [(empty? to-visit) false]
         [else
          (local
            [(define var (argmin (lambda (x) (num-blocks (game-tubes x))) to-visit))
             (define draw (draw-board var draw-option))] 
            (cond
              [(finished-game? var) true]
              [(member? var visited)
               (solve-helper (remove var to-visit) visited)]
              [else
               (local [(define nbrs (next-games var))
                       (define new (filter (lambda (x) (not (member? x visited))) nbrs))
                       (define new-to-visit (append new (remove var to-visit)))
                       (define new-visited (cons var visited))]
                 (solve-helper new-to-visit new-visited))]))]))]
    (solve-helper (list gm) empty)))

;; Test cases that can be uncommented as the solution is completed

(check-expect (solve smallgame1 'slow) true)
(check-expect (solve mediumgame 'slow) true)
(check-expect (solve mediumgamestuck 'slow) false)

;; Below is the format for testing and timing the solution:
;; be sure to remove any other check-expects when measuring your timing

;;; (check-expect (time (solve mediumgame 'off)) true)
;;; (check-expect (time (solve largergame 'off)) true)
;;; (check-expect (time (solve biggame 'off)) true)
;;; (check-expect (time (solve hugegame 'off)) true)

(define (flatten-list lst)
  (foldr (lambda (x r) (append x r)) empty lst)
)

(define (remove-duplicates lst)
    (foldr (lambda (n r) (cons n (filter (lambda (x) (not (symbol=? n x))) r))) empty lst)
)

;; Part A:
;; (check-colour? size num los) produces true there are exactly size number
;; of occurances of each symbol in los and if there the number of different
;; symbols is less than or equal to num consumed
;; Examples:
(check-expect (check-colour? 2 3 '(red blue green red blue green)) true)
(check-expect (check-colour? 2 1 '(red blue orange red blue green)) false)

;; check-colour?: Nat Nat (listof Symbol) -> Boolean
(define (check-colour? size num los)
	(cond
    [(and (empty? los) (>= num 0)) true]
    [(local [(define filtered (filter (lambda (x) (symbol=? x (first los))) los))]
      (= (length filtered) size)) (check-colour? size (sub1 num)
        (filter (lambda (x) (not (symbol=? x (first los)))) los))]
    [else false]
  )
)
;; Tests:

;; Part B:
;; (valid-game? gm) produces true or false indicating if the gm consumed is a valid game
;; Examples:
(check-expect (valid-game? smallgame1) true)
(check-expect (valid-game? smallinvalidgame1) false)

;; valid-game?: Game -> Boolean
(define (valid-game? gm)
  (local [(define flatten (flatten-list (game-tubes gm)))]
    (and (andmap (lambda (x) (<= (length x) (game-tubesize gm))) (game-tubes gm))
      (<= (length (remove-duplicates flatten)) (game-maxcolours gm))
      (check-colour? (game-tubesize gm) (game-maxcolours gm) flatten)))
)
;; Tests:
(check-expect (valid-game? emptygame) true)
(check-expect (valid-game? emptygame2) true)
(check-expect (valid-game? emptygame3) true)

;; Part C:
;; (remove-completed gm) produces the Game consumed after removing any tubes
;; that are filled with balls of the same colour
;; Examples:
(check-expect (remove-completed smallgame1) smallgame1)
(check-expect (remove-completed smallgamefinal) (make-game 2 0 (list empty)))

;; remove-completed: Game -> Game
(define (remove-completed gm)
  (local [(define (completed? lst)
            (and (= (game-tubesize gm) (length lst))
                 (equal? lst (filter (lambda (x) (symbol=? x (first lst))) lst))))
          (define newlst (filter (lambda (x) (not (completed? x))) (game-tubes gm)))
          (define colour-count (length (remove-duplicates (flatten-list newlst))))]
    (make-game (game-tubesize gm) colour-count newlst))
)
;; Tests:
(check-expect (remove-completed emptygame) (make-game 0 0 empty))
(check-expect (remove-completed emptygame2) (make-game 10 0 empty))
(check-expect (remove-completed emptygame3) (make-game 10 0 (list empty empty)))
(check-expect (remove-completed smallgame2) smallgame1)

;; Part D:
;; (finished-game? gm) produces true or false indicating whether or not the
;; game consumed is finished
;; Examples:
(check-expect (finished-game? smallgamefinal) true)
(check-expect (finished-game? mediumgame) false)

;; finished-game?: Game -> Boolean
(define (finished-game? gm)
  (zero? (game-maxcolours (remove-completed gm)))
)
;; Tests:
(check-expect (finished-game? emptygame) true)
(check-expect (finished-game? emptygame2) true)
(check-expect (finished-game? emptygame3) true)

;; Part E:
;; (num-blocks llos) produces the number of "blocks," that is, the number of
;; consecutive sequences of identical symbols in one list
;; Examples:
(check-expect (num-blocks (list empty '(a a a) '(a a b a a))) 4)
(check-expect (num-blocks (list empty '(a a a) '(b b b))) 2)

;; num-blocks: (listof (listof Symbol)) -> Int
(define (num-blocks llos)
  (cond
    [(empty? llos) 0]
    [(empty? (first llos)) (num-blocks (rest llos))]
    [else (+ (num-blocks/acc (rest (first llos)) (first (first llos)) 1)
      (num-blocks (rest llos)))]
  )
)
;; Tests:
;; (num-blocks/acc lst sym acc) is an accumulator for num-blocks and produces a number
;; of blocks in an individual list of symbols
;; Examples:
(check-expect (num-blocks/acc '(a a a) 'a 1) 1)
(check-expect (num-blocks/acc '(a a b a a) 'a 1) 3)

;; num-blocks/acc: (listof Symbol) -> Int
(define (num-blocks/acc lst sym acc)
  (cond
    [(empty? lst) acc]
    [(symbol=? sym (first lst)) (num-blocks/acc (rest lst) sym acc)]
    [else (num-blocks/acc (rest lst) (first lst) (add1 acc))]
  )
)
;; Tests:

;; Part F:
;; (equiv-game? gm1 gm2) produces true or false indcating whether or not the
;; two games consumed are equivalent
;; Examples:
(check-expect (equiv-game? smallgame1 smallgame1) true)
(check-expect (equiv-game? smallgame1 smallgame2) false)

;; equiv-game?: Game Game -> Boolean
(define (equiv-game? gm1 gm2)
	(and (= (game-maxcolours gm1) (game-maxcolours gm2))
    (= (game-tubesize gm1) (game-tubesize gm2))
    (= (length (game-tubes gm1)) (length (game-tubes gm2)))
    (andmap (lambda (x) (member? x (game-tubes gm2))) (game-tubes gm1)))
)
;; Tests:

;; Part G:
;; (all-equiv? log1 log2) produces true if there exists an equivalent game in log
;; for every game in log1
;; Examples:
(check-expect (all-equiv? (list smallgame1 mediumgame) (list smallgame1 mediumgame)) true)
(check-expect (all-equiv? (list smallgame1 mediumgame) (list smallgame1 smallgame2)) false)

;; all-equiv?: (listof Game) (listof Game) -> Boolean)
(define (all-equiv? log1 log2)
  (and (andmap (lambda (x) (ormap (lambda (y) (equiv-game? x y)) log2)) log1)
    (andmap (lambda (x) (ormap (lambda (y) (equiv-game? x y)) log1)) log2))
)
;; Tests:

(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))
(define smallgameexpected (list (make-game 2 2
                                  (list (list 'red)
                                        (list 'blue 'red)
                                        (list 'blue)))
                                (make-game 2 2
                                  (list (list 'blue 'red)
                                        (list 'red)
                                        (list 'blue)))))
(define mediumgameexpected (list (make-game 2 3
                                  (list (list 'red)
                                        (list 'red 'yellow)
                                        (list 'yellow 'blue)
                                        (list 'blue)))
                                (make-game 2 3
                                  (list (list 'blue 'red)
                                        (list 'yellow)
                                        (list 'yellow 'blue)
                                        (list 'red)))
                                (make-game 2 3
                                  (list (list 'blue 'red)
                                        (list 'red 'yellow)
                                        (list 'blue)
                                        (list 'yellow)))))
(define smallgame2expected (list (make-game 2 3
                                  (list (list 'red)
                                        (list 'blue 'red)
                                        (list 'blue)))
                                 (make-game 2 3
                                  (list (list 'blue 'red)
                                        (list 'red)
                                        (list 'blue)))))
(define largergameexpected (list
              (make-game 3 3
                (list (list 'red 'red)
                      (list 'yellow 'blue 'yellow)
                      (list 'red 'yellow 'blue)
                      (list 'blue)))
              (make-game 3 3
                (list (list 'blue 'red 'red)
                      (list 'blue 'yellow)
                      (list 'red 'yellow 'blue)
                      (list 'yellow)))
              (make-game 3 3
                (list (list 'blue 'red 'red)
                      (list 'yellow 'blue 'yellow)
                      (list 'yellow 'blue)
                      (list 'red)))))

;; Part H:
;; (next-games gm) produces a list of Games representing the possibilities of moving
;; one ball from gm
;; Examples:
(check-expect (test-next-games smallgame1 smallgameexpected) true)
(check-expect (test-next-games smallgame2 smallgame2expected) true)

(check-expect (next-games smallgame1) smallgameexpected)
(check-expect (next-games smallgame2) smallgame2expected)
;; next-games: Game -> (listof Game)
(define (next-games gm)
	(foldl (lambda (x r) (cond [(empty? x) r]
      [else (append r (make-moves gm (first x) (second x) empty))]))
    empty (get-baselists (game-tubes gm) (sub1 (length (game-tubes gm))) empty))
)
;; Tests:
(check-expect (test-next-games mediumgame mediumgameexpected) true)
(check-expect (test-next-games largergame largergameexpected) true)
(check-expect (test-next-games emptygame empty) true)
(check-expect (test-next-games emptygame2 empty) true)
(check-expect (test-next-games emptygame3 empty) true)
(check-expect (test-next-games smallgamefinal (list
(make-game 2 2 (list (list 'red) (list 'blue 'blue) (list 'red)))
(make-game 2 2 (list (list 'blue) (list 'blue) (list 'red 'red))))) true)

(check-expect (next-games mediumgame) mediumgameexpected)
(check-expect (next-games largergame) largergameexpected)
(check-expect (next-games emptygame) empty)
(check-expect (next-games emptygame2) empty)
(check-expect (next-games emptygame3) empty)
(check-expect (next-games smallgamefinal) (list
(make-game 2 2 (list (list 'blue) (list 'blue) (list 'red 'red)))
(make-game 2 2 (list (list 'red) (list 'blue 'blue) (list 'red)))))

(define (get-baselists tubes n acc) 
  (cond
    [(< n 0) acc]
    [else (get-baselists tubes (sub1 n) (cons (baselist tubes n empty) acc))]
  )
)

(define (baselist llos n prev)
  (cond
    [(empty? llos) empty]
    [(zero? n) (cond
      [(empty? (first llos)) empty]
      [else (list (first (first llos))
      (append prev (list (remove (first (first llos)) (first llos))) (rest llos)))])]
    [else (baselist (rest llos) (sub1 n) (append prev (list (first llos))))]
  )
)

(define (make-moves gm ball baselst prev)
  (cond
    [(empty? baselst) empty]
    [else (local [(define move? (make-game (game-tubesize gm) (game-maxcolours gm)
                    (append prev (list (cons ball (first baselst))) (rest baselst))))
                  (define next (make-moves gm ball (rest baselst)
                    (append prev (list (first baselst)))))]
            (cond
          [(or (not (valid-game? move?)) (equal? move? gm)) next]
          [else (cons move? next)]))]
  )
)

