#lang racket

(require "util.rkt")
(define input (get-input 15))

(define-values (floor dirstr) (splitf-at input (compose not (curry string=? ""))))

;; Convert the input into a set of boxes and walls
(define (handle part input [row 0] [col 0] [boxes (set)] [walls (set)] [bot '()])
  (cond
    [(null? input) (values boxes walls bot)]
    [(>= col (string-length (car input))) (handle part (cdr input) (add1 row) 0 boxes walls bot)]
    [(eq? #\# (string-ref (car input) col))
     (if (eq? part 1)
         (handle part input row (add1 col) boxes (set-add walls (cons row col)) bot)
         (handle part input row (add1 col) boxes
              (set-add (set-add walls (cons row (* 2 col))) (cons row (add1 (* 2 col)))) bot))]
    [(eq? #\O (string-ref (car input) col))
     (handle part input row (add1 col) (set-add boxes (cons row (* part col))) walls bot)]
    [(eq? #\@ (string-ref (car input) col))
     (handle part input row (add1 col) boxes walls (cons row (* part col)))]
    [else (handle part input row (add1 col) boxes walls bot)]))

(define (score boxes)
  (cond
    [(set-empty? boxes) 0]
    [else (match-let ([(cons i j) (set-first boxes)])
            (+ (* i 100) j (score (set-remove boxes (cons i j)))))]))

(define (getv dir)
  (match dir
    [#\^ (cons -1 0)]
    [#\v (cons 1 0)]
    [#\< (cons 0 -1)]
    [#\> (cons 0 1)]))

(define (get-rep box boxes)
  (cond
    [(set-member? boxes box) box]
    [(set-member? boxes (cons (car box) (sub1 (cdr box)))) (cons (car box) (sub1 (cdr box)))]
    [else #f]))

(define (get-alt box boxes)
  (cond
    [(set-member? boxes box) (cons (car box) (add1 (cdr box)))]
    [(set-member? boxes (cons (car box) (sub1 (cdr box)))) box]
    [else #f]))

(define (try-move-row ri rj di dj boxes walls)
  (cond
    [(set-member? walls (cons (+ ri di) (+ rj dj))) (values #f #f #f)]
    [(set-member? boxes (cons (+ ri di) (+ rj dj)))
     (let-values ([(robot^ boxes^ walls^)
                   (try-move-row (+ ri di) (+ rj dj) di dj boxes walls)])
       (if (not robot^) (values #f #f #f) 
           (values (cons (+ ri di) (+ rj dj))
                   (set-add (set-remove boxes^ (cons (+ ri di) (+ rj dj))) (cons (+ ri di di) (+ rj dj dj)))
                   walls^)))]
    [else (values (cons ri rj) (set-add boxes (cons (+ ri di) (+ rj dj))) walls)]))

(define (try-move-row2 ri rj di dj boxes walls)
  (define next (cons (+ ri di) (+ rj dj)))
  (cond
    [(set-member? walls next) (values #f #f #f)]
    [(set-member? boxes (get-rep next boxes))
     (cond
       [(zero? di) (let-values ([(robot^ boxes^ walls^)
                                 (try-move-row2 ri (+ rj dj) di dj boxes walls)])
                     (if (not robot^) (values #f #f #f)
                         (values (cons ri (+ rj dj))
                                 (let ([rep (get-rep next boxes)])
                                   (set-add
                                    (set-remove boxes^ (cons (car rep) (cdr rep)))
                                    (cons (car rep) (+ dj (cdr rep)))))
                                 walls^)))]
       [(zero? dj) (let-values ([(robot^ boxes^ walls^) 
                                 (try-move-row2 (+ ri di) (cdr (get-rep next boxes)) di dj boxes walls)]
                                [(robot^^ boxes^^ walls^^)
                                 (try-move-row2 (+ ri di) (cdr (get-alt next boxes)) di dj boxes walls)])
                     (if (and robot^ robot^^)
                         (values (cons (+ ri di) (+ rj dj))
                                 (set-union
                                  (set (cons (+ ri di di) (+ dj (cdr (get-rep next boxes)))))
                                  (set-subtract boxes^ boxes)
                                  (set-subtract boxes^^ boxes)
                                  (set-remove 
                                   (set-remove
                                    (set-remove
                                     (set-intersect boxes boxes^^ boxes^)
                                     (get-rep next boxes))
                                    (get-rep (cons (+ ri di di) (cdr (get-alt next boxes))) boxes))
                                   (get-rep (cons (+ ri di di) (cdr (get-rep next boxes))) boxes)))
                                 walls^)
                         (values #f #f #f)))])]
    [else (values (cons ri rj)
                  (set-add boxes (cons (+ ri di) (+ dj (cdr (get-rep (cons ri rj) boxes))))) walls)]))

(define (move part robot dir boxes walls)
  (match-let ([(cons di dj) (getv dir)]
              [(cons ri rj) robot])
    (cond
      [(set-member? walls (cons (+ ri di) (+ rj dj))) (values robot boxes walls)]
      [(set-member? boxes (if (eq? part 1)
                              (cons (+ ri di) (+ rj dj))
                              (get-rep (cons (+ ri di) (+ rj dj)) boxes)))
       (let-values ([(robot^ boxes^ walls^)
                     ((if (eq? part 1) try-move-row try-move-row2) ri rj di dj boxes walls)])
         (if (not robot^) (values robot boxes walls)
             (values robot^ boxes^ walls^)))]
      [else  (values (cons (+ ri di) (+ rj dj)) boxes walls)])))
    
(define (solve part robot dirs boxes walls)
  (cond
    [(null? dirs) (score boxes)]
    [else (let-values ([(robot boxes walls) (move part robot (car dirs) boxes walls)])
            (solve part robot (cdr dirs) boxes walls))]))

(define dirs (string->list (foldr string-append "" dirstr)))

(define-values (boxes walls robot) (handle 1 input))
(solve 1 robot dirs boxes walls)

(define-values (boxes2 walls2 robot2) (handle 2 input))
(solve 2 robot2 dirs boxes2 walls2)
