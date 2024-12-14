#lang racket

(require "util.rkt")
(define test #f)

(define input (get-input 14 test))
(define width (if test 11 101))
(define height (if test 7 103))
(define split-x (quotient width 2))
(define split-y (quotient height 2))

(define (parse ls)
  (if (null? ls)
      '()
      (cons
       (map string->number
            (cdr (regexp-match #px"p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" (car ls))))
        (parse (cdr ls)))))

(define (update ls secs)
  (match ls
    ['() '()]
    [(cons (list px py vx vy) d)
     (let ([fx (modulo (+ px (* vx secs)) width)]
              [fy (modulo (+ py (* vy secs)) height)])
        (cons (list fx fy vx vy) (update d secs)))]))

(define (quadrant-totals positions [tl 0] [br 0] [tr 0] [bl 0])
  (match positions
    ['() (* tl tr bl br)]
    [(cons (list x y _ _) d) #:when (and (< x split-x) (< y split-y))
                (quadrant-totals (cdr positions) (add1 tl) br tr bl)]
    [(cons (list x y _ _) d) #:when (and (< x split-x) (> y split-y))
                (quadrant-totals (cdr positions) tl br tr (add1 bl))]
    [(cons (list x y _ _) d) #:when (and (> x split-x) (< y split-y))
                (quadrant-totals (cdr positions) tl br (add1 tr) bl)]
    [(cons (list x y _ _) d) #:when (and (> x split-x) (> y split-y))
                (quadrant-totals (cdr positions) tl (add1 br) tr bl)]
    [(cons (list x y _ _) d) (quadrant-totals d tl br tr bl)]))

(define add2 (compose add1 add1))
(define sub2 (compose sub1 sub1))
(define (neighbors p)
  (list (list (add1 (first p)) (second p))
        (list (add1 (first p)) (add1 (second p)))
        (list (add1 (first p)) (sub1 (second p)))
        (list (first p) (add1 (second p)))
        (list (first p) (sub1 (second p)))
        (list (sub1 (first p)) (second p))
        (list (sub1 (first p)) (add1 (second p)))
        (list (sub1 (first p)) (sub1 (second p)))))

;; is-picture when everything is pretty close to something else
(define (picture? positions)
  (let* ([ps^ (map (λ (x) (list (first x) (second x))) positions)]
         [p-set (list->set ps^)]
         [score (- (set-count p-set)
          (count (λ (p) (findf (curry set-member? p-set) (neighbors p))) ps^))])
    (< score 150)))

(define (draw positions)
  (let ([ps (list->set positions)])
    (for ([y (range height)])
      (for ([x (range width)])
        (display
         (if (set-member? ps (list x y)) "*" "_")))
      (display "\n"))))

(define start 6500)
#;(let ([ps (update (parse input) (sub1 start))])
  ;; Bounds revealed by finding 10403 cycle and submitting ala binary search
  (for ([i (range start 8001)])
    (set! ps (update ps 1))
    (when (picture? ps) 
      (printf "~a \n" i)
      (draw (map (λ (x) (take x 2)) ps)))))

(quadrant-totals (update (parse input) 100))
(draw (map (λ (x) (take x 2)) (update (parse input) 7055)) )