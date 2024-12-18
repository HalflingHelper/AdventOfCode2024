#lang racket

(require "util.rkt")
(define test #f)
(define input (get-input 18 test))
(define data (map (λ (s) (map string->number (string-split s ","))) input))

(define target (if test 6 70))

(define (neighbors i j score path)
  (list (list (add1 i) j (add1 score) (set-add path (list i j)))
        (list (sub1 i) j (add1 score) (set-add path (list i j)))
        (list i (add1 j) (add1 score) (set-add path (list i j)))
        (list i (sub1 j) (add1 score) (set-add path (list i j)))))

(define (inbounds p)
  (match-let ([(list i j _ _) p])
    (and (>= i 0) (>= j 0) (<= i target) (<= j target))))

(define (rm-score p)
  (list (first p) (second p)))

(define (solve1 q visited)
  (match q
    ['() #f]
    [(cons (list i j score path) rest)
     (if (and (eq? i target) (eq? j target)) path
         (solve1 (append rest  (filter (λ (p) (and (inbounds p)
                                                (not (set-member? visited (rm-score p)))))
                                    (neighbors i j score path)))
                 (set-union visited (list->set (map rm-score (neighbors i j score path))))))]))

(set-count (solve1 (list (list 0 0 0 (set))) (list->set (take data (if test 12 1024)))))

(define (solve2 cur fallen data)
  (match cur
    [#f (solve2 (solve1 (list (list 0 0 0 fallen)) (set)) fallen data)] ;; Kick things off
    [_ #:when (null? data) '()]
    [_ #:when (set-member? cur (car data))
       (let ([cur^ (solve1 (list (list 0 0 0 (set))) (set-add fallen (car data)))])
         (if cur^ (solve2 cur^ (set-add fallen (car data)) (cdr data))
             (car data)))]
    [_ (solve2 cur (set-add fallen (car data)) (cdr data))]))

(solve2 #f (set) data)
  