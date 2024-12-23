#lang racket

(require "util.rkt")
(define input (get-input 23))

(define edges (map (λ (s) (string-split s "-")) input))
(define vertices (set->list (list->set (flatten edges))))

(define G (make-hash))

(for ([e edges])
  (hash-set! G (first e) (set-add (hash-ref G (first e) (set)) (second e)))
  (hash-set! G (second e) (set-add (hash-ref G (second e) (set)) (first e))))

(define (count-n-pairs ls)
  (cond
    [(null? ls) 0]
    [(null? (cdr ls)) 0]
    [else
     (+ (sum (map (λ (v)
                    (cond
                      [(not (set-member? (hash-ref G v) (car ls))) 0]
                      [(and (eq? #\t (string-ref v 0)) (eq? #\t (string-ref (car ls) 0))) (/ 1 3)]
                      [(or (eq? #\t (string-ref v 0)) (eq? #\t (string-ref (car ls) 0))) (/ 1 2)]
                      [else 1]))
                  (cdr ls)))
        (count-n-pairs (cdr ls)))]))

(sum (map (λ (v)
            (if (eq? (string-ref v 0) #\t)
                (count-n-pairs (set->list (hash-ref G v)))
                0))
          vertices))

;; seed -> max-component
(define memo (make-hash))
(define (build-components seed fringe)
  (cond
    [(hash-has-key? memo seed) (hash-ref memo seed)]
    [else
     (define best seed)
     (for/list ([n fringe])
       (define r (build-components (set-add seed n) (set-intersect fringe (hash-ref G n))))
       (when (> (set-count r) (set-count best))
         (set! best r)))
     (hash-set! memo seed best)
     best]))

(string-join (sort (set->list (argmax set-count (map (λ (n) (build-components (set n) (hash-ref G n))) vertices)))
                   string<?)
             ",")
