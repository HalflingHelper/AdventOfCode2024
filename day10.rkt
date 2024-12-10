#lang racket

(require "util.rkt" )
(define test? #f)
(define input (map string->list (get-input 10 test?)))

(define (get-number-indices ls [i 0] [j 0])
  (cond
    [(null? ls) (make-immutable-hash)]
    [(null? (car ls)) (get-number-indices (cdr ls) (add1 i) 0)]
    [else (let ([cur (caar ls)]
                [rest (get-number-indices (cons (cdar ls) (cdr ls)) i (add1 j))])
            (hash-set rest (char->num cur) (set-add (hash-ref rest (char->num cur) (set)) (cons i j))))]))

(define numbers (get-number-indices input))

(define memo1 (list->vector
               (map (λ (row rowi)
                      (list->vector
                       (map (λ (n col)
                              (if (eq? n #\9) (set (cons rowi col)) (set)))
                            row (range (length row)))))
                    input (range (length input)))))

(define memo2 (list->vector
               (map (λ (row rowi)
                      (list->vector
                       (map (λ (n col)
                              (if (eq? n #\9) 1 0))
                            row (range (length row)))))
                    input (range (length input)))))

(define (neighbors pair)
  (match-let ([(cons i j) pair])
    (list (cons (add1 i) j)
          (cons i (add1 j))
          (cons (sub1 i) j)
          (cons i (sub1 j)))))

(define ((vec-ref-p v) p)
  (match-let ([(cons i j) p])
    (vector-ref (vector-ref v i) j)))

(for ([i (reverse (range 9))])
  (define pairs (set->list (hash-ref numbers i)))
  (define nexts (hash-ref numbers (add1 i)))
  (for ([pair pairs])
    (match-let ([(cons row col) pair])
      (vector-set! (vector-ref memo2 row) col
                   (sum (map (vec-ref-p memo2)
                             (filter (curry set-member? nexts) (neighbors pair)))))
      (vector-set! (vector-ref memo1 row) col
                   (foldr set-union (set) (map (vec-ref-p memo1)
                                               (filter (curry set-member? nexts) (neighbors pair))))))))

(sum (map (compose set-count (vec-ref-p memo1)) (set->list (hash-ref numbers 0))))
(sum (map (vec-ref-p memo2) (set->list (hash-ref numbers 0))))
