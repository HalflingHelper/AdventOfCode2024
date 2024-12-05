#lang racket

(require "util.rkt")

(define input (get-input 5))

(define-values (rules-raw tests-raw) (splitf-at input (λ (s) (string-contains? s "|"))))

(define rules (list->set (map (λ (s) (map string->number (string-split s "|"))) rules-raw)))
(define tests (map (λ (s) (map string->number (string-split s ","))) (cdr tests-raw)))

(define ((is-before rules a) b)
  (not (set-member? rules (list b a))))

(define ((valid-order? rules) test)
  (if (null? test) #t
      (and (all? (cdr test) (is-before rules (car test)))
           ((valid-order? rules) (cdr test)))))

(define (middle ls)
  (list-ref ls (quotient (length ls) 2)))

(sum (map middle (filter (valid-order? rules) tests)))

(define (insert rules e ls)
  (cond
    [(null? ls) (list e)]
    [((is-before rules e) (car ls)) (cons e ls)]
    [else (cons (car ls) (insert rules e (cdr ls)))]))

(define ((fix rules) order [res '()])
  (if (null? order) res
      ((fix rules) (cdr order) (insert rules (car order) res))))

(sum (map middle (map (fix rules) (filter-not (valid-order? rules) tests))))
