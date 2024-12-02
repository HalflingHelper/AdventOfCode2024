#lang racket

(require "util.rkt")

(define input (get-input 1))

(define l (map (compose string->number car) (map string-split input)))
(define r (map (compose string->number cadr) (map string-split input)))

(sum (map (compose abs -) (sort l <) (sort r <)))

(define ((get-score ls) x)
  (* x (length
  (filter ((curry eq?) x) ls))))

(sum (map (get-score r) l))
