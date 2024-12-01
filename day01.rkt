#lang racket

(define input (file->lines "inputs/input_01.txt"))

(define l (map (compose string->number car) (filter (compose not null?) (map string-split input))))
(define r (map (compose string->number cadr) (filter (compose not null?) (map string-split input))))

(apply + (map (compose abs -)
            (sort l <)
            (sort r <)))

(define ((get-score ls) x)
  (* x (length
  (filter ((curry eq?) x) ls))))

(apply + (map (get-score r) l))
