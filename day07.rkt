#lang racket

(require "util.rkt")
(define input (get-input 7))

(define data (map (λ (line) (map string->number (regexp-split #rx":? " line))) input))

(define (|| a b)
  (string->number (string-append (number->string a) (number->string b ))))

(define (equation total ns fst p2)
  (cond
    [(null? ns) (eq? fst total)]
    [else (or (equation total (cdr ns) (* fst (car ns)) p2)
              (equation total (cdr ns) (+ fst (car ns)) p2)
              (and p2 (equation total (cdr ns) (|| fst (car ns)) p2)))]))

(define (solve ls [p2 #f])
  (match ls
    ['() '()]
    [`((,total ,fst . ,ns) . ,rest)
     (cons (if (equation total ns fst p2) total 0) (solve rest p2))]))
;; I know I could do this in one pass, but I don't think it's worth it
(sum (solve data))
(sum (solve data 'p2))
