#lang racket

(require "util.rkt")
(define input (get-input 25))

(define col-height 7)
(define (count-cols c ls)
   (map (λ (x) (count (λ (x) (eq? x c)) x)) (apply map list (map string->list ls))))

(define (process input)
  (cond
    [(null? input) (values '() '())]
    [else (define-values (ai di) (split-at input col-height))
          (define-values (locks keys) (process (cdr di)))
          (if (string=? (car ai) "#####")
               (values (cons (count-cols #\# ai) locks) keys)
               (values locks (cons (count-cols #\# ai) keys))
               )]))

(define-values (locks keys) (process input))

(define (valid-pair? p)
  (let ([lock (car p)]
        [key (cadr p)])
    (zero? (count (λ (x) (> x col-height)) (map + lock key)))))

(count valid-pair? (cartesian-product locks keys))

