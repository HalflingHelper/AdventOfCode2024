#lang racket

(require "util.rkt")

(define input (get-input 2))
(define data (map (λ (line) (map string->number (string-split line))) input))

(define (safe? ls [dir #f] [last #f])
  (cond
    [(null? ls) #t]
    [(eq? dir 1) (and (> (car ls) last) (<= (- (car ls) last) 3)
                      (safe? (cdr ls) 1 (car ls)))]
    [(eq? dir -1) (and (> last (car ls)) (<= (- last (car ls)) 3)
                       (safe? (cdr ls) -1 (car ls)))]
    [last (if (> (car ls) last) (safe? ls 1 last) (safe? ls -1 last))]
    [else (safe? (cdr ls) #f (car ls))]))

(define (any-safe? ls [acc '()])
  (cond
    [(null? ls) #f]
    [else (or (safe? (append acc (cdr ls)))
              (any-safe? (cdr ls) (append acc (list (car ls)))))]))

(length (filter safe? data))
(length (filter any-safe? data))
