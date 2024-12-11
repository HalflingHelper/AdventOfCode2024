#lang racket

(require "util.rkt")
(define test? #f)
(define input (map string->number (string-split (car (get-input 11 test?)))))

(define (split n)
  (let ([i (expt 10 (/ (floor (+ (log n 10) 1)) 2))])
    (list (quotient n i) (modulo n i))))

(define (process-stone n)
  (cond
    [(zero? n) (list 1)]
    [(odd? (floor (log n 10))) (split n)]
    [else (list (* n 2024))]))

(define memo (make-hash))

(define (process1 stone n)
  (if (hash-has-key? memo (cons stone n))
      (hash-ref memo (cons stone n))
      (let ([res (process2 (process-stone stone) (sub1 n))])
        (hash-set! memo (cons stone n) res)
        res)))

(define (process2 ls n)
  (cond
    [(or (null? ls) (zero? n)) (length ls)]
    [else (+ (process1 (car ls) n) (process2 (cdr ls) n))]))

(process2 input 25)
(process2 input 75)
