#lang racket

(require "util.rkt")
(define input (get-input 17))

(define (get-reg str)
  (string->number (car (regexp-match #px"\\d+" str))))

(define A0 (get-reg (first input)))
(define B0 (get-reg (second input)))
(define C0 (get-reg (third input)))

(define instrs (filter (λ (x) x) (map string->number (regexp-split #rx" |," (fifth input)))))

(define (combo op a b c)
  (match op
    [7 (error "invalid combo")]
    [4 a]
    [5 b]
    [6 c]
    [_ op]))

(define (eval instrs a b c prog acc)
  (match instrs
    [`(0 ,op . ,rest)
     (define a^ (quotient a (expt 2 (combo op a b c))))
     (eval rest a^ b c prog acc)] ;; adv
    [`(1 ,op . ,rest)
     (define b^ (bitwise-xor b op))
     (eval rest a b^ c prog acc)]
    [`(2 ,op . ,rest)
     (define b^ (modulo (combo op a b c) 8))
     (eval rest a b^ c prog acc)]
    [`(3 ,op . ,rest) #:when (zero? a) (eval rest a b c prog acc)]
    [`(3 ,op . ,rest)
     (define instrs^ (drop prog op))
     (eval instrs^ a b c prog acc)]
    [`(4 ,op . ,rest)
     (define b^ (bitwise-xor b c))
     (eval rest a b^ c prog acc)]
    [`(5 ,op . ,rest)
     (define acc^ (format "~a," (modulo (combo op a b c) 8)))
     (eval rest a b c prog (string-append acc acc^))]
    [`(6 ,op . ,rest)
     (define b^ (quotient a (expt 2 (combo op a b c))))
     (eval rest a b^ c prog acc)]
    [`(7 ,op . ,rest)
     (define c^ (quotient a (expt 2 (combo op a b c))))
     (eval rest a b c^ prog acc)]
    ['() acc #;(printf "\n Done!")]))

(eval instrs A0 B0 C0 instrs "")

;; Reverse engineering the code
(define (build2 acc targets)
  (if (null? targets) acc
    (for/list ([i (range 8)])
      (define acc^ (+ (* acc 8) i))
      (define c (quotient acc^ (expt 2 (bitwise-xor i 1))))
      (define b (bitwise-xor i 4))
      (define n (modulo (bitwise-xor c b) 8))
      (if (eq? n (car targets)) (build2 acc^ (cdr targets)) #f))))

(apply min (filter (λ (x) x) (flatten (build2 0 (reverse instrs)))))
