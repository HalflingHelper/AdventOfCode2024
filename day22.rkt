#lang racket

(require "util.rkt")
(require racket/hash)
(define input (map string->number (get-input 22)))

(define (next-seed s)
  (let* ([a (modulo (bitwise-xor s (* s 64)) 16777216)]
         [b (bitwise-xor a (quotient a 32))]
         [c (bitwise-xor b (* b 2048))])
    (modulo c 16777216)))

(define (apply-n-times f n acc)
  (if (zero? n)
      acc
      (apply-n-times f (sub1 n) (f acc))))

(sum (map (λ (v) (apply-n-times next-seed 2000 v)) input))

;; Part 2 - Hash map of every sequence for every monkey (2000 * 2000
(define (hash-from-seed s [acc (make-immutable-hash)] [a1 #f] [a2 #f] [a3 #f] [a4 #f] [n 2000])
  (cond
    [(zero? n) acc]
    [(and a1 a2 a3 a4)
     (define poss-key (list (- a2 a1) (- a3 a2) (- a4 a3) (- (modulo s 10) a4)))
     (define acc^
       (if (dict-has-key? acc poss-key)
           acc
           (dict-set acc poss-key (modulo s 10))))
     (hash-from-seed (next-seed s) acc^ a2 a3 a4 (modulo s 10) (sub1 n))]
    [else (hash-from-seed (next-seed s) acc a2 a3 a4 (modulo s 10) (sub1 n))]))

(define (rec ls)
  (cond
    [(null? ls) (make-immutable-hash)]
    [else
     (hash-union (rec (cdr ls)) (hash-from-seed (car ls)) #:combine/key (λ (k v1 v2) (+ v1 v2)))]))

(argmax (λ (l) (cdr l)) (hash->list (rec input)))
