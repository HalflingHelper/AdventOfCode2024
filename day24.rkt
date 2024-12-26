#lang racket

(require "util.rkt")
(define input (get-input 24))

(define-values (inits gates) (splitf-at input (λ (s) (not (string=? s "")))))

;; Gross, I know
(define state (make-hash))

;; Using thunks for lazy though!
(for ([i inits])
  (match-let ([(list a b) (string-split i ": ")])
    (dict-set! state a (λ () (string->number b)))))

(define (exec instr a b)
  (cond
    [(string=? instr "AND") (bitwise-and a b)]
    [(string=? instr "XOR") (bitwise-xor a b)]
    [(string=? instr "OR")  (bitwise-ior a b)]
    [else (error "invalid instr")]))

(for ([g (cdr gates)])
  (match-let ([(list full a instr b c)
               (regexp-match #px"(...) ([A-Z]*) (...) -> (...)" g)])
    (dict-set! state c 
               (λ () (exec instr ((hash-ref state a)) 
                                 ((hash-ref state b))))))) 

(define (lpad n)
  (if (< n 10) (string-append "0" (number->string n))
      (number->string n)))

(define (build-num [pos 0])
  (define k (string-append "z" (lpad pos)))
  (cond
    [(hash-has-key? state k) (+ ((hash-ref state k)) (* 2 (build-num (add1 pos))))]
    [else 0]))

(build-num)

;; Add two numbers, and xor the intended / actual result?
;; Use this as a plce to guide my manual search
(define (try-add x y [acc 0])
  (if (and (zero? x) (zero? y)) (build-num)
      (begin (dict-set! state (string-append "x" (lpad acc)) (λ () (bitwise-and x 1)))
             (dict-set! state (string-append "y" (lpad acc)) (λ () (bitwise-and y 1)))
             (try-add (quotient x 2) (quotient y 2) (add1 acc)))))

(define x #b101010101010101010101010101010101010101010101)
(define y #b101010101010101010101010101010101010101010101)
(define ans (try-add x y))
(printf "~b\n" (bitwise-xor ans (+ x y)))
;; Manual checking of sites
"cph,gws,hgj,nnt,npf,z13,z19,z33"



