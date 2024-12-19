#lang racket

(require "util.rkt")
(define input (get-input 19 #f))

(define towels (string-split (car input) ", "))
(define patterns (cddr input))

(define memo (make-hash))
(dict-set! memo "" 1)

(define (possible? pattern)
  (cond
    [(dict-has-key? memo pattern) (dict-ref memo pattern)]
    [else (define res (for/sum ([t towels])
            (or (and
                 (>= (string-length pattern) (string-length t))
                 (string=? t (substring pattern 0 (string-length t)))
                 (possible? (substring pattern (string-length t))))
                0)))
          (dict-set! memo pattern res) res]))

(define ways (filter (compose not zero?) (map possible? patterns)))
(length ways)
(sum ways)
