#lang racket

(require "util.rkt")
(define input (get-input 21 #f))

;; 9:57 Start
;; 1:34 Finish
(define (num-pad c)
  (cond
    [(eq? c #\A) (cons 3 2)]
    [(eq? c #\0) (cons 3 1)]
    [else (cons (- 3 (quotient (+ 2 (char->num c)) 3)) (modulo (sub1 (char->num c)) 3))]))

(define (arrow-pad c)
  (match c
    [#\^ (cons 0 1)]
    [#\A (cons 0 2)]
    [#\< (cons 1 0)]
    [#\v (cons 1 1)]
    [#\> (cons 1 2)]))

(define (dist^^ p1 p2 pad-ty)
  (match-let ([(cons i1 j1) p1]
              [(cons i2 j2) p2])
    (cond
      ;; Not checked :)
      [(and (eq? i1 i2) (eq? j1 j2)) "A"]
      ;; Going up
      [(and (eq? j1 j2) (< i2 i1)) (string-append (make-string (- i1 i2) #\^) "A")]
      ;; Going right
      [(and (eq? i1 i2) (> j2 j1)) (string-append (make-string (- j2 j1) #\>) "A")]
      ;; Going left
      [(and (eq? i1 i2) (< j2 j1)) (string-append (make-string (- j1 j2) #\<) "A")]
      ;; Going Down
      [(and (eq? j1 j2) (> i2 i1)) (string-append (make-string (- i2 i1) #\v) "A")]
      [(and (> j2 j1) (< i2 i1)) ;; up right
       (cond
         [(and (eq? pad-ty 'dir) (eq? j1 0) (eq? i2 0))
          (string-append (make-string (- j2 j1) #\>) (make-string (- i1 i2) #\^) "A")]
         [else (string-append (make-string (- i1 i2) #\^) (make-string (- j2 j1) #\>) "A")])]
      [(and (> j2 j1) (> i2 i1)) ;; down right
       (cond
         [(and (eq? pad-ty 'num) (eq? i2 3) (eq? j1 0))
          (string-append (make-string (- j2 j1) #\>) (make-string (- i2 i1) #\v) "A")]
         [else (string-append (make-string (- i2 i1) #\v) (make-string (- j2 j1) #\>) "A")])]
      [(and (< j2 j1) (> i2 i1)) ;; down left
       (cond
         [(and (eq? pad-ty 'dir) (eq? i1 0) (eq? j2 0))
          (string-append (make-string (- i2 i1) #\v) (make-string (- j1 j2) #\<) "A")]
         [else (string-append (make-string (- j1 j2) #\<) (make-string (- i2 i1) #\v) "A")])]
      [(and (< i2 i1) (< j2 j1)) ;; up left
       (cond
         [(and (eq? pad-ty 'num) (eq? i1 3) (eq? j2 0))
          (string-append (make-string (- i1 i2) #\^) (make-string (- j1 j2) #\<) "A")]
         [else (string-append (make-string (- j1 j2) #\<) (make-string (- i1 i2) #\^) "A")])])))

(define memo (make-hash))

(define (solve2^ str n [pad num-pad] [pad-ty 'num])
  (if (dict-has-key? memo (cons str n))
      (dict-ref memo (cons str n))
      (cond
        [(zero? n) (string-length str)]
        [else
         (define parts
           (map (λ (l1 l2) (dist^^ (pad l1) (pad l2) pad-ty))
                (cons #\A (drop-right (string->list str) 1))
                (string->list str)))
         (define res (sum (map (λ (p) (solve2^ p (sub1 n) arrow-pad 'dir)) parts)))
         (dict-set! memo (cons str n) res)
         res])))

(sum (map (λ (x) (* (string->number (substring x 0 3)) (solve2^ x 3))) input))
(sum (map (λ (x) (* (string->number (substring x 0 3)) (solve2^ x 26))) input))

