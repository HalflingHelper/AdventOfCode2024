#lang racket

(require "util.rkt")

(define input (get-input 4))

(define (transpose ls)
  (map list->string (apply map list (map string->list ls))))

(define (diag-pad ls)
  (map (λ (pad-amt s)
         (string-append
          (build-string pad-amt (const #\0)) s
          (build-string (- (length ls) pad-amt) (const #\0))))
       (range (length ls))
       ls))

(define inputT (transpose input))
(define input-diag-pad (transpose (diag-pad input)))
(define input-rdiag-pad (transpose (diag-pad (reverse inputT))))

(define (count-xmassmax str)
  (let ((m (regexp-match-positions #px"XMAS|SAMX" str)))
    (match m
      [`((,s . ,e)) (add1 (count-xmassmax (substring str (add1 s))))]
      [#f 0])))

(+
 (sum (map count-xmassmax input))
 (sum (map count-xmassmax inputT))
 (sum (map count-xmassmax input-diag-pad))
 (sum (map count-xmassmax input-rdiag-pad)))

;; P2 
(define (find-A-str str)
  (let ((m (regexp-match-positions #rx"A" str)))
    (match m
      [`((,s . ,e)) (cons s (map (λ (x) (+ (add1 s) x))
                                 (find-A-str (substring str (add1 s)))))]
      [#f '()])))

(define (find-A-ls ls [acc 0])
  (cond
    [(null? ls) '()]
    [else (append (map ((curry cons) acc) (find-A-str (car ls)))
                (find-A-ls (cdr ls) (add1 acc)))]))

(define (count x ls)
  (length (filter ((curry string=?) x) ls)))

(define (list-ref^ ls i [default "0"])
  (if (or (< i 0) (>= i (length ls))) default
      (list-ref ls i)))

(define (substring^ str i [default ""])
  (if (or (< i 0) (>= i (string-length str))) default
      (substring str i (add1 i))))

(define ((is-x-mas ls) pos)
  (match-let ((`(,r . ,c) pos))
    (let ((l (list
              (substring^ (list-ref^ ls (add1 r)) (add1 c))
              (substring^ (list-ref^ ls (add1 r)) (sub1 c))
              (substring^ (list-ref^ ls (sub1 r)) (add1 c))
              (substring^ (list-ref^ ls (sub1 r)) (sub1 c)))))
      (if (and (eq? (count "M" l) 2)
               (eq? (count "S" l) 2)
               (not (string=? (list-ref l 0) (list-ref l 3)))) 1 0))))

(sum (map (is-x-mas input) (find-A-ls input)))
                