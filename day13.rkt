#lang racket

(require "util.rkt")
(define input (get-input 13))

(define (parse input)
  (match-let ([`(,_ ,ax ,ay) (regexp-match #px"X\\+(\\d+), Y\\+(\\d+)" (first input))]
              [`(,_ ,bx ,by) (regexp-match #px"X\\+(\\d+), Y\\+(\\d+)" (second input))]
              [`(,_ ,px ,py) (regexp-match #px"X=(\\d+), Y=(\\d+)" (third input))])
    (cons (map string->number (list ax ay bx by px py))
          (if (null? (cdddr input)) '() (parse (cddddr input))))))

(define ((best-score part) mach)
  (match mach
    [(list ax ay bx by px py)
     (let ([px (if (eq? part 2) (+ 10000000000000 px) px)]
           [py (if (eq? part 2) (+ 10000000000000 py) py)])
       (let ([m (/ (- (* py ax) (* px ay)) (- (* by ax) (* bx ay)))]
             [n (/ (- (* py bx) (* px by)) (- (* ay bx) (* ax by)))])
         (if (and (integer? n) (integer? m)) (+ (* 3 n) m) 0)))]))

(define machines (parse input))

(sum (map (best-score 1) machines))
(sum (map (best-score 2) machines))
