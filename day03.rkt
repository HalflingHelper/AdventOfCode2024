#lang racket

(require "util.rkt")

(define input (get-input 3))
(define data (apply string-append input))

(define (solve^ str [p1? #f] [flag #t])
  (let ([m (regexp-match-positions
            #px"mul\\((\\d+),(\\d+)\\)|do.{0,3}\\(\\)" str)])
    (match m
      [`((,s . ,e) (,ns . ,ne) (,ms . ,me)) 
       (let* ([n (string->number (substring str ns ne))]
              [m (string->number (substring str ms me))])
         (+ (if (or p1? flag) (* m n) 0) (solve^ (substring str e) p1? flag)))]
      [`((,s . ,e) #f #f)
       #:when (string=? (substring str s e) "do()")
       (solve^ (substring str e) p1? #t)]
      [`((,s . ,e) #f #f)
       #:when (string=? (substring str s e) "don't()")
       (solve^ (substring str e) p1? #f)]
      [_ 0])))

(solve^ data 'p1)
(solve^ data)
