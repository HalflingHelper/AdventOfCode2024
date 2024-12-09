#lang racket

(require "util.rkt")

(define input (get-input 9)) ;; A Single Line

(define data (map char->num (string->list (car input))))

(define chunks (map (λ (n i) (make-list n (if (even? i) (/ i 2) #\.))) data (range (length data))))

(define disk (list->vector (apply append chunks)))

(define (checksum ls [i 0])
  (cond
    [(null? ls) 0]
    [(eq? (car ls) #\.) (checksum (cdr ls) (add1 i))]
    [else (+ (* i (car ls)) (checksum (cdr ls) (add1 i)))]))

(define (solve1! i j)
  (when (>= j i)
      (let ([c1 (vector-ref disk i)]
            [c2 (vector-ref disk j)])
        (cond
          [(eq? c2 #\.) (solve1! i (sub1 j))]
          [(eq? c1 #\.) (vector-set! disk i c2) (vector-set! disk j #\.) (solve1! (add1 i) (sub1 j))]
          [else (solve1! (add1 i) j)]))))

(solve1! 0 (sub1 (vector-length disk)))
(checksum (vector->list disk))

;; Solve 2, start at back, find the first available space, diminish it, update disk and space
(define data2 (list->vector data)) ;; Memo for available space
(define disk2 (list->vector chunks))

(define (fill-space ls id cnt)
  (cond
    [(or (null? ls) (zero? cnt)) ls]
    [(eq? (car ls) #\.) (cons id (fill-space (cdr ls) id (sub1 cnt)))]
    [else (cons (car ls) (fill-space (cdr ls) id cnt))]))

(define (find-first-dest size [acc 1])
  (cond
    [(>= acc (vector-length disk2)) #f]
    [(>= (vector-ref data2 acc) size) acc]
    [else (find-first-dest size (+ 2 acc))]))

(define (use-slot! src)
  (let ([size (vector-ref data2 src)]
        [id (car (vector-ref disk2 src))])
    (let ([dest (find-first-dest size)])
      (when (and dest (> src dest))
        (vector-set! data2 dest (- (vector-ref data2 dest) size))
        (vector-set! disk2 dest (fill-space (vector-ref disk2 dest) id size))
        (vector-set! disk2 src (make-list size #\.))))))

(for ([i (reverse (filter even? (range (vector-length disk2))))]) (use-slot! i))
(checksum (apply append (vector->list disk2)))
