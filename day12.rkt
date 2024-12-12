#lang racket

(require "util.rkt")
(define input (get-input 12))
(define data (list->vector (map (compose list->vector string->list) input)))

(define (neighbors i j)
  (list (cons (add1 i) j)
        (cons (sub1 i) j)
        (cons i (add1 j))
        (cons i (sub1 j))))

(define (findall ls pred)
  (cond
    [(null? ls) #f]
    [(pred (car ls)) (cons (car ls) (or (findall (cdr ls) pred) '()))]
    [else (findall (cdr ls) pred)]))

(define (merge-regions ls)
  (cond
    [(null? ls) '()]
    [(null? (cdr ls)) (car ls)]
    [else (cons (caar ls) (set-union (cdar ls) (cdr (merge-regions (cdr ls)))))]))

(define (rm-regions all-regions rs-to-rm)
  (filter-not (λ (r) (memq r rs-to-rm)) all-regions))

(define (build-regions i j all-regions)
  (cond
    [(>= i (vector-length data)) all-regions]
    [(>= j (vector-length (vector-ref data i))) (build-regions (add1 i) 0 all-regions)]
    [else (let ([ns (neighbors i j)]
                [plant (vector-ref (vector-ref data i) j)])
            (let ([regions (findall all-regions (λ (r) (and (eq? (car r) plant)
                                                            (findf (λ (n) (set-member? (cdr r) n)) (neighbors i j)))))])
              (cond
                [(not regions) (build-regions i (add1 j) (cons (cons plant (set (cons i j))) all-regions))] 
                [else (build-regions i (add1 j) (cons (merge-regions (cons (cons plant (set (cons i j))) regions))
                                                      (rm-regions all-regions regions)))])))]))

(define (area region)
  (set-count (cdr region)))

(define (perimeter region)
  (length (append-map (λ (coord)
                        (filter-not
                         (λ (n) (set-member? (cdr region) n))
                         (neighbors (car coord) (cdr coord))))
                      (set->list (cdr region)))))

(define (corner-pairs i j)
  (list (cons (cons (add1 i) j) (cons i (add1 j)))
        (cons (cons (add1 i) j) (cons i (sub1 j)))
        (cons (cons (sub1 i) j) (cons i (add1 j)))
        (cons (cons (sub1 i) j) (cons i (sub1 j)))))

(define (corners i j rc)
  (length (filter (λ (cps)
                    (or
                     (and (not (set-member? rc (car cps))) (not (set-member? rc (cdr cps))))
                     (and (set-member? rc (car cps))
                          (set-member? rc (cdr cps))
                          (not (set-member? rc (cons (caar cps) (cddr cps)))))))
                  (corner-pairs i j))))

(define (sides region)
  (sum (map (λ (coord)
              (corners (car coord) (cdr coord) (cdr region)))
            (set->list (cdr region)))))

(define regions (build-regions 0 0 '()))

(sum (map (λ (r) (* (area r) (perimeter r))) regions))
(sum (map (λ (r) (* (area r) (sides r))) regions))
