#lang racket

(require "util.rkt")
(require racket/hash)

(define input (map string->list (get-input 8)))

;; Freq : List<Position>
(define antennas
  (cdr
   (foldl (λ (row i-m)
            (let* ([i (car i-m)]
                   [m (cdr i-m)]
                   [xs (foldl (λ (cell j-m)
                                (let ([j (car j-m)]
                                      [m (cdr j-m)])
                                  (cons (add1 j)
                                        (if (not (eq? cell #\.))
                                            (hash-set m cell (cons (cons i j) (hash-ref m cell '())))
                                            m))))
                              (cons 0 (make-immutable-hash))
                              row)])
              (cons (add1 i) (hash-union m (cdr xs) #:combine/key (lambda (k v1 v2) (append v1 v2))))))
          (cons 0 (make-immutable-hash))
          input)))


(define (gen-nodes start dir)
  (if (inbounds start)
      (cons start (gen-nodes (cons (+ (car start) (car dir)) (+ (cdr start) (cdr dir))) dir))
      '()))

(define (all-anodes pp)
  (match-let ([`((,i1 . ,j1) (,i2 . ,j2)) pp])
    (let ([di (- i1 i2)]
          [dj (- j1 j2)])
      (append (gen-nodes (cons i1 j1) (cons di dj))
              (gen-nodes (cons i2 j2) (cons (- di) (- dj)))))))

(define (pair-anodes pp)
  (match-let ([`((,i1 . ,j1) (,i2 . ,j2)) pp])
    (let ([di (- i1 i2)]
          [dj (- j1 j2)])
      (list (cons (+ i1 di) (+ j1 dj))
            (cons (- i2 di) (- j2 dj))))))

(define (inbounds p)
  (match-let ([(cons i j) p])
    (and (>= i 0)
         (>= j 0)
         (< i (length input))
         (< j (length (car input))))))

(define ((get-antinodes f) pair-ls)
  (list->set
   (filter inbounds
           (append-map f
                       (filter (λ (p) (not (equal? (car p) (cadr p)))) (cartesian-product pair-ls pair-ls))))))

(set-count (apply set-union (map (get-antinodes pair-anodes) (hash-values antennas))))
(set-count (apply set-union (map (get-antinodes all-anodes) (hash-values antennas))))
