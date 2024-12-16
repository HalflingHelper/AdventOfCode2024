#lang racket

(require "util.rkt")
(define input (get-input 16))
(require data/heap)

(define grid (list->vector (map (compose list->vector string->list) input)))

(define (<^ a b)
  (< (first a) (first b)))

(define memo (make-hash))
(define fringe! (make-heap <^))

(define (search! [found #f])
  (cond
    [(zero? (heap-count fringe!)) found]
    [else (match-let ([(list score i j di dj cells) (heap-min fringe!)])
            (heap-remove-min! fringe!)
            (cond
              [(and (hash-has-key? memo (list i j di dj))
                    (> score (first (hash-ref memo (list i j di dj)))))
               (search! found)]
              [(and (hash-has-key? memo (list i j di dj))
                    (eq? score (first (hash-ref memo (list i j di dj)))))
               (heap-remove! fringe! (list score i j di dj (second (hash-ref memo (list i j di dj)))))
               (heap-add! fringe! (list score i j di dj (set-union (second (hash-ref memo (list i j di dj))) cells)))
               (hash-remove! memo (list i j di dj))
               (search! found)]
              [(eq? (vector-ref (vector-ref grid i) j) #\E)
               (cond
                 [(and found (< (caar found) score)) found]
                 [(and found (eq? (caar found) score))
                  (search! (cons (list score i j di dj cells) found))]
                 [else (search! (list (list score i j di dj cells)))])]
              [(eq? (vector-ref (vector-ref grid i) j) #\#) (search! found)]
              [else (hash-set! memo (list i j di dj) (list score cells))
                    (heap-add-all! fringe!
                                   (list
                                    (if (zero? di) (list (+ 1000 score) i j (- dj) di (set-add cells (cons i j)))
                                        (list (+ 1000 score) i j dj (- di) (set-add cells (cons i j))))
                                    (list (+ 1000 score) i j dj di (set-add cells (cons i j)))
                                    (list (+ 1 score) (+ i di) (+ j dj) di dj (set-add cells (cons i j)))))
                    (search! found)]))]))
                                        
(heap-add-all! fringe! `((0 ,(- (vector-length grid) 2) 1 0 1 ,(set (cons 0 (- (vector-length grid) 2))))))
(define paths (search!))

(caar paths)
(set-count (foldl (λ (a b) (set-union (sixth a) b)) (set)
                  (filter (λ (p) (and (eq? (third p) (third (car paths))) (eq? (second p) (second (car paths)))))
                          paths)))
