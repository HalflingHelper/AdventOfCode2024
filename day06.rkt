#lang racket

(require "util.rkt")

(define input (map string->list (get-input 6)))

(define guardY (index-where input (λ (row) (member #\^ row))))
(define guardX (index-of (list-ref input guardY) #\^))
(define guardVx 0)
(define guardVy -1)

(define input-vec (list->vector (map list->vector input)))

(define (inbounds)
  (and (>= guardY 0) (< guardY (vector-length input-vec))
       (>= guardX 0) (< guardX (vector-length (vector-ref input-vec guardY)))))

(define (turn)
  (define newVx (* -1 guardVy))
  (define newVy (* 1 guardVx))
  (set! guardVx newVx)
  (set! guardVy newVy))

(define (turnl)
  (turn) (turn) (turn))

(define (advance)
  (set! guardX (+ guardX guardVx))
  (set! guardY (+ guardY guardVy)))

(define (retreat)
  (set! guardX (- guardX guardVx))
  (set! guardY (- guardY guardVy)))

(define (place i)
  (define ret (cur-tile))
  (vector-set! (vector-ref input-vec guardY) guardX i)
  ret)

(define (cur-tile)
  (vector-ref (vector-ref input-vec guardY) guardX))

(define (cur-state)
  (list guardX guardY guardVx guardVy))

(define (place-walk)
  (place #\X))

(define (pickup)
  (place #\.))

(define (obstacle)
  (place #\#))

(define g_hist (make-hash))

(define (loop-if-turn)
  (define ret #f)
  (define hist (make-hash))
  (dict-set! hist (cur-state) #t)
  (advance)
  (define i (obstacle))
  (retreat)
  (turn)
  (define (loop)
    (if (inbounds)
        (cond
          [(or (dict-ref hist (cur-state) #f) (dict-ref g_hist (cur-state) #f)) (set! ret #t)]
          [(eq? (cur-tile) #\#) (retreat) (turn) (loop) (turnl) (advance)]
          [else (dict-set! hist (cur-state) #t) (advance) (loop) (retreat) ])
        #f))
  (loop)
  (turnl)
  (advance)
  (place i)
  (retreat)
  ret)
    

(define obstacles (set (cons guardX guardY)))

(define (solve1)
  (if (inbounds)
      (let ([x (vector-ref (vector-ref input-vec guardY) guardX)])
        (place-walk) ;; Mark position
        (advance)
        (if (and (inbounds) (eq? (cur-tile) #\.) (not (set-member? obstacles (cons guardX guardY))))
            (begin
              (retreat)
              (when (loop-if-turn)
                (set! obstacles (set-add obstacles (cons (+ guardX guardVx) (+ guardY guardVy))))))
            (retreat))
        (advance)
        (if (inbounds)
            (begin
              (retreat)
              (dict-set! g_hist (cur-state) #t)
              (if (eq? (vector-ref (vector-ref input-vec (+ guardY guardVy)) (+ guardX guardVx)) #\#)
                  (turn)
                  (advance)))
            (solve1))
        (solve1))
      (sum (vector->list (vector-map (curry vector-count (λ (x) (and (not (eq? x #\.)) (not (eq? x #\#))))) input-vec)))))

(solve1)

(sub1 (set-count obstacles))
