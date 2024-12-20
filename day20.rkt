#lang racket

(require "util.rkt")
(define input (list->vector (map (compose list->vector string->list) (get-input 20))))

(define (get-start grid [i 0] [j 0])
  (cond
    [(eq? i (vector-length grid)) #f]
    [(eq? j (vector-length (vector-ref grid i))) (get-start grid (add1 i) 0)]
    [(eq? (vector-ref (vector-ref grid i) j) #\S) (list i j 0)]
    [else (get-start grid i (add1 j))]))

(define start (get-start input))

(define add2 (compose add1 add1))
(define sub2 (compose sub1 sub1))

(define (possible-cheats i j)
  (list (cons (add2 i) j)
        (cons (sub2 i) j)
        (cons i (add2 j))
        (cons i (sub2 j))))

(define (neighbors i j score)
  (list (list (add1 i) j (add1 score))
        (list (sub1 i) j (add1 score))
        (list i (add1 j) (add1 score))
        (list i (sub1 j) (add1 score))))

(define visited (make-hash))

;; Fill the memo and get the easy cheats for part 1
(define (bfs-with-cheats q acc)
  (match q
    ['() acc]
    [`((,i ,j ,score) . ,rest) #:when (dict-has-key? visited (cons i j))
                               (bfs-with-cheats rest acc)]
    [`((,i ,j ,score) . ,rest) #:when (eq? #\# (vector-ref (vector-ref input i) j))
                               (bfs-with-cheats rest acc)]
    [`((,i ,j ,score) . ,rest)
     (define-values (q^^ acc^^)
     (for/lists (q^ acc^ #:result (values (filter (λ (x) x) q^) (filter (λ (x) x) acc^)))
                ([n (neighbors i j score)] [pc (possible-cheats i j)])
       (match-let ([(list i^ j^ score^) n] [(cons is js) pc])
         (cond
           [(eq? #\# (vector-ref (vector-ref input i^) j^))
            (values #f (if (dict-has-key? visited pc)
                           (sub2 (- score (dict-ref visited pc))) #f))] 
           [else (values n #f)]))))
     (dict-set! visited (cons i j) score)
     (bfs-with-cheats (append rest q^^) (append acc acc^^))]))

(length (filter (λ (x) (>= x 100)) (bfs-with-cheats (list start) '())))

(define (manhattan a b)
  (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b)))))

;; Checks that there's a path through the walls. Not actually needed because of input layout.
(define (can-cheat q goal dist [vs (set)])
  (match q
    ['() #f]
    [`((,i ,j ,d) . ,rest)#:when(set-member? vs (cons i j)) (can-cheat rest goal dist vs)]
    [`((,i ,j ,d) . ,rest)#:when(> d dist) #f]
    [`((,i ,j ,d) . ,rest)#:when(equal? (cons i j) goal) #t]
    [`((,i ,j ,d) . ,rest)#:when(or (< i 0) (< j 0)
                                    (>= i (vector-length input))
                                    (>= j (vector-length (vector-ref input i))))
                          (can-cheat rest goal dist (set-add vs (cons i j)))]
    [`((,i ,j ,d) . ,rest)#:when (not (eq? (vector-ref (vector-ref input i) j) #\#))
                          (cons (list i j d)
                          (can-cheat rest goal dist (set-add vs (cons i j))))]
    [`((,i ,j ,d) . ,rest)
     (can-cheat (append q (neighbors i j d)) goal dist (set-add vs (cons i j)))]))

(define (cursed ls)
  (cond
    [(null? ls) 0]
    [else (let ([x (count (λ (b)
                                (let ([m (manhattan (car ls) b)])
                                  (and (<= m 20)
                                       (>= (- (abs (- (hash-ref visited (car ls)) (hash-ref visited b))) m) 100)
                                       #;(can-cheat (list (list (caar ls) (cdar ls) 0)) b 20)))) (cdr ls))])
            (+ x (cursed (cdr ls))))]))

(cursed (hash-keys visited))
