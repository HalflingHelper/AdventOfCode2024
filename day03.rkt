#lang racket

(require "util.rkt")

(define input (get-input 3))
(define data (apply string-append input))

(define (solve str [p1? #f] [flag #t])
  (let ([m (regexp-match-positions #px"mul\\((\\d+),(\\d+)\\)" str)]
        [do (regexp-match-positions #rx"do\\(\\)" str)]
        [dont (regexp-match-positions #rx"don't\\(\\)" str)]
        [sl (string-length str)])
    (match* (m do dont)
      [(`((,s . ,e) (,ns . ,ne) (,ms . ,me)) dos donts)
       (let* ([n (string->number (substring str ns ne))]
              [m (string->number (substring str ms me))]
              [rest (substring str e)]
              [dos^ (or (and dos (caar dos)) sl)]
              [donts^ (or (and donts (caar donts)) sl)]
              [v (min dos^ donts^ s)])
         (cond
           [(eq? v dos^) (solve (substring str (cdar dos)) p1? #t)]
           [(eq? v donts^) (solve (substring str (cdar donts)) p1? #f)]
           [(+ (if (or p1? flag) (* m n) 0) (solve rest p1? flag))]))]
      [(#f _ _) 0])))

(solve data 'p1)
(solve data)
