#lang racket

;; Ability score tables and the combinatorics over them.
;;
;; These tables used to be duplicated verbatim in pathfinder-character.rkt and
;; all_ability_scores.rkt; they live here so the two stay in step.

(provide ability->cost
         ability->bonus-points
         abilities-cost
         bonus-points-of-abilities
         legal-ability-scores
         all-ability-scores
         combinations-with-repetition
         for-each-combination
         legal-sets-by-cost)

;; Scores 1-6 and 19-45 are not legal purchase values; they are extrapolations
;; of the published table, kept so that rolled characters (which can fall
;; outside the purchase range) can still be priced and compared.
(define COSTS
  #hash((1 . -25) (2 . -20) (3 . -16) (4 . -12) (5 . -9) (6 . -6) (7 . -4)
        (8 . -2) (9 . -1) (10 . 0) (11 . 1) (12 . 2) (13 . 3) (14 . 5)
        (15 . 7) (16 . 10) (17 . 13) (18 . 17) (19 . 21) (20 . 26) (21 . 31)
        (22 . 37) (23 . 43) (24 . 50) (25 . 57) (26 . 65) (27 . 73) (28 . 82)
        (29 . 91) (30 . 101) (31 . 111) (32 . 122) (33 . 133) (34 . 145)
        (35 . 157) (36 . 170) (37 . 183) (38 . 197) (39 . 211) (40 . 226)
        (41 . 241) (42 . 257) (43 . 273) (44 . 290) (45 . 307)))

(define BONUSES
  #hash((1 . -5) (2 . -4) (3 . -4) (4 . -3) (5 . -3) (6 . -2) (7 . -2)
        (8 . -1) (9 . -1) (10 . 0) (11 . 0) (12 . 1) (13 . 1) (14 . 2)
        (15 . 2) (16 . 3) (17 . 3) (18 . 4) (19 . 4) (20 . 5) (21 . 5)
        (22 . 6) (23 . 6) (24 . 7) (25 . 7) (26 . 8) (27 . 8) (28 . 9)
        (29 . 9) (30 . 10) (31 . 10) (32 . 11) (33 . 11) (34 . 12) (35 . 12)
        (36 . 13) (37 . 13) (38 . 14) (39 . 14) (40 . 15) (41 . 15) (42 . 16)
        (43 . 16) (44 . 17) (45 . 17)))

(define (ability->cost n) (hash-ref COSTS n))
(define (ability->bonus-points n) (hash-ref BONUSES n))

(define (abilities-cost ab) (apply + (map ability->cost ab)))
(define (bonus-points-of-abilities ab) (apply + (map ability->bonus-points ab)))

;; Descending so that generated combinations come out sorted descending.
(define (legal-ability-scores) (range 18 6 -1))
(define (all-ability-scores) (range 45 0 -1))

;; Every k-element multiset drawn from pool, preserving pool ordering.
;;
;; The nested-loop version this replaces ran 12^6 = 2,985,984 iterations and
;; discarded duplicates through a set to arrive at 12,376 distinct results.
;; These are just multisets: C(17,6) = 12,376, generated directly.
(define (combinations-with-repetition pool k)
  (cond
    [(zero? k) '(())]
    [(null? pool) '()]
    [else
     (append
      (map (lambda (rest) (cons (car pool) rest))
           (combinations-with-repetition pool (sub1 k)))
      (combinations-with-repetition (cdr pool) k))]))

;; Same enumeration, but calls proc on each combination instead of building the
;; whole list. Needed for the 45-score case, where the list would be 15,890,700
;; elements long.
(define (for-each-combination pool k proc)
  (let loop ([pool pool] [k k] [acc '()])
    (cond
      [(zero? k) (proc (reverse acc))]
      [(null? pool) (void)]
      [else
       (loop pool (sub1 k) (cons (car pool) acc))
       (loop (cdr pool) k acc)])))

;; Legal spreads grouped by total purchase cost, computed once on first use.
;; `delay` gives compute-once semantics without the memoize package.
(define legal-sets-promise
  (delay
    (for/fold ([table (hash)])
              ([abils (in-list (combinations-with-repetition (legal-ability-scores) 6))])
      (hash-update table (abilities-cost abils) (lambda (v) (cons abils v)) '()))))

(define (legal-sets-by-cost) (force legal-sets-promise))
