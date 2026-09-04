#lang racket

;; Unit tests for the library modules, plus end-to-end checks that run the
;; command line programs. Run with: raco test tests.rkt

(require rackunit
         rackunit/text-ui
         racket/system
         "abilities.rkt"
         "cli-args.rkt"
         "notation.rkt")

;; ---------------------------------------------------------------------------
;; helpers

;; Parses and renders, which is the clearest way to assert on a whole expression.
(define (rendered text)
  (define-values (e err) (parse-dice-notation text))
  (if err (string-append "ERROR: " err) (expr->string e)))

(define (parse-error text)
  (define-values (e err) (parse-dice-notation text))
  err)

(define (roll-of text)
  (define-values (b err) (parse-roll text))
  (if err
      (string-append "ERROR: " err)
      (list (batch-repeat b) (expr->string (batch-expr b)))))

(define (roll-error text)
  (define-values (b err) (parse-roll text))
  err)

(define (roll-batch text)
  (define-values (b err) (parse-roll text))
  b)

;; The displayed value read back exactly, so a rounding check needs no epsilon.
(define (shown-as-exact kind totals)
  (string->number (aggregate->string kind totals) 10 'number-or-false 'decimal-as-exact))

(define (only-spec text)
  (define-values (e err) (parse-dice-notation text))
  (car (filter spec? (map cdr (expr-terms e)))))

(define (program-output program . args)
  (define out (open-output-string))
  (parameterize ([current-output-port out] [current-error-port out])
    (apply system*/exit-code (find-executable-path "racket") program args))
  (get-output-string out))

(define (program-exit-code program . args)
  (define sink (open-output-string))
  (parameterize ([current-output-port sink] [current-error-port sink])
    (apply system*/exit-code (find-executable-path "racket") program args)))

;; ---------------------------------------------------------------------------

(define notation-tests
  (test-suite
   "dice notation"

   (test-case "single group renders canonically"
     (check-equal? (rendered "4d6k3+2") "4D6K3+2")
     (check-equal? (rendered "3d6") "3D6")
     (check-equal? (rendered "1d20+4") "1D20+4")
     (check-equal? (rendered "1d20-1") "1D20-1")
     (check-equal? (rendered "1d20*2") "1D20*2"))

   (test-case "keep defaults to every die"
     (check-equal? (spec-keep (only-spec "3d6")) 3)
     (check-eq? (spec-from (only-spec "3d6")) 'high))

   (test-case "case insensitive"
     (check-equal? (rendered "4D6K3") (rendered "4d6k3"))
     (check-equal? (rendered "2D20KL1") (rendered "2d20kl1")))

   (test-case "whitespace tolerated"
     (check-equal? (rendered "  3d6 + 2 ") "3D6+2")
     (check-equal? (rendered "2d6 + 1d8") "2D6+1D8"))

   (test-case "k defaults to keeping the highest"
     (check-eq? (spec-from (only-spec "4d6k3")) 'high)
     (check-eq? (spec-from (only-spec "4d6kh3")) 'high)
     (check-equal? (rendered "4d6kh3") "4D6K3"))

   (test-case "kl keeps the lowest, which is disadvantage"
     (check-eq? (spec-from (only-spec "2d20kl1")) 'low)
     (check-equal? (spec-keep (only-spec "2d20kl1")) 1)
     (check-equal? (rendered "2d20kl1") "2D20KL1"))

   (test-case "dl drops the lowest, which is keeping the highest of the rest"
     (check-equal? (spec-keep (only-spec "4d6dl1")) 3)
     (check-eq? (spec-from (only-spec "4d6dl1")) 'high)
     (check-equal? (rendered "4d6dl1") "4D6K3"))

   (test-case "dh drops the highest, which is keeping the lowest of the rest"
     (check-equal? (spec-keep (only-spec "4d6dh1")) 3)
     (check-eq? (spec-from (only-spec "4d6dh1")) 'low)
     (check-equal? (rendered "4d6dh1") "4D6KL3"))

   (test-case "keeping every die renders without a keep clause"
     (check-equal? (rendered "4d6k4") "4D6")
     (check-equal? (rendered "4d6kl4") "4D6"))

   (test-case "multiple groups"
     (check-equal? (rendered "2d6+1d8") "2D6+1D8")
     (check-equal? (rendered "2d6-1d4") "2D6-1D4")
     (check-equal? (rendered "2d6+1d8-1") "2D6+1D8-1")
     (check-equal? (rendered "1d20+2d6+3") "1D20+2D6+3")
     (check-equal? (rendered "4d6k3+2d20kl1") "4D6K3+2D20KL1"))

   (test-case "trailing multiplier scales the whole expression"
     (check-equal? (rendered "2d6+3*2") "2D6+3*2"))

   (test-case "malformed notation is rejected"
     (for ([bad '("d20" "4d" "4x6" "" "4d6k" "abc" "2d6+" "4d6d1" "4d6kx3")])
       (check-true (string? (parse-error bad))
                   (format "expected ~s to fail" bad))))

   (test-case "spec validation errors propagate with the original messages"
     (check-equal? (parse-error "2d6k5") "dice must be greater than or equal to keep.")
     (check-equal? (parse-error "2d0") "sides must be greater than 0.")
     (check-equal? (parse-error "0d6") "dice must be greater than 0.")
     (check-equal? (parse-error "4d6dl4") "keep must be greater than 0."))

   (test-case "a multiplier that is not last is rejected"
     (check-true (regexp-match? #rx"must come last" (parse-error "2d6*2+3"))))

   (test-case "a leading repeat count is parsed separately from the expression"
     (check-equal? (roll-of "6x4d6k3") '(6 "4D6K3"))
     (check-equal? (roll-of "10X2d20kl1") '(10 "2D20KL1"))
     (check-equal? (roll-of "4d6k3") '(1 "4D6K3"))
     (check-equal? (roll-of "2d6+1d8-1") '(1 "2D6+1D8-1")))

   (test-case "a repeat count must be greater than zero"
     (check-equal? (roll-error "0x3d6") "repeat count must be greater than 0."))

   (test-case "a roll must contain dice"
     (check-equal? (roll-error "7") "expression contains no dice: \"7\"")
     (check-equal? (roll-error "2+3") "expression contains no dice: \"2+3\""))

   (test-case "a trailing x is not a repeat count"
     (check-true (string? (roll-error "2d6x3")))
     (check-true (string? (roll-error "6x"))))

   (test-case "an aggregate wraps the whole roll"
     (check-eq? (batch-aggregate (roll-batch "sum(6x4d6k3)")) 'sum)
     (check-equal? (batch-repeat (roll-batch "sum(6x4d6k3)")) 6)
     (check-equal? (expr->string (batch-expr (roll-batch "sum(6x4d6k3)"))) "4D6K3"))

   (test-case "the colon form needs no shell quoting and means the same"
     (check-equal? (roll-batch "sum:6x4d6k3") (roll-batch "sum(6x4d6k3)")))

   (test-case "an aggregate needs no repeat count"
     (check-eq? (batch-aggregate (roll-batch "avg(4d6k3)")) 'avg)
     (check-equal? (batch-repeat (roll-batch "avg(4d6k3)")) 1))

   (test-case "an aggregate is case insensitive and tolerates whitespace"
     (check-equal? (roll-batch "SUM ( 6 X 4d6k3 )") (roll-batch "sum(6x4d6k3)")))

   (test-case "an unaggregated roll still describes one roll"
     (check-equal? (batch->string (roll-batch "6x4d6k3")) "4D6K3")
     (check-false (batch-aggregate (roll-batch "6x4d6k3"))))

   (test-case "an aggregate renders with the repeat it depends on"
     (check-equal? (batch->string (roll-batch "sum(6x4d6k3)")) "SUM(6x4D6K3)")
     (check-equal? (batch->string (roll-batch "sum(4d6k3)")) "SUM(1x4D6K3)")
     (check-equal? (batch->string (roll-batch "MAX(2X4d6dl1)")) "HIGH(2x4D6K3)"))

   (test-case "an unknown aggregate is rejected by name"
     (check-equal? (roll-error "worst(6x4d6k3)")
                   "unknown aggregate \"worst\"; use sum, avg, high, low, or median."))

   (test-case "an aggregate propagates the errors of the roll it wraps"
     (check-equal? (roll-error "sum(0x3d6)") "repeat count must be greater than 0.")
     (check-equal? (roll-error "sum(3x2d6k5)") "dice must be greater than or equal to keep.")
     (check-equal? (roll-error "sum(3x2)") "expression contains no dice: \"sum(3x2)\""))

   (test-case "an unclosed aggregate is just unparseable notation"
     (check-equal? (roll-error "sum(4d6") "could not parse dice notation: \"sum(4d6\""))

   ;; The aggregate and the repeat count are stripped before the expression is
   ;; parsed, but an error still names the roll the way it was written.
   (test-case "a parse error quotes the roll as written"
     (check-equal? (roll-error "4d6 k") "could not parse dice notation: \"4d6 k\"")
     (check-equal? (roll-error "6x4d6 k") "could not parse dice notation: \"6x4d6 k\"")
     (check-equal? (roll-error "sum(4d6 k)")
                   "could not parse dice notation: \"sum(4d6 k)\""))))

(define aggregate-tests
  (test-suite
   "aggregates"

   (test-case "every alias resolves to its kind"
     (for ([n '("sum" "total")]) (check-eq? (parse-aggregate n) 'sum))
     (for ([n '("avg" "average" "mean")]) (check-eq? (parse-aggregate n) 'avg))
     (for ([n '("high" "highest" "max")]) (check-eq? (parse-aggregate n) 'high))
     (for ([n '("low" "lowest" "min")]) (check-eq? (parse-aggregate n) 'low))
     (for ([n '("median" "med")]) (check-eq? (parse-aggregate n) 'median)))

   (test-case "names are matched case insensitively"
     (check-eq? (parse-aggregate "SUM") 'sum)
     (check-eq? (parse-aggregate "Median") 'median))

   (test-case "anything else is not an aggregate"
     (for ([bad '("worst" "best" "sums" "s" "")])
       (check-false (parse-aggregate bad) (format "expected ~s not to parse" bad))))

   (test-case "every alias renders under one canonical name"
     (check-equal? (aggregate->notation 'sum) "SUM")
     (check-equal? (aggregate->notation 'avg) "AVG")
     (check-equal? (aggregate->notation 'high) "HIGH")
     (check-equal? (aggregate->notation 'low) "LOW")
     (check-equal? (aggregate->notation 'median) "MEDIAN"))

   (test-case "every canonical name parses back to its kind"
     (for ([n (aggregate-names)])
       (check-equal? (aggregate->notation (parse-aggregate n)) (string-upcase n))))

   (test-case "reduce computes the exact value"
     (check-equal? (aggregate-reduce 'sum '(14 12 3)) 29)
     (check-equal? (aggregate-reduce 'avg '(14 12 3)) 29/3)
     (check-equal? (aggregate-reduce 'high '(14 12 3)) 14)
     (check-equal? (aggregate-reduce 'low '(14 12 3)) 3)
     (check-equal? (aggregate-reduce 'median '(14 12 3)) 12)
     (check-equal? (aggregate-reduce 'median '(14 12 3 1)) 15/2))

   (test-case "reduce does not care what order the rolls arrived in"
     (for ([kind '(sum avg high low median)])
       (check-equal? (aggregate-reduce kind '(5 1 9 3))
                     (aggregate-reduce kind '(3 9 1 5)))))

   (test-case "a single roll is its own aggregate"
     (for ([kind '(sum avg high low median)])
       (check-equal? (aggregate-reduce kind '(13)) 13)))

   (test-case "every aggregate lands between the worst and the best roll"
     (for ([kind '(avg high low median)])
       (for ([totals '((5 1 9 3) (-4 -1) (7) (2 2 2 2 2))])
         (define v (aggregate-reduce kind totals))
         (check-true (and (>= v (apply min totals)) (<= v (apply max totals)))
                     (format "~a of ~a was ~a" kind totals v)))))

   (test-case "a whole result prints without a decimal point"
     (check-equal? (aggregate->string 'sum '(40 31)) "71")
     (check-equal? (aggregate->string 'avg '(12 12)) "12")
     (check-equal? (aggregate->string 'median '(11 13)) "12"))

   (test-case "a fractional result is rounded to two places"
     (check-equal? (aggregate->string 'avg '(14 12 3)) "9.67")
     (check-equal? (aggregate->string 'median '(14 11)) "12.5"))

   (test-case "a leading zero in the second place is kept"
     (check-equal? (aggregate->string 'avg '(1 0 0 0)) "0.25")
     (check-equal? (aggregate->string 'avg '(101 100 100 100)) "100.25"))

   ;; 3/40 is exactly 0.075, but the nearest double is a hair below it, so
   ;; rounding a flonum would show 0.07. Exact arithmetic does not, and neither
   ;; does the Elixir implementation, which rounds in whole hundredths.
   (test-case "a half rounds away from zero, in exact arithmetic"
     (check-equal? (aggregate->string 'avg (cons 3 (make-list 39 0))) "0.08")
     (check-equal? (aggregate->string 'avg (cons 7 (make-list 39 0))) "0.18")
     (check-equal? (aggregate->string 'avg (cons -3 (make-list 39 0))) "-0.08"))

   (test-case "a negative aggregate prints the same way"
     (check-equal? (aggregate->string 'sum '(-4 -1)) "-5")
     (check-equal? (aggregate->string 'median '(-14 -11)) "-12.5"))

   (test-case "a displayed average is never more than half a hundredth off"
     (for* ([n (in-range 1 25)] [sum (in-range -60 61)])
       (define totals (cons sum (make-list (sub1 n) 0)))
       (check-true (<= (abs (- (shown-as-exact 'avg totals) (/ sum n))) 1/200)
                   (format "~a/~a displayed as ~a" sum n (aggregate->string 'avg totals)))))))

(define roll-tests
  (test-suite
   "rolling"

   (test-case "keeps the right number of dice"
     (define-values (e err) (parse-dice-notation "4d6k3"))
     (define s (cdr (first (expr-terms e))))
     (for ([i (in-range 200)])
       (define-values (rolled kept sum) (roll-spec s))
       (check-equal? (length rolled) 4)
       (check-equal? (length kept) 3)
       (check-equal? sum (apply + kept))))

   (test-case "every die lands within range"
     (define-values (e err) (parse-dice-notation "10d6"))
     (define-values (rolled kept sum) (roll-spec (cdr (first (expr-terms e)))))
     (for ([d rolled]) (check-true (and (>= d 1) (<= d 6)))))

   (test-case "keep highest takes the largest dice"
     (define-values (e err) (parse-dice-notation "5d20k2"))
     (define s (cdr (first (expr-terms e))))
     (for ([i (in-range 100)])
       (define-values (rolled kept sum) (roll-spec s))
       (check-equal? kept (take (sort rolled >) 2))))

   (test-case "keep lowest takes the smallest dice"
     (define-values (e err) (parse-dice-notation "5d20kl2"))
     (define s (cdr (first (expr-terms e))))
     (for ([i (in-range 100)])
       (define-values (rolled kept sum) (roll-spec s))
       (check-equal? kept (take (sort rolled <) 2))))

   (test-case "a one-sided die is deterministic"
     (define-values (e err) (parse-dice-notation "3d1+2"))
     (define-values (groups total) (roll-expr e))
     (check-equal? groups '((1 1 1)))
     (check-equal? total 5))

   (test-case "the multiplier scales the whole total, not each die"
     (define-values (e err) (parse-dice-notation "3d1*2"))
     (define-values (groups total) (roll-expr e))
     (check-equal? total 6))

   (test-case "groups combine with their signs"
     (define-values (e err) (parse-dice-notation "2d1+1d1-1"))
     (define-values (groups total) (roll-expr e))
     (check-equal? (length groups) 2)
     (check-equal? total 2))

   (test-case "keep lowest really is lower on average than keep highest"
     (define (mean text n)
       (define-values (e err) (parse-dice-notation text))
       (/ (for/sum ([i (in-range n)])
            (define-values (groups total) (roll-expr e))
            total)
          (exact->inexact n)))
     (define low (mean "2d20kl1" 3000))
     (define high (mean "2d20kh1" 3000))
     (check-true (< low high))
     ;; theoretical means are 7.175 and 13.825
     (check-true (< (abs (- low 7.175)) 1.0) (format "kl1 mean was ~a" low))
     (check-true (< (abs (- high 13.825)) 1.0) (format "kh1 mean was ~a" high)))))

(define ability-tests
  (test-suite
   "ability tables"

   (test-case "boundary costs match the published table"
     (check-equal? (ability->cost 1) -25)
     (check-equal? (ability->cost 10) 0)
     (check-equal? (ability->cost 18) 17)
     (check-equal? (ability->cost 45) 307))

   (test-case "boundary bonuses match the published table"
     (check-equal? (ability->bonus-points 1) -5)
     (check-equal? (ability->bonus-points 10) 0)
     (check-equal? (ability->bonus-points 18) 4)
     (check-equal? (ability->bonus-points 45) 17))

   (test-case "every bonus follows floor((score - 10) / 2)"
     (for ([n (in-range 1 46)])
       (check-equal? (ability->bonus-points n)
                     (floor (/ (- n 10) 2))
                     (format "bonus(~a) disagrees with the published progression" n))))

   (test-case "cost rises monotonically"
     (for ([n (in-range 2 46)])
       (check-true (> (ability->cost n) (ability->cost (sub1 n))))))

   (test-case "the standard array costs the standard-fantasy budget"
     (check-equal? (abilities-cost '(15 14 13 12 10 8)) 15)
     (check-equal? (bonus-points-of-abilities '(15 14 13 12 10 8)) 5))))

(define combination-tests
  (test-suite
   "combinations"

   (test-case "enumerates the closed-form count"
     (define (expected n k)
       (for/fold ([acc 1]) ([i (in-range 1 (add1 k))])
         (/ (* acc (- (+ n k -1) (- k i))) i)))
     (for* ([n (in-range 1 7)] [k (in-range 0 4)])
       (check-equal? (length (combinations-with-repetition (range 1 (add1 n)) k))
                     (expected n k)
                     (format "n=~a k=~a" n k))))

   (test-case "the ability score case yields C(17,6)"
     (check-equal? (length (combinations-with-repetition (legal-ability-scores) 6)) 12376))

   (test-case "no duplicates"
     (define all (combinations-with-repetition (range 1 7) 4))
     (check-equal? (length (remove-duplicates all)) (length all)))

   (test-case "combinations preserve pool ordering"
     (for ([c (combinations-with-repetition (legal-ability-scores) 3)])
       (check-equal? c (sort c >))))

   (test-case "for-each-combination visits the same combinations"
     (define collected '())
     (for-each-combination (range 5 0 -1) 3 (lambda (c) (set! collected (cons c collected))))
     (check-equal? (reverse collected)
                   (combinations-with-repetition (range 5 0 -1) 3)))))

(define purchase-tests
  (test-suite
   "purchase table"

   (test-case "holds C(17,6) spreads"
     (check-equal? (for/sum ([(k v) (in-hash (legal-sets-by-cost))]) (length v)) 12376))

   (test-case "each campaign has the spread count the brute force produced"
     (check-equal? (length (hash-ref (legal-sets-by-cost) 10 '())) 225)
     (check-equal? (length (hash-ref (legal-sets-by-cost) 15 '())) 262)
     (check-equal? (length (hash-ref (legal-sets-by-cost) 20 '())) 280)
     (check-equal? (length (hash-ref (legal-sets-by-cost) 25 '())) 272))

   (test-case "every spread is grouped under its true cost"
     (for* ([(cost spreads) (in-hash (legal-sets-by-cost))] [s spreads])
       (check-equal? (abilities-cost s) cost)))

   (test-case "every spread uses only legal scores, sorted descending"
     (for* ([(cost spreads) (in-hash (legal-sets-by-cost))] [s spreads])
       (check-equal? (length s) 6)
       (check-equal? s (sort s >))
       (for ([score s]) (check-true (and (>= score 7) (<= score 18))))))

   (test-case "an unreachable budget has no spreads"
     (check-equal? (hash-ref (legal-sets-by-cost) 9999 '()) '()))))

(define reorder-tests
  (let ([value-flags '("-d" "--dice" "-s" "--sides")])
    (test-suite
     "argument reordering"

     (test-case "moves flags ahead of positionals"
       (check-equal? (reorder-args '("5" "-v") value-flags) '("-v" "5")))

     (test-case "keeps a flag with its value"
       (check-equal? (reorder-args '("5" "-d" "3") value-flags) '("-d" "3" "5")))

     (test-case "leaves an already-ordered line alone"
       (check-equal? (reorder-args '("-v" "3" "6") value-flags) '("-v" "3" "6")))

     (test-case "a negative number is a value, not a flag"
       (check-equal? (reorder-args '("3" "6" "-1") value-flags) '("3" "6" "-1"))
       (check-equal? (reorder-args '("3" "6" "-10") value-flags) '("3" "6" "-10")))

     ;; Racket reads "-i" and "+i" as the imaginary unit, so a string->number
     ;; test would classify the short form of --iterations as a value.
     (test-case "short flags that look like Racket numbers are still flags"
       (check-equal? (reorder-args '("1d1" "-i" "5") '("-i")) '("-i" "5" "1d1"))
       (check-equal? (reorder-args '("1d1" "-e") value-flags) '("-e" "1d1")))

     (test-case "everything after -- is positional"
       (check-equal? (reorder-args '("-v" "--" "-1" "2") value-flags) '("-v" "-1" "2")))

     (test-case "accepts a vector as well as a list"
       (check-equal? (reorder-args (vector "5" "-v") value-flags) '("-v" "5"))))))

(define cli-tests
  (test-suite
   "command line programs"

   (test-case "notation forms work end to end"
     (check-regexp-match #rx"^1D20 \\([0-9]+\\) => [0-9]+"
                         (program-output "dieroller.rkt" "-v" "1d20"))
     (check-regexp-match #rx"^5D20 \\([0-9 ]+\\) => [0-9]+"
                         (program-output "dieroller.rkt" "-v" "5d20"))
     (check-regexp-match #rx"^3D6\\+3 " (program-output "dieroller.rkt" "-v" "3d6+3"))
     (check-regexp-match #rx"^4D6K3 " (program-output "dieroller.rkt" "-v" "4d6k3"))
     (check-regexp-match #rx"^2D20KL1 " (program-output "dieroller.rkt" "-v" "2d20kl1"))
     (check-regexp-match #rx"^4D6KL3 " (program-output "dieroller.rkt" "-v" "4d6dh1"))
     (check-regexp-match #rx"^2D6\\+1D8-1 \\([0-9 ]+\\) \\([0-9]+\\) => "
                         (program-output "dieroller.rkt" "-v" "2d6+1d8-1")))

   (test-case "flags may follow the expression"
     (check-regexp-match #rx"^4D6K3 " (program-output "dieroller.rkt" "4d6k3" "-v")))

   (test-case "a quoted expression may contain spaces"
     (check-regexp-match #rx"^2D6\\+1D8 " (program-output "dieroller.rkt" "-v" "2d6 + 1d8")))

   (test-case "a repeat count produces one line per roll"
     (check-equal? (length (string-split (program-output "dieroller.rkt" "5x1d1") "\n")) 5)
     (check-equal? (length (string-split (program-output "dieroller.rkt" "1d1") "\n")) 1))

   (test-case "the repeat count is not part of the rendered notation"
     (check-equal? (program-output "dieroller.rkt" "-v" "3x1d1")
                   "1D1 (1) => 1\n1D1 (1) => 1\n1D1 (1) => 1\n"))

   (test-case "an aggregate reduces the repeats to one line"
     (check-equal? (program-output "dieroller.rkt" "sum(6x1d1)") "6\n")
     (check-equal? (program-output "dieroller.rkt" "avg(6x1d1)") "1\n")
     (check-equal? (program-output "dieroller.rkt" "high(6x1d1)") "1\n")
     (check-equal? (program-output "dieroller.rkt" "low(6x1d1)") "1\n")
     (check-equal? (program-output "dieroller.rkt" "median(6x1d1)") "1\n"))

   (test-case "the colon form means the same as the parenthesised one"
     (check-equal? (program-output "dieroller.rkt" "sum:6x1d1")
                   (program-output "dieroller.rkt" "sum(6x1d1)")))

   (test-case "an aggregate summarises exactly the rolls it replaces"
     (define rolls
       (map string->number (string-split (program-output "dieroller.rkt" "40x1d20") "\n")))
     (check-equal? (length rolls) 40)
     ;; The rolls differ from run to run, so check the shape rather than a value.
     (for ([args '(("sum:40x1d20") ("high:40x1d20") ("low:40x1d20") ("median:40x1d20"))])
       (check-regexp-match #px"^-?[0-9]+(\\.[0-9]+)?\n$"
                           (apply program-output "dieroller.rkt" args)))
     (check-regexp-match #px"^[0-9]+(\\.[0-9]{1,2})?\n$"
                         (program-output "dieroller.rkt" "avg:40x1d20")))

   (test-case "verbose shows each roll and then the summary"
     (check-equal? (program-output "dieroller.rkt" "-v" "sum(3x1d1)")
                   "1D1 (1) => 1\n1D1 (1) => 1\n1D1 (1) => 1\nSUM(3x1D1) => 3\n"))

   (test-case "an alias reaches its canonical notation end to end"
     (check-regexp-match #rx"HIGH\\(2x1D1\\) => 1"
                         (program-output "dieroller.rkt" "-v" "max:2x1d1"))
     (check-regexp-match #rx"AVG\\(2x1D1\\) => 1"
                         (program-output "dieroller.rkt" "-v" "mean:2x1d1")))

   (test-case "an aggregate without a repeat count is the single roll"
     (check-equal? (program-output "dieroller.rkt" "sum(3d1)") "3\n"))

   (test-case "an unknown aggregate is rejected"
     (check-not-equal? (program-exit-code "dieroller.rkt" "worst:6x4d6k3") 0)
     (check-regexp-match #rx"unknown aggregate \"worst\"; use sum, avg, high, low, or median\\."
                         (program-output "dieroller.rkt" "worst:6x4d6k3")))

   (test-case "version is reported by both programs"
     (check-regexp-match #rx"^dieroller [0-9]+\\.[0-9]+\\.[0-9]+"
                         (program-output "dieroller.rkt" "--version"))
     (check-regexp-match #rx"^dieroller [0-9]+\\.[0-9]+\\.[0-9]+"
                         (program-output "dieroller.rkt" "-V"))
     (check-regexp-match #rx"^pathfinder-character [0-9]+\\.[0-9]+\\.[0-9]+"
                         (program-output "pathfinder-character.rkt" "--version")))

   (test-case "validation errors exit non-zero"
     (for ([args '(("0d6") ("2d6k5") ("2d0") ("0x3d6") ("7"))])
       (check-not-equal? (apply program-exit-code "dieroller.rkt" args) 0
                         (format "expected ~a to fail" args))))

   (test-case "validation messages are unchanged"
     (check-regexp-match #rx"dice must be greater than 0\\."
                         (program-output "dieroller.rkt" "0d6"))
     (check-regexp-match #rx"dice must be greater than or equal to keep\\."
                         (program-output "dieroller.rkt" "2d6k5"))
     (check-regexp-match #rx"keep must be greater than 0\\."
                         (program-output "dieroller.rkt" "4d6dl4"))
     (check-regexp-match #rx"sides must be greater than 0\\."
                         (program-output "dieroller.rkt" "2d0")))

   ;; The flags and positional arguments the notation replaced.
   (test-case "the removed dice flags are no longer accepted"
     (for ([flag '("-d" "-s" "-k" "-m" "-i" "--dice" "--sides" "--keep" "--modifier" "--iterations")])
       (check-not-equal? (program-exit-code "dieroller.rkt" flag "3") 0
                         (format "expected ~a to be rejected" flag))))

   (test-case "the old positional form suggests its notation equivalent"
     (check-regexp-match #rx"try: dieroller 5d20" (program-output "dieroller.rkt" "5"))
     (check-regexp-match #rx"try: dieroller 3d6" (program-output "dieroller.rkt" "3" "6"))
     (check-regexp-match #rx"try: dieroller 3d6\\+3" (program-output "dieroller.rkt" "3" "6" "+3"))
     (check-regexp-match #rx"try: dieroller 3d6k2\\+6"
                         (program-output "dieroller.rkt" "3" "6" "+6" "2")))

   (test-case "an unquoted spaced expression suggests quoting"
     (check-regexp-match #rx"quote the whole expression"
                         (program-output "dieroller.rkt" "2d6" "+" "1d8")))

   (test-case "an empty command line asks for an expression"
     (check-regexp-match #rx"no dice expression given\\."
                         (program-output "dieroller.rkt")))

   (test-case "pathfinder methods all run"
     (for ([args '(("-c") ("-s") ("-r") ("-l" "3/3/4/6/4/4") ("-p" "epic"))])
       (check-regexp-match #rx"[0-9]"
                           (apply program-output "pathfinder-character.rkt" (append args '("-n" "2"))))))

   (test-case "pathfinder verbose prints no stray list line"
     (define out (program-output "pathfinder-character.rkt" "-c" "-v" "-n" "3"))
     (check-equal? (length (string-split out "\n")) 3)
     (check-false (regexp-match? #rx"'\\(\\(" out)))

   (test-case "pathfinder plain output is one character per line"
     (define lines (string-split (program-output "pathfinder-character.rkt" "-s" "-n" "4") "\n"))
     (check-equal? (length lines) 4)
     (for ([l lines])
       (check-equal? (length (string-split l " ")) 6)))

   (test-case "purchase characters are not always in descending order"
     (define lines (string-split (program-output "pathfinder-character.rkt" "-p" "standard" "-n" "200") "\n"))
     (define descending
       (for/sum ([l lines])
         (define scores (map string->number (string-split l " ")))
         (if (equal? scores (sort scores >)) 1 0)))
     (check-true (< descending 40) (format "~a of 200 were descending" descending)))

   (test-case "purchase characters spend exactly the campaign budget"
     (for ([pair '(("low" 10) ("standard" 15) ("high" 20) ("epic" 25))])
       (define lines (string-split (program-output "pathfinder-character.rkt"
                                                   "-p" (first pair) "-n" "20") "\n"))
       (for ([l lines])
         (check-equal? (abilities-cost (map string->number (string-split l " ")))
                       (second pair)))))

   (test-case "an unknown purchase type is rejected"
     (check-not-equal? (program-exit-code "pathfinder-character.rkt" "-p" "banana") 0)
     (check-regexp-match #rx"purchase type must be one of"
                         (program-output "pathfinder-character.rkt" "-p" "banana")))

   (test-case "pool validation messages are unchanged"
     (check-regexp-match #rx"dice per attribute must specify die quantity for six attributes\\."
                         (program-output "pathfinder-character.rkt" "--pool" "3/3/3/3/3"))
     (check-regexp-match #rx"a minimum of 3 dice must be used for each attribute\\."
                         (program-output "pathfinder-character.rkt" "--pool" "2/3/3/3/3/10"))
     (check-regexp-match #rx"you must specify a total of twenty-four dice for the pool\\."
                         (program-output "pathfinder-character.rkt" "--pool" "3/3/3/3/3/3")))))

(module+ test
  (define failures
    (+ (run-tests notation-tests)
       (run-tests aggregate-tests)
       (run-tests roll-tests)
       (run-tests ability-tests)
       (run-tests combination-tests)
       (run-tests purchase-tests)
       (run-tests reorder-tests)
       (run-tests cli-tests)))
  (when (> failures 0)
    (error 'tests "~a test(s) failed" failures)))
