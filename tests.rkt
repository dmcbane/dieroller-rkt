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

   (test-case "dice-like? drives CLI dispatch, dice-notation? reports validity"
     (check-true (dice-like? "2d6k5"))
     (check-false (dice-notation? "2d6k5"))
     (check-false (dice-like? "abc"))
     (check-false (dice-like? "5"))
     (check-true (dice-notation? "2d6+1d8")))))

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

   (test-case "legacy positional forms still work"
     (check-regexp-match #rx"^5D20 \\([0-9 ]+\\) => [0-9]+"
                         (program-output "dieroller.rkt" "-v" "5"))
     (check-regexp-match #rx"^3D6\\+3 "
                         (program-output "dieroller.rkt" "-v" "3" "6" "+3"))
     (check-regexp-match #rx"^3D6K2\\+6 "
                         (program-output "dieroller.rkt" "-v" "3" "6" "+6" "2")))

   (test-case "flags may follow positionals"
     (check-regexp-match #rx"^5D20 " (program-output "dieroller.rkt" "5" "-v")))

   (test-case "notation forms work end to end"
     (check-regexp-match #rx"^4D6K3 " (program-output "dieroller.rkt" "-v" "4d6k3"))
     (check-regexp-match #rx"^2D20KL1 " (program-output "dieroller.rkt" "-v" "2d20kl1"))
     (check-regexp-match #rx"^4D6KL3 " (program-output "dieroller.rkt" "-v" "4d6dh1"))
     (check-regexp-match #rx"^2D6\\+1D8-1 \\([0-9 ]+\\) \\([0-9]+\\) => "
                         (program-output "dieroller.rkt" "-v" "2d6+1d8-1")))

   (test-case "iterations produce one line each"
     (check-equal? (length (string-split (program-output "dieroller.rkt" "1d1" "-i" "5") "\n")) 5))

   (test-case "validation errors exit non-zero"
     (for ([args '(("0") ("3" "6" "+6" "7") ("-i" "0") ("3" "0"))])
       (check-not-equal? (apply program-exit-code "dieroller.rkt" args) 0
                         (format "expected ~a to fail" args))))

   (test-case "validation messages are unchanged"
     (check-regexp-match #rx"dice must be greater than 0\\."
                         (program-output "dieroller.rkt" "0"))
     (check-regexp-match #rx"dice must be greater than or equal to keep\\."
                         (program-output "dieroller.rkt" "3" "6" "+6" "7"))
     (check-regexp-match #rx"keep must be greater than 0\\."
                         (program-output "dieroller.rkt" "--dice" "3" "--sides" "6" "--keep" "0")))

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
       (run-tests roll-tests)
       (run-tests ability-tests)
       (run-tests combination-tests)
       (run-tests purchase-tests)
       (run-tests reorder-tests)
       (run-tests cli-tests)))
  (when (> failures 0)
    (error 'tests "~a test(s) failed" failures)))
