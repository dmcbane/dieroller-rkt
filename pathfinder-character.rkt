#lang racket

(require "abilities.rkt"
         "cli-args.rkt"
         "version.rkt")

;; Options that consume the following token, so reorder-args keeps each flag
;; together with its value.
(define VALUE-FLAGS '("-l" "--pool" "-p" "--purchase" "-n" "--number"))

(define PATHFINDERCHARHELP "Try 'pathfinder-character --help' for more information.")

(define (parse-dice-per-ability string)
  (map string->number (string-split string #px"[,/:]")))

;; total ability purchase points available by campaign type
(define (campaign-type->total-purchase-points n)
  (case n
    [(low) 10]
    [(standard) 15]
    [(high) 20]
    [(epic) 25]
    [else 0]))

;; Returns #f for an unrecognised type. The original fell through to 'low, so a
;; typo silently produced a 10 point character instead of reporting the mistake.
(define (parse-purchase-type string)
  (let ([s (string-upcase (string-trim string))])
    (cond [(regexp-match? #rx"^E" s) 'epic]
          [(regexp-match? #rx"^H" s) 'high]
          [(regexp-match? #rx"^S" s) 'standard]
          [(regexp-match? #rx"^L" s) 'low]
          [else #f])))

(define (attribute-generator dice keep sides modamt)
  (let* ([myrand (lambda (x) (+ 1 (random sides)))])
    (lambda ()
      (let* ([rands (build-list dice myrand)]
             [maxkeep (take (sort rands >) keep)]
             [sum (apply + maxkeep)]
             [adjusted (+ sum modamt)])
        adjusted))))

;; Picks a random spread costing exactly the points available.
;;
;; Spreads are stored sorted descending so that they deduplicate as multisets.
;; Handed to a character in that order STR would always be the highest ability,
;; so the assignment is shuffled here, where a spread becomes a character.
(define (purchase-generator points-available)
  (define candidates (hash-ref (legal-sets-by-cost) points-available '()))
  (when (null? candidates)
    (die PATHFINDERCHARHELP
         (format "no legal ability spread costs exactly ~a points." points-available)))
  (lambda ()
    (shuffle (list-ref candidates (random (length candidates))))))

(define (method->dice m)
  (cond [(equal? m 'heroic) 2]
        [(equal? m 'classic) 3]
        [else 4]))

(define (method->keep m)
  (cond [(equal? m 'heroic) 2]
        [else 3]))

(define (method->adjustment-amount m)
  (cond [(equal? m 'heroic) 6]
        [else 0]))

(define generation-method (make-parameter 'standard))
(define verbose-is-on (make-parameter false))
(define number-to-roll (make-parameter 1))
(define dice-per-ability (make-parameter (parse-dice-per-ability "4/4/4/4/4/4")))
;; The raw string, validated in the body so a bad value is reported with the
;; other usage errors rather than silently defaulting to low.
(define purchase-type-string (make-parameter "standard"))


;; A pathfinder character generation command-line utility
(command-line
 #:argv (reorder-args (current-command-line-arguments) VALUE-FLAGS)

 #:usage-help
 ""
 "Examples:"
 ""
 "  pathfinder-character --classic -v --number 10"
 "  pathfinder-character -s -n 3"
 ""

 #:once-any
 [("-c" "--classic") ("The classic method: 3D6 per ability.")
                     (generation-method 'classic)]
 [("-s" "--standard") ("The standard method: 4D6 keep high 3 per ability."
                       "(this is the default)")
                      (generation-method 'standard)]
 [("-r" "--heroic") ("The heroic method: 2D6 plus 6 per ability.")
                    (generation-method 'heroic)]
 [("-l" "--pool") diceperability ("The pool method: 24D6 for all 6 abilities. The parameter"
                                  "specifies how many dice are assigned to each ability as"
                                  "follows: 3/3/3/3/3/9 with a minimum of 3 dice per ability.")
                  (begin
                    (generation-method 'pool)
                    (dice-per-ability (parse-dice-per-ability diceperability)))]
 [("-p" "--purchase") purchasetype ("The purchase method: parameters are set according to cost."
                                    "The parameter specifies the purchase type as follows: low,"
                                    "standard, high, and epic fantasy which provides 10, 15, 20,"
                                    "and 25 purchase points respectively.")
                      (begin
                        (generation-method 'purchase)
                        (purchase-type-string purchasetype))]

 #:once-each
 [("-v" "--verbose") ("Display additional information (default to false).")
                     (verbose-is-on true)]
 [("-V" "--version") ("Show the version")
                     (begin (displayln (string-append "pathfinder-character " TOOLS-VERSION))
                            (exit 0))]
 [("-n" "--number") n ("Number of characters to roll. Must be greater than 0."
                       "(default to 1)")
                    (number-to-roll (string->number n))]

 #:args arguments

 (let* ([pool-dist (dice-per-ability)]
        [dice (method->dice (generation-method))]
        [keep (method->keep (generation-method))]
        [sides 6]
        [numabils 6]
        [amt (method->adjustment-amount (generation-method))]
        [verbose (verbose-is-on)]
        [purchasing (equal? (generation-method) 'purchase)]
        [campaign (parse-purchase-type (purchase-type-string))]
        [characters (number-to-roll)])
   (cond [(< characters 1)
          (die PATHFINDERCHARHELP "number of characters must be greater than 0.")]
         [(and purchasing (not campaign))
          (die PATHFINDERCHARHELP "purchase type must be one of low, standard, high, or epic.")]
         [(not (equal? (length pool-dist) 6))
          (die PATHFINDERCHARHELP
               "dice per attribute must specify die quantity for six attributes.")]
         [(ormap (lambda (x) (< x 3)) pool-dist)
          (die PATHFINDERCHARHELP "a minimum of 3 dice must be used for each attribute.")]
         [(not (equal? (for/sum ([x pool-dist]) x) 24))
          (die PATHFINDERCHARHELP "you must specify a total of twenty-four dice for the pool.")]
         [else
          (let* ([ability-gen
                  (cond [(equal? (generation-method) 'pool)
                         (map (lambda (cnt) (attribute-generator cnt keep sides amt)) pool-dist)]
                        [purchasing 'nil]
                        [else
                         (map (lambda (x) (attribute-generator dice keep sides amt))
                              (stream->list (in-range numabils)))])]
                 [abilities
                  (cond [purchasing
                         (purchase-generator (campaign-type->total-purchase-points campaign))]
                        [else (lambda () (map (lambda (x) (x)) ability-gen))])]
                 [with-ratings
                  (lambda (x)
                    (let* ([a (abilities)]) (list a (bonus-points-of-abilities a))))]
                 [all-characters
                  (sort (map with-ratings (stream->list (in-range characters)))
                        (lambda (x y) (< (last x) (last y))))])
            ;; for-each rather than map: the map's return value became the value
            ;; of the whole module-level expression, so Racket printed a stray
            ;; '((7 9 9 4 10 15) ...) line after the report.
            (if verbose
                (for-each
                 (lambda (char-with-rating)
                   (let ([attrs (first char-with-rating)]
                         [rating (second char-with-rating)])
                     (let ([str (first attrs)]
                           [dex (second attrs)]
                           [con (third attrs)]
                           [int (fourth attrs)]
                           [wis (fifth attrs)]
                           [chr (sixth attrs)])
                       (display "STR: ")
                       (display str)
                       (display " DEX: ")
                       (display dex)
                       (display " CON: ")
                       (display con)
                       (display " INT: ")
                       (display int)
                       (display " WIS: ")
                       (display wis)
                       (display " CHR: ")
                       (display chr)
                       (display " (")
                       (display rating)
                       (displayln ")"))))
                 all-characters)
                ;; One character per line rather than one raw list-of-lists.
                (for-each
                 (lambda (char-with-rating)
                   (displayln (string-join (map number->string (first char-with-rating)) " ")))
                 all-characters)))])))
