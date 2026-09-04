#lang racket

(require "cli-args.rkt"
         "notation.rkt")

;; Options that consume the following token, so reorder-args keeps each flag
;; together with its value.
(define VALUE-FLAGS
  '("-d" "--dice" "-k" "--keep" "-m" "--modifier"
    "-s" "--sides" "-i" "--iterations"))

(define DIEROLLERHELP "Try 'dieroller --help' for more information.")

;; Parses a standalone modifier argument: a leading +, -, or * selects the
;; operation, and without one + is assumed. Returns (values op amount) or
;; (values #f #f) when it is not a number.
(define (parse-modifier modifier)
  (let* ([s (string-trim modifier)])
    (if (string=? s "")
        (values #f #f)
        (let* ([head (substring s 0 1)]
               [signed? (member head '("+" "-" "*"))]
               [digits (if signed? (string-trim (substring s 1)) s)]
               [amount (string->number digits)])
          (if (exact-integer? amount)
              (values (cond [(string=? head "-") '-]
                            [(string=? head "*") '*]
                            [else '+])
                      amount)
              (values #f #f))))))

(define number-to-roll (make-parameter 1))
(define sides-per-die (make-parameter 20))
;; #f distinguishes "--keep not supplied" from an explicit "--keep 0", which the
;; help text always said was invalid.
(define number-to-keep (make-parameter #f))
(define modifier-to-rolls (make-parameter "0"))
(define verbose-is-on (make-parameter false))
(define iterations-to-roll (make-parameter 1))

;; Reads the nth positional argument as an integer, falling back to the flag.
(define (positional args n fallback name)
  (if (< (length args) (add1 n))
      (values fallback #f)
      (let ([v (string->number (list-ref args n))])
        (if (exact-integer? v)
            (values v #f)
            (values #f (format "~a must be a number, got \"~a\"." name (list-ref args n)))))))

;; Builds an expression from the legacy positional form, merging flags and
;; positionals slot by slot the way the original did, with positionals winning.
(define (legacy-expression args)
  (define-values (dice dice-err) (positional args 0 (number-to-roll) "dice"))
  (define-values (sides sides-err) (positional args 1 (sides-per-die) "sides"))
  (define-values (keep keep-err)
    (positional args 3 (or (number-to-keep) (or dice 1)) "keep"))
  (define modifier-text
    (if (< (length args) 3) (modifier-to-rolls) (list-ref args 2)))
  (define-values (op amount) (parse-modifier modifier-text))
  (cond
    [dice-err (values #f dice-err)]
    [sides-err (values #f sides-err)]
    [keep-err (values #f keep-err)]
    [(not op) (values #f (format "could not parse modifier: \"~a\"" modifier-text))]
    [else (legacy->expr dice sides keep op amount)]))

;; A dice-rolling command-line utility
(command-line
 #:argv (reorder-args (current-command-line-arguments) VALUE-FLAGS)
 #:usage-help
 ""
 "where the <arguments> are"
 ""
 "  <notation>"
 "or"
 "  <dice>"
 "or"
 "  <dice> <sides>"
 "or"
 "  <dice> <sides> <modifier>"
 "or"
 "  <dice> <sides> <modifier> <keep>"
 ""
 "<notation> is standard dice notation:"
 ""
 "  <dice>d<sides>[<selector>]  combined with + and -, optionally scaled by *n"
 ""
 "  <selector> is one of"
 "    k<n>, kh<n>  keep the highest <n> dice (k defaults to highest)"
 "    kl<n>        keep the lowest <n> dice   (roll with disadvantage)"
 "    dl<n>        drop the lowest <n> dice"
 "    dh<n>        drop the highest <n> dice"
 ""
 "See the --dice, --sides, and --modifier parameters for details."
 ""
 "Examples:"
 ""
 "  dieroller 5"
 "  dieroller 1 10"
 "  dieroller 3 6 +3"
 "  dieroller 3 6 +6 2"
 "  dieroller 4d6k3"
 "  dieroller 2d20kl1"
 "  dieroller 4d6dl1"
 "  dieroller 2d6+1d8-1"
 "  dieroller --dice 5 --sides 100 --modifier +4 --keep 3"
 "  dieroller --dice 4 --sides 6 --keep 3"
 ""
 #:once-each
 [("-v" "--verbose") ("Display additional information (default to false).")
                     (verbose-is-on true)]
 [("-d" "--dice") dice ("Number of dice to roll.  Must be greater than 0."
                        "(default to 1)")
                  (number-to-roll (string->number dice))]
 [("-k" "--keep") keep
                  ("Number of rolls to keep. Must be greater than 0 and less than or equal to <dice>."
                   "(default to number of dice)")
                  (number-to-keep (string->number keep))]
 [("-m" "--modifier") modifier
                      ("Modifier to the rolls. The first character can optionally"
                       "be one of +, -, or * followed by a number.  If the +, -, or"
                       "* are missing, + is assumed. (default to no modifier)")
                      (modifier-to-rolls modifier)]
 [("-s" "--sides") sides
                   ("Number of sides per die. Must be greater than 0."
                    "(default to 20)")
                   (sides-per-die (string->number sides))]
 [("-i" "--iterations") iterations ("Number of times to repeat the same rolls.  Must be greater than 0."
                                    "(default to 1)")
                        (iterations-to-roll (string->number iterations))]
 #:args arguments

 (let*-values
     ([(reps) (iterations-to-roll)]
      [(verbose) (verbose-is-on)]
      ;; A leading positional in dice notation short-circuits the legacy form.
      [(e err)
       (cond
         [(and (pair? arguments) (dice-like? (first arguments)))
          (if (pair? (rest arguments))
              (values #f (format "unexpected arguments after dice notation: ~a"
                                 (string-join (rest arguments) " ")))
              (parse-dice-notation (first arguments)))]
         [else (legacy-expression arguments)])])
   (cond
     [(< reps 1) (die DIEROLLERHELP "iterations must be greater than 0.")]
     [err (die DIEROLLERHELP err)]
     [else
      (let ([dicetype (expr->string e)])
        ;; Printed as each roll is made, so a large --iterations streams rather
        ;; than buffering.
        (for ([i (in-range reps)])
          (let-values ([(groups total) (roll-expr e)])
            (when verbose
              (display dicetype)
              (display " ")
              ;; One parenthesised group per dice term, so a single-group
              ;; expression reads exactly as it always has.
              (for ([kept (in-list groups)])
                (display "(")
                (display (string-join (map number->string kept) " "))
                (display ") "))
              (display "=> "))
            (displayln total))))])))
