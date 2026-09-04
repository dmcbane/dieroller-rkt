#lang racket

(require "cli-args.rkt"
         "notation.rkt"
         "version.rkt")

(define DIEROLLERHELP "Try 'dieroller --help' for more information.")

;; No option takes a value any more, so reorder-args only has to lift flags out
;; from among the positional arguments.
(define VALUE-FLAGS '())

;; The <dice> <sides> <modifier> <keep> arguments the notation replaced.
(define LEGACY-RX #px"^[-+*]?[0-9]+$")

(define (legacy-form? args)
  (and (pair? args)
       (<= (length args) 4)
       (andmap (lambda (a) (regexp-match? LEGACY-RX a)) args)))

;; Renders the old positional arguments as the equivalent expression.
(define (modifier-text m)
  (let* ([s (string-trim m)]
         [head (substring s 0 1)]
         [signed? (member head '("+" "-" "*"))]
         [digits (if signed? (substring s 1) s)]
         [amount (string->number digits)]
         [op (cond [(string=? head "-") "-"] [(string=? head "*") "*"] [else "+"])])
    (if (and (equal? op "+") (equal? amount 0))
        ""
        (string-append op (number->string amount)))))

(define (suggestion args)
  (case (length args)
    [(1) (string-append (first args) "d20")]
    [(2) (string-append (first args) "d" (second args))]
    [(3) (string-append (first args) "d" (second args) (modifier-text (third args)))]
    [else (string-append (first args) "d" (second args)
                         "k" (fourth args)
                         (modifier-text (third args)))]))

(define (legacy-hint args)
  (string-append
   "the <dice> <sides> <modifier> <keep> arguments have been replaced by dice"
   " notation; try: dieroller " (suggestion args)))

(define (quoting-hint args)
  (format "a roll is one argument; quote the whole expression, for example: dieroller \"~a\""
          (string-join args " ")))

;; Returns (values batch #f) or (values #f message).
(define (roll-from args)
  (cond
    [(null? args) (values #f "no dice expression given.")]
    ;; Caught before parsing so the old positional form gets a migration message
    ;; rather than "expression contains no dice".
    [(legacy-form? args) (values #f (legacy-hint args))]
    [(null? (rest args)) (parse-roll (first args))]
    [else (values #f (quoting-hint args))]))

;; One parenthesised group per dice term, so a single-group expression reads
;; exactly as it always has: "4D6K3 (5 4 4) => 13".
(define (show-roll dicetype groups total)
  (display dicetype)
  (display " ")
  (for ([kept (in-list groups)])
    (display "(")
    (display (string-join (map number->string kept) " "))
    (display ") "))
  (display "=> ")
  (displayln total))

(define verbose-is-on (make-parameter false))

;; A dice-rolling command-line utility
(command-line
 #:argv (reorder-args (current-command-line-arguments) VALUE-FLAGS)
 #:usage-help
 ""
 "A roll is written entirely in dice notation, as a single argument:"
 ""
 "  <roll>       := <repeated> | <aggregate>(<repeated>) | <aggregate>:<repeated>"
 "  <repeated>   := [<repeat>x] <expression>"
 "  <expression> := <term> (('+' | '-') <term>)* ['*' <integer>]"
 "  <term>       := <dice> | <integer>"
 "  <dice>       := <count>d<sides>[<selector>]"
 ""
 "  <selector> is one of"
 "    k<n>, kh<n>  keep the highest <n> dice (k defaults to highest)"
 "    kl<n>        keep the lowest <n> dice   (roll with disadvantage)"
 "    dl<n>        drop the lowest <n> dice"
 "    dh<n>        drop the highest <n> dice"
 ""
 "  <aggregate> reduces a repeated roll to a single number, and is one of"
 "    sum, total          every roll added together"
 "    avg, average, mean  their average, to two places"
 "    high, highest, max  the best of them"
 "    low, lowest, min    the worst of them"
 "    median, med         the middle one"
 ""
 "A modifier applies to the sum of the kept dice, not to each die, so 3d6*2"
 "doubles the total rather than rolling 3d12. Quote the expression if you write"
 "it with spaces. Most shells eat unquoted parentheses, so either quote the"
 "whole roll or use the colon form, which means exactly the same thing."
 ""
 "Examples:"
 ""
 "  dieroller 1d20                 a single twenty-sided die"
 "  dieroller 5d20                 five of them"
 "  dieroller 3d6+3                three six-sided dice, plus three"
 "  dieroller 4d6k3                keep the best three of four"
 "  dieroller 2d20kh1              advantage"
 "  dieroller 2d20kl1              disadvantage"
 "  dieroller 4d6dl1               drop the lowest of four"
 "  dieroller 2d6+1d8-1            several dice groups and a constant"
 "  dieroller 3d6*2                double the total"
 "  dieroller 6x4d6k3              roll the same thing six times"
 "  dieroller 6x4d6k3 --verbose    show the dice that were kept"
 "  dieroller \"2d6 + 1d8\"          spaces are fine when quoted"
 "  dieroller \"sum(6x4d6k3)\"       add those six rolls up"
 "  dieroller sum:6x4d6k3          the same, with nothing for a shell to eat"
 "  dieroller avg:100x1d20         the average of a hundred rolls"
 "  dieroller max:2x1d20           the better of two rolls"
 "  dieroller \"sum(6x4d6k3)\" -v    show each roll, then the sum"
 ""
 #:once-each
 [("-v" "--verbose") ("Show the notation and the dice that were kept.")
                     (verbose-is-on true)]
 [("-V" "--version") ("Show the version")
                     (begin (displayln (string-append "dieroller " TOOLS-VERSION))
                            (exit 0))]
 #:args arguments

 (let-values ([(b err) (roll-from arguments)])
   (if err
       (die DIEROLLERHELP err)
       (let* ([e (batch-expr b)]
              [kind (batch-aggregate b)]
              [dicetype (expr->string e)]
              [verbose (verbose-is-on)])
         ;; Each roll is printed as it is made, so a large repeat count streams
         ;; rather than buffering. An aggregate is the one thing that cannot
         ;; stream all the way -- its answer depends on every roll -- so its
         ;; totals are collected and the summary follows them. Verbose still
         ;; shows the rolls behind an aggregate as they happen.
         (define totals
           (for/fold ([acc '()] #:result (reverse acc))
                     ([i (in-range (batch-repeat b))])
             (define-values (groups total) (roll-expr e))
             (cond
               [verbose (show-roll dicetype groups total)]
               [(not kind) (displayln total)])
             (if kind (cons total acc) acc)))
         (when kind
           (when verbose
             (display (batch->string b))
             (display " => "))
           (displayln (aggregate->string kind totals)))))))
