#lang racket

;; Dice notation.
;;
;;   expression := term (('+' | '-') term)* ('*' integer)?
;;   term       := dice | integer
;;   dice       := integer 'd' integer selector?
;;   selector   := 'k' ('h' | 'l')? integer     -- keep, defaulting to highest
;;               | 'd' ('h' | 'l')  integer     -- drop
;;
;; so 4d6k3, 2d20kl1 (roll with disadvantage), 4d6dl1 (drop the lowest) and
;; 2d6+1d8-1 are all accepted. Drop always needs its direction letter, because a
;; bare d is already the dice separator.
;;
;; Dropping is stored as keeping from the opposite end, so 4d6dl1 and 4d6k3
;; produce the same spec and both render as 4D6K3.

(provide (struct-out spec)
         (struct-out expr)
         make-spec
         legacy->expr
         parse-dice-notation
         dice-notation?
         dice-like?
         spec->string
         expr->string
         roll-spec
         roll-expr)

;; from is 'high or 'low: which end of the sorted roll the kept dice come from.
(struct spec (dice sides keep from) #:transparent)

;; terms is a list of (sign . value) where sign is '+ or '- and value is a spec
;; or an integer. scale is a trailing *n, or #f.
(struct expr (terms scale) #:transparent)

(define TOKEN-RX
  #px"^([-+*])?([0-9]+[dD][0-9]+(?:[kK][hHlL]?[0-9]+|[dD][hHlL][0-9]+)?|[0-9]+)")

(define DICE-RX
  #px"^([0-9]+)[dD]([0-9]+)(?:([kK])([hHlL]?)([0-9]+)|([dD])([hHlL])([0-9]+))?$")

;; Returns (values spec #f) or (values #f message). Keeping every die is the
;; same from either end, so that case is canonicalised to 'high to keep the
;; rendered notation stable.
(define (make-spec dice sides keep from)
  (let* ([k (or keep dice)]
         [f (if (= k dice) 'high from)])
    (cond
      [(< dice 1) (values #f "dice must be greater than 0.")]
      [(< k 1) (values #f "keep must be greater than 0.")]
      [(< dice k) (values #f "dice must be greater than or equal to keep.")]
      [(< sides 1) (values #f "sides must be greater than 0.")]
      [else (values (spec dice sides k f) #f)])))

;; Builds an expression from the legacy positional form, preserving the way the
;; modifier used to apply to the sum of the kept dice.
(define (legacy->expr dice sides keep op amt)
  (define-values (s err) (make-spec dice sides keep 'high))
  (cond
    [err (values #f err)]
    [(eq? op '*) (values (expr (list (cons '+ s)) amt) #f)]
    [(and (eq? op '+) (= amt 0)) (values (expr (list (cons '+ s)) #f) #f)]
    [else (values (expr (list (cons '+ s) (cons op amt)) #f) #f)]))

(define (strip str) (regexp-replace* #px"\\s+" str ""))

(define (direction letter default)
  (cond [(member letter '("l" "L")) 'low]
        [(member letter '("h" "H")) 'high]
        [else default]))

(define (opposite d) (if (eq? d 'low) 'high 'low))

(define (parse-term body)
  (define m (regexp-match DICE-RX body))
  (if (not m)
      (values (string->number body) #f)
      (let ([dice (string->number (list-ref m 1))]
            [sides (string->number (list-ref m 2))]
            [kdir (list-ref m 4)]
            [kn (list-ref m 5)]
            [ddir (list-ref m 7)]
            [dn (list-ref m 8)])
        (cond
          [kn (make-spec dice sides (string->number kn) (direction kdir 'high))]
          ;; Drop is keeping from the opposite end: drop the lowest 1 of 4d6 is
          ;; keep the highest 3, drop the highest 1 is keep the lowest 3.
          [dn (make-spec dice sides
                         (- dice (string->number dn))
                         (opposite (direction ddir 'low)))]
          [else (make-spec dice sides dice 'high)]))))

(define (tokenize str original)
  (let loop ([s str] [acc '()])
    (cond
      [(string=? s "")
       (if (null? acc)
           (values #f "no dice expression given.")
           (values (reverse acc) #f))]
      [else
       (define m (regexp-match TOKEN-RX s))
       (if (not m)
           (values #f (format "could not parse dice notation: \"~a\"" original))
           (let ([whole (list-ref m 0)]
                 [sign (list-ref m 1)]
                 [body (list-ref m 2)])
             (loop (substring s (string-length whole))
                   (cons (cons (or sign "+") body) acc))))])))

;; Returns (values expr #f) or (values #f message).
(define (parse-dice-notation string)
  (define original (string-trim string))
  (define-values (tokens err) (tokenize (strip string) original))
  (if err
      (values #f err)
      (let loop ([ts tokens] [terms '()] [scale #f])
        (cond
          [(null? ts) (values (expr (reverse terms) scale) #f)]
          ;; A scale must be the final token, so anything after one is an error.
          [scale (values #f (format "a * multiplier must come last: \"~a\"" original))]
          [else
           (define sign (car (car ts)))
           (define body (cdr (car ts)))
           (cond
             [(string=? sign "*")
              (let ([n (string->number body)])
                (if (exact-integer? n)
                    (loop (cdr ts) terms n)
                    (values #f
                            (format "a * multiplier must be a whole number: \"~a\"" original))))]
             [else
              (define-values (value term-err) (parse-term body))
              (if term-err
                  (values #f term-err)
                  (loop (cdr ts)
                        (cons (cons (if (string=? sign "-") '- '+) value) terms)
                        scale))])]))))

(define (dice-notation? string)
  (and (dice-like? string)
       (let-values ([(e err) (parse-dice-notation string)]) (and e #t))))

;; True when the string is *shaped* like a dice expression, whether or not it
;; actually parses. The CLI dispatches on this rather than dice-notation? so a
;; malformed expression reports why it is malformed, instead of falling through
;; to the legacy positional form and complaining that "2d6k5" is not a number.
(define (dice-like? string)
  (regexp-match? #px"[0-9][dD][0-9]" (strip string)))

(define (spec->string s)
  (string-append
   (number->string (spec-dice s))
   "D"
   (number->string (spec-sides s))
   (cond [(= (spec-keep s) (spec-dice s)) ""]
         [(eq? (spec-from s) 'high) (string-append "K" (number->string (spec-keep s)))]
         [else (string-append "KL" (number->string (spec-keep s)))])))

(define (expr->string e)
  (string-append
   (apply string-append
          (for/list ([t (in-list (expr-terms e))] [i (in-naturals)])
            (let* ([sign (car t)]
                   [value (cdr t)]
                   [text (if (spec? value) (spec->string value) (number->string value))])
              ;; A leading plus is implicit; every later term carries its sign.
              (if (and (zero? i) (eq? sign '+))
                  text
                  (string-append (if (eq? sign '+) "+" "-") text)))))
   (if (expr-scale e) (string-append "*" (number->string (expr-scale e))) "")))

;; Returns (values rolled kept sum).
(define (roll-spec s)
  (define rolled (build-list (spec-dice s) (lambda (i) (+ 1 (random (spec-sides s))))))
  (define ordered (if (eq? (spec-from s) 'high) (sort rolled >) (sort rolled <)))
  (define kept (take ordered (spec-keep s)))
  (values rolled kept (apply + kept)))

;; Returns (values list-of-kept-lists total), one kept list per dice term.
(define (roll-expr e)
  (define-values (groups sum)
    (for/fold ([groups '()] [sum 0]) ([t (in-list (expr-terms e))])
      (let ([sign (car t)] [value (cdr t)])
        (if (spec? value)
            (let-values ([(rolled kept s) (roll-spec value)])
              (values (cons kept groups) (if (eq? sign '+) (+ sum s) (- sum s))))
            (values groups (if (eq? sign '+) (+ sum value) (- sum value)))))))
  (values (reverse groups)
          (if (expr-scale e) (* sum (expr-scale e)) sum)))
