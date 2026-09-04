#lang racket

;; Argument reordering for command-line.
;;
;; Racket's `command-line` stops parsing flags at the first non-flag argument,
;; so `dieroller 5 -v` read "-v" as the <sides> argument and died with a
;; contract violation. Moving flags ahead of positionals before parsing lets
;; options appear anywhere on the line.

(provide reorder-args die)

;; Returns #t for a token that starts an option rather than a value. A negative
;; number such as "-1" is a modifier, not a flag, so it is excluded.
;;
;; Testing that with string->number is not enough: Racket reads "-i" and "+i" as
;; the imaginary unit, so `-i` (the short form of --iterations) looked like a
;; number and was mistaken for a positional argument. Only a "-" followed by a
;; digit is a value.
(define (flag? token)
  (and (> (string-length token) 1)
       (char=? (string-ref token 0) #\-)
       (not (regexp-match? #px"^-[0-9]" token))))

;; flags-taking-values : the options that consume the token after them, so that
;; value is kept with its flag rather than treated as a positional.
(define (reorder-args args flags-taking-values)
  (let loop ([tokens (if (vector? args) (vector->list args) args)]
             [flags '()]
             [positionals '()])
    (cond
      [(null? tokens)
       (append (reverse flags) (reverse positionals))]
      ;; Everything after "--" is positional by definition.
      [(equal? (car tokens) "--")
       (append (reverse flags) (reverse positionals) (cdr tokens))]
      [(flag? (car tokens))
       (if (and (member (car tokens) flags-taking-values) (pair? (cdr tokens)))
           (loop (cddr tokens)
                 (cons (cadr tokens) (cons (car tokens) flags))
                 positionals)
           (loop (cdr tokens) (cons (car tokens) flags) positionals))]
      [else
       (loop (cdr tokens) flags (cons (car tokens) positionals))])))

;; Reports a usage failure the way a command line tool should: on stderr, with a
;; non-zero exit status, so a shell pipeline can tell that it failed. These
;; messages previously went to stdout and exited 0.
(define (die hint . messages)
  (for ([m (in-list messages)]) (displayln m (current-error-port)))
  (displayln hint (current-error-port))
  (exit 1))
