#lang racket

(require "abilities.rkt"
         "cli-args.rkt")

;; Writes ability score cost/bonus tables to CSV.
;;
;;   all_ability_scores [--out DIR] [--legal-only]
;;
;; The original also wrote all_scores.csv, one row per *ordering* of six scores:
;; 45^6 is about 8.3 billion rows, which does not finish in any practical time.
;; Neither cost nor bonus depends on ability order, so every one of those rows
;; duplicates a spread already present in uniq_scores.csv. It is not generated.

(define VALUE-FLAGS '("-o" "--out"))

(define output-dir (make-parameter "."))
(define legal-only (make-parameter false))

(define HEADER "cost,bonus,s1,s2,s3,s4,s5,s6")

;; Streams rows straight to the port so memory stays flat regardless of how many
;; combinations there are; uniq_scores.csv is 15,890,700 rows.
(define (write-table path pool)
  (printf "Writing ~a...\n" path)
  (define start (current-inexact-milliseconds))
  (define written
    (call-with-output-file path #:exists 'truncate
      (lambda (out)
        (displayln HEADER out)
        (let ([count 0])
          (for-each-combination
           pool 6
           (lambda (abils)
             (set! count (add1 count))
             (displayln (string-append (number->string (abilities-cost abils))
                                       ","
                                       (number->string (bonus-points-of-abilities abils))
                                       ","
                                       (string-join (map number->string abils) ","))
                        out)))
          count))))
  (printf "  ~a rows in ~as\n"
          written
          (/ (round (- (current-inexact-milliseconds) start)) 1000.0)))

(command-line
 #:argv (reorder-args (current-command-line-arguments) VALUE-FLAGS)

 #:usage-help
 ""
 "Writes legal_scores.csv (12,376 rows, scores 7-18) and uniq_scores.csv"
 "(15,890,700 rows, scores 1-45)."
 ""

 #:once-each
 [("-o" "--out") dir ("Directory to write into. (default to the current directory)")
                 (output-dir dir)]
 [("--legal-only") ("Write only legal_scores.csv.")
                   (legal-only true)]

 #:args ()
 (begin
   (make-directory* (output-dir))
   (write-table (build-path (output-dir) "legal_scores.csv") (legal-ability-scores))
   (if (legal-only)
       (displayln "Skipped uniq_scores.csv (--legal-only).")
       (write-table (build-path (output-dir) "uniq_scores.csv") (all-ability-scores)))
   (displayln
    (string-append
     "Skipped all_scores.csv: 45^6 is about 8.3 billion rows, one per ordering of\n"
     "six scores. Cost and bonus do not depend on ability order, so every such row\n"
     "duplicates a spread already in uniq_scores.csv."))))
