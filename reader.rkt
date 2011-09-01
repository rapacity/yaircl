#lang racket/base

(require racket/match racket/date "codes.rkt")

(struct irc-message (timestamp sender command arguments) #:transparent)

(define (parse-prefix str)
  (match str
    [(regexp #rx"^([^!]+)!([^@]+)@(.+)$" (list _ nick user host))
     (list nick user host)]
    [else (list str)]))

(define (parse-command str)
  (let ([sym (string->symbol (string-downcase str))])
    (if (numeric-reply? str)
        (numeric-reply-ref sym)
        sym)))

(define (parse-arguments str)
  (match str
    [#f null]
    [(regexp #rx"^([^:]+)(?: )+:(.+)$" (list _ head tail))
     (append (regexp-split #rx" +" head) (list tail))]
    [(regexp #rx"^(?: +)?:(.+)" (list _ tail))
     (list tail)]
    [else (regexp-split #rx" +" str)]))

(define (parse-message str)
  (match str
    [(regexp #rx"^(?::([^ ]+) )?([^ ]+) ?(.+)?$" (list _ prefix command args))
     (irc-message 
      (current-date)
      (if prefix (parse-prefix prefix) prefix)
      (parse-command command)
      (parse-arguments args))]
    [else (error (format "reader error '~a'" str))]))

(require srfi/13)

(define (irc-read port)
  (let ([msg (read-line port 'any)])
    (if (eof-object? msg) msg
        (if (string=? "" (string-trim msg)) (irc-read port)
            (parse-message msg)))))
 
(provide (except-out (all-defined-out) parse-prefix parse-command parse-arguments parse-message)
         )