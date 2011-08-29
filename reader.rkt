#lang racket/base

(require racket/match racket/date "codes.rkt")

(struct irc-server (name) #:transparent)
(struct irc-client (nick user host) #:transparent)
(struct irc-message (timestamp sender command arguments) #:transparent)

(define (parse-prefix str)
  (match str
    [(regexp #rx"^([^!]+)!([^@]+)@(.+)$" (list _ nick user host))
     (irc-client nick user host)]
    [else (irc-server str)]))

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
      (parse-arguments args))]))

(define (irc-read port)
  (let ([msg (read-line port 'any)])
    (if (eof-object? msg) msg
        (parse-message msg))))

(provide (except-out (all-defined-out) parse-prefix parse-command parse-arguments parse-message)
         )