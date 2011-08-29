#lang racket

(require "reader.rkt" (for-syntax syntax/parse))


(define-syntax (irc-λ stx)
  (syntax-parse stx
    [(_ (timestamp sender command arguments) body ...+)
      #`(match-lambda 
          [(irc-message timestamp sender command arguments) body ...]
          [else (void)])]))

(define-syntax (irc-λ* stx)
  (syntax-parse stx
    [(_ [(timestamp sender command arguments) body ...+] ...)
      #`(match-lambda 
          [(irc-message timestamp sender command arguments) body ...] ...
          [else (void)])]))

(provide (all-defined-out))