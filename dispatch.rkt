#lang racket

(require "reader.rkt" (for-syntax syntax/parse))


(define-syntax (dispatch-rule stx)
  (syntax-parse stx
     [(_ (sender command arguments) body ...+)
      #`(match-lambda 
          [(irc-message _ sender command arguments) body ...]
          [else (void)])]
    [(_ (timestamp sender command arguments) body ...+)
      #`(match-lambda 
          [(irc-message timestamp sender command arguments) body ...]
          [else (void)])]))

(begin-for-syntax 
  (define (dispatch-rules-row stx)
    (syntax-parse stx
      [((timestamp sender command arguments) body ...+)
       #`[(irc-message timestamp sender command arguments) body ...]]
      [((sender command arguments) body ...+)
       #`[(irc-message _ sender command arguments) body ...]]
      [(args body ...+)
       #`[args body ...]])))


(define-syntax (dispatch-rules stx)
  (syntax-parse stx #:literals (else)
    [(_ row ... (~optional [else else-body ...]))
     (with-syntax ([(rule ...) (map dispatch-rules-row (syntax-e #`(row ...)))])
       #`(match-lambda
           rule ...
           [else #,@(if (attribute else-body) #`(else-body ...) #`((void)))]))]))

(provide (all-defined-out))