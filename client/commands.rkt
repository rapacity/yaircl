#lang racket

(require "core.rkt")

#;
(string-append*
 (for/list: : (Listof String) ([line : String (regexp-split #rx"\r|\n" message)])
   (format "PRIVMSG ~a :~a" (string-join target ",") line)))


(define (maybe-multi obj)
  (if (string? obj) obj
      (string-join obj ",")))

(define-irc/out (irc-say target message)
  (format "PRIVMSG ~a :~a" (maybe-multi target) message))

(define-irc/out (irc-action target message)
  (format "PRIVMSG ~a :\001ACTION ~a\001" (maybe-multi target) message))

(define-irc/out (irc-raw message)
  (format "~a" message))

(define-irc/out (irc-notice target message)
  (format "NOTICE ~a :~a" (maybe-multi target) message))

(define-irc/out (irc-join . channels)
  (format "JOIN ~a" (maybe-multi channels)))

(define-irc/out (irc-part channel [message ""])
  (format "PART ~a :~a" (maybe-multi channel) message))

(define-irc/out (irc-oper username password)
  (format "OPER ~a ~a" username password))


(define-irc/out (irc-nick nickname)
  (format "NICK ~a" nickname))

(provide (all-defined-out))