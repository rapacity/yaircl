#lang racket

(require "../server.rkt")

(define port (+ 10000 (random 10000)))

(define s (irc-listen port))


(require "../client.rkt" "../dispatch.rkt")

(define c (irc-connect "localhost" port #:reconnect? #t #:reconnect-wait 3))

(define z (irc-connect "localhost" port #:reconnect? #t #:reconnect-wait 3))


(irc-register-handler! #:connection c
 (dispatch-rule (from type args)
  (printf "c> ~a | ~a | ~a~%" from type args)))

(irc-register-handler! #:connection z
 (dispatch-rule (from type args)
  (printf "z> ~a | ~a | ~a~%" from type args)))


(irc-register-handler! #:connection c
 (dispatch-rules
  [(_ 'welcome _)
   (irc-join "#eof")
   (irc-say "#eof" "uguu")   
   (irc-action "#eof" "toaster")]))
  
(irc-register-handler! #:connection z
 (dispatch-rules
  [(_ 'welcome _)
   (irc-join "#eof")
   (irc-say "#eof" "uguu")   
   (irc-action "#eof" "toaster")]))
  