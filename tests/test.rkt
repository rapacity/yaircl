#lang racket


(require "../client.rkt" "../dispatch.rkt")

(current-irc-connection (irc-connect "localhost" #:reconnect? #t))




(irc-register-handler!
 (dispatch-rule (from type args)
  (printf "~a | ~a | ~a~%" from type args)))


(irc-register-handler! 
 (dispatch-rules
  [(_ 'welcome _)
   (irc-join "#eof")
   (irc-say "#eof" "uguu")   
   (irc-action "#eof" "toaster")]
  [((list "rapacity" _ _) 'privmsg (list channel "hello"))
   (irc-say "#eof" "it's you!")]))
