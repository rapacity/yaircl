#lang racket


(require "../main.rkt")

(current-irc-connection (irc-connect "irc.freenode.net" #:reconnect? #t))


(irc-register-handler! 
 (match-lambda
   [(irc-message _ 'rpl-welcome _)
    (irc-join "##test123")
    (irc-say "##test123" "uguu")]
   [else (void)]))


(irc-register-handler!
  (match-lambda
    [(irc-message (irc-client nick _ _) 'privmsg (list channel message))
     (printf "~a@~a> ~a~%" nick channel message)]
    [else (void)]))



