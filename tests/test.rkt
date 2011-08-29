#lang racket


(require "../main.rkt" "../match.rkt")

(current-irc-connection (irc-connect "irc.freenode.net" #:reconnect? #t))




(irc-register-handler! 
 (irc-λ [timestamp from type args]
   (printf "~a | ~a | ~a~%" from type args)))

(irc-register-handler! 
 (irc-λ [_ _ 'welcome _]
   (irc-join "##test123")
   (irc-say "##test123" "uguu")   
   (irc-action "##test123" "toaster")
   ))

(irc-register-handler! 
 (irc-λ [_ (irc-client "rapacity" _ _) 'privmsg (list channel "hello")]
   (irc-say "##test123" "it's you!")))

 