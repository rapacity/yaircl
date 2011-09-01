#lang racket

(require "../client.rkt" "../dispatch.rkt" "../dcc.rkt")


(current-irc-connection (irc-connect "irc.freenode.net" #:reconnect? #t))

