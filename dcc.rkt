#lang racket

(require "core.rkt" "commands.rkt" "private/network.rkt")
(require racket/path)

(define (dcc target . lst)
  (format "PRIVMSG ~a :\001DCC ~a\001~%" (maybe-multi target)
          (string-join (map (curry format "~a") lst) " ")))



;DCC SEND <filename> <ipaddress> <port> <filesize>
(define-irc/out (dcc-send* target filename ip port filesize)
  (dcc target 'SEND filename ip port filesize))


(define (get-ip i) (let-values ([(_ f) (tcp-addresses i)]) f))

(define-irc/out (dcc-send target file
                          #:ip      [ip-string   (get-ip (irc-connection-receiver connection))]
                          #:port    [port-number (+ 49152 (random 16383))]
                          #:timeout [timeout     180])
  (unless (file-exists? file) (error 'dcc-send "file does not exist"))
  (let ([name (file-name-from-path file)]
        [size (file-size file)]
        [ip-network-order (host->network (ip-string->bytes ip-string))]
        [listener (tcp-listen port-number)]
        [file-port (open-input-file file)])
    (thread (thunk (let-values ([(i o) (tcp-accept listener)])
                     (copy-port file-port o))))
    (dcc target 'SEND name ip-network-order port-number size)))

(define (dcc-get ip-address port-number size)
  (define-values (input _) (tcp-connect ip-address port-number))
  (make-limited-input-port input size #t))

(define (dcc-get->file ip-address port-number size file)
  (when (file-exists? file) (error "file exists"))
  (copy-port (dcc-get ip-address port-number size) 
             (open-output-file file)))



(provide (all-defined-out))


