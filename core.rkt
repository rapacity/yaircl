#lang racket

(require openssl "reader.rkt")

(struct irc-connection
  (receiver
   sender
   thread
   host
   port
   password
   ssl?
   reconnect?
   reconnect-timeout
   nick
   name
   user
   mode
   wait
   handlers)
  #:mutable #:transparent)

(define current-irc-connection (make-parameter #f))

(define (pong-handler message)
  (when (eq? 'ping (irc-message-command message))
    (irc-pong (car (irc-message-arguments message)))))

(define (irc-connect host    [port              6667]
         #:password          [password          #f]
         #:ssl?              [ssl?              #f]
         #:reconnect?        [reconnect?        #f]
         #:reconnect-timeout [reconnect-timeout 0]
         #:nick              [nick              (symbol->string (gensym))]
         #:name              [name              (symbol->string (gensym))]
         #:user              [user              (symbol->string (gensym))]       
         #:mode              [mode              0]
         #:wait              [wait              0.2]
         #:handlers          [handlers          null])
  (define new-connection
    (irc-connection
     #f ; receiver
     #f ; sender
     #f ; thread
     host
     port
     password
     ssl?
     reconnect?
     reconnect-timeout
     nick
     name
     user
     mode
     wait
     `(,pong-handler ,@handlers)))
  (irc-establish-connection new-connection)
  new-connection)

(define (irc-establish-connection connection)
  (if (irc-connected? connection) (error 'irc-connect "Already connected")
      (let*-values ([(connect)      (if (irc-connection-ssl? connection) ssl-connect tcp-connect)]
                    [(input output) (connect (irc-connection-host connection)
                                             (irc-connection-port connection))])
        (file-stream-buffer-mode output 'none)
        (set-irc-connection-sender! connection output)
        (set-irc-connection-receiver! connection input)
        (set-irc-connection-thread! connection (thread (thunk (irc-main-loop connection))))
        (irc-connection-registration connection))))

(define (irc-connection-registration connection)
  (let ([sender   (irc-connection-sender connection)]
        [password (irc-connection-password connection)]
        [nickname (irc-connection-nick connection)])
    (when password (fprintf sender "PASS :~a~%" password))
    (fprintf sender "USER ~a ~a * :~a~%" 
      (irc-connection-user connection)
      (irc-connection-mode connection)
      (irc-connection-name connection))
    (fprintf sender "NICK ~a~%" nickname)))

(define (irc-disconnect [connection (current-irc-connection)] [message #f])
  (let ([receiver (irc-connection-receiver connection)]
        [sender   (irc-connection-sender connection)]
        [thread   (irc-connection-thread connection)])
    (set-irc-connection-thread! connection #f)
    (set-irc-connection-receiver! connection #f)
    (set-irc-connection-sender! connection #f)
    (with-handlers ([void void])
      (fprintf sender "QUIT :~a~%" message)
      (tcp-abandon-port receiver)
      (tcp-abandon-port sender))))

(define (irc-reconnect [connection (current-irc-connection)])
  (irc-disconnect connection)
  (irc-establish-connection connection))

(define (irc-connected? [connection (current-irc-connection)])
  (and (irc-connection-thread connection) #t))

(define (irc-main-loop connection)
  (define (maybe-reconnect)
    (if (irc-connection-reconnect? connection)
        (let ([timeout (irc-connection-reconnect-timeout connection)])
          (sleep timeout)
          (irc-reconnect connection))
        (irc-disconnect connection)))
  (define (notify-handlers message)
    (parameterize ([current-irc-connection connection])
      (for ([handler (in-list (irc-connection-handlers connection))])
        (thread (thunk (handler message))))))
  (let loop ()
    (let ([input (irc-connection-receiver connection)])
      (when input
        (let ([message (irc-read input)]
              [wait    (irc-connection-wait connection)])
          (if (eof-object? message) (maybe-reconnect)
              (begin (notify-handlers message)
                     (sleep wait)
                     (loop))))))))

(require (for-syntax syntax/parse racket/syntax))

(define-syntax (define-irc/out stx)
  (syntax-parse stx
    [(define-irc (name:id . args)
       body ...+)
      (with-syntax ([connection (format-id #'name "connection")])
        #`(define (name #:connection [connection (current-irc-connection)] . args)
            (define sender (irc-connection-sender connection))
            (if (not sender) (error 'name "Not connected.")
                (fprintf sender "~a\r\n" (begin body ...)))))]))

(define-syntax (define-irc stx)
  (syntax-parse stx
    [(define-irc (name:id . args)
       body ...+)
      (with-syntax ([connection (format-id #'name "connection")])
        #`(define (name #:connection [connection (current-irc-connection)] . args)
            (define sender (irc-connection-sender connection))
            (if (not sender) (error 'name "Not connected.")
                (begin body ...))))]))

(define-irc/out (irc-pong server-1 [server-2 #f])
  (format "PONG ~a~%" server-1))

(define-irc (irc-register-handler! proc)
  (let ([handlers (irc-connection-handlers connection)])
    (set-irc-connection-handlers! connection (cons proc handlers))))

(define-irc (irc-remove-handler! proc)
  (let ([handlers (irc-connection-handlers connection)])
    (set-irc-connection-handlers! connection (remq proc handlers))))

(provide (all-defined-out) (all-from-out "reader.rkt"))