#lang racket

(require openssl "reader.rkt")

(struct irc-connection
  (receiver
   sender
   thread
   semaphore
   host
   port
   password
   ssl?
   reconnect?
   reconnect-wait
   nick
   name
   user
   mode
   wait
   error-handler
   message-handlers)
  #:mutable #:transparent)

(define current-irc-connection (make-parameter #f))

(define (pong-handler message)
  (when (eq? 'ping (irc-message-command message))
    (irc-pong (car (irc-message-arguments message)))))

; if an error occurs in the main loop, such as failure to reconnect, it'll dispatch the error-handler
(define (irc-connect host    [port              6667]
         #:password          [password          #f]
         #:ssl?              [ssl?              #f]
         #:reconnect?        [reconnect?        #f]
         #:reconnect-wait    [reconnect-wait    12]
         #:nick              [nick              (symbol->string (gensym))]
         #:name              [name              (symbol->string (gensym))]
         #:user              [user              (symbol->string (gensym))]       
         #:mode              [mode              0]
         #:wait              [wait              0.2]
         #:error-handler     [error-handler     (λ (e) (printf "error: ~a~%" (exn-message e)))]
         #:message-handlers  [message-handlers  null])
  (define new-connection
    (irc-connection
     #f ; receiver
     #f ; sender
     #f ; thread
     (make-semaphore 1)
     host
     port
     password
     ssl?
     reconnect?
     reconnect-wait
     nick
     name
     user
     mode
     wait
     error-handler
     `(,pong-handler ,@message-handlers)))
  (spawn-new-connection-loop new-connection)
  new-connection)


(define (spawn-new-connection-loop connection)
  (set-irc-connection-thread! connection (thread (thunk (irc-main-loop connection)))))

(define (irc-establish-connection connection)
  (with-handlers ([(λ (_) #t)     (λ (e)
                                    (let ([error-handler (irc-connection-error-handler connection)])
                                      (irc-cleanup-ports connection)
                                      (error-handler e)
                                      ))])
    (let*-values ([(connect)      (if (irc-connection-ssl? connection) ssl-connect tcp-connect)]
                  [(input output) (connect (irc-connection-host connection)
                                           (irc-connection-port connection))])
      (file-stream-buffer-mode output 'none)
      (set-irc-connection-sender! connection output)
      (set-irc-connection-receiver! connection input)
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

(define (irc-disconnect [message #f] [connection (current-irc-connection)])
  (let ([semaphore (irc-connection-semaphore connection)])
    (call-with-semaphore semaphore
     (thunk
      (let ([sender   (irc-connection-sender connection)])
        (irc-cleanup-thread connection)
        (with-handlers ([void void]) (when message (fprintf sender "QUIT :~a~%" message)))
        (irc-cleanup-ports connection))))))

(define (irc-reconnect [connection (current-irc-connection)])
  (let ([semaphore (irc-connection-semaphore connection)])
    (call-with-semaphore semaphore
     (thunk
      (irc-cleanup-thread connection)
      (irc-cleanup-ports connection)
      (spawn-new-connection-loop connection)))))

(define (irc-connected? [connection (current-irc-connection)])
  (and (irc-connection-receiver connection) #t))

(define (irc-cleanup connection)
  (irc-cleanup-ports connection)
  (irc-cleanup-thread connection))

(define (irc-cleanup-thread connection)
  (with-handlers ([void void]) 
    (let ([thread   (irc-connection-thread connection)])
      (set-irc-connection-thread! connection #f)
      (kill-thread thread))))

(define (irc-cleanup-ports connection)
  (with-handlers ([void void]) 
    (let ([receiver (irc-connection-receiver connection)]
          [sender   (irc-connection-sender connection)])
      (set-irc-connection-receiver! connection #f)
      (set-irc-connection-sender! connection #f)
      (close-input-port receiver)
      (close-output-port sender))))

(define (irc-main-loop connection)
  (define (notify-handlers message)
    (parameterize ([current-irc-connection connection])
      (for ([handler (in-list (irc-connection-message-handlers connection))])
        (thread (thunk (handler message))))))
  (define (maybe-reconnect-loop)
    (if (irc-connection-reconnect? connection)
        (let ([reconnect-wait (irc-connection-reconnect-wait connection)])
          (sleep reconnect-wait)
          (connect-loop))
        (irc-cleanup connection)))
  (define (connect-loop)
    (irc-establish-connection connection)
    (if (irc-connected? connection) (main-loop)
        (maybe-reconnect-loop)))
  (define (main-loop)
    (let* ([input (irc-connection-receiver connection)]
           [message (irc-read input)])
      (if (eof-object? message) (maybe-reconnect-loop)
          (let ([wait (irc-connection-wait connection)])
            (thread (thunk (notify-handlers message)))
            (sleep wait)
            (main-loop)))))
  (connect-loop))
  
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
            body ...))]))

(define-irc/out (irc-pong server-1 [server-2 #f])
  (format "PONG ~a~%" server-1))

(define-irc (irc-register-handler! proc)
  (let ([semaphore (irc-connection-semaphore connection)])
    (call-with-semaphore semaphore
     (thunk
      (let ([handlers (irc-connection-message-handlers connection)])
        (set-irc-connection-message-handlers! connection (cons proc handlers)))))))

(define-irc (irc-remove-handler! proc)
  (let ([semaphore (irc-connection-semaphore connection)])
    (call-with-semaphore semaphore
     (thunk
      (let ([handlers (irc-connection-message-handlers connection)])
        (set-irc-connection-message-handlers! connection (remq proc handlers)))))))
     
(provide (all-defined-out) (all-from-out "reader.rkt"))
