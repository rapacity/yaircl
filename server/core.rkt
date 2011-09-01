#lang racket

(require "../reader.rkt")

(struct irc-listener
  (server
   port
   thread
   memberships
   channels
   clients
   welcome-message
   host
   version
   )
  #:mutable
  #:transparent)



(define (irc-listen port 
                    #:host [host "localhost"]
                    #:welcome-message [welcome-message "welcome to localhost"]
                    #:version [version "yaircl 0.01"])
  (define new-listener
    (irc-listener
     #f
     port
     #f
     null
     null
     null
     welcome-message
     host
     version
     ))
  (spawn-server-loop new-listener)
  new-listener)



(struct irc-client (nick user name host modes input output) #:mutable #:transparent)
(struct irc-channel (name topic modes) #:mutable #:transparent)
(struct irc-membership (channel client status) #:mutable #:transparent)


(define (can-join? o) #t)


(define (make-prefix client)
  (string-append ":" (irc-client-nick client) "!" (irc-client-user client) 
                 "@" (irc-client-host client)))

(define (client-channel-join memberships channel client)
  (unless (findf (λ (m) (and (eq? (irc-membership-channel m) channel)
                             (eq? (irc-membership-client m) client))) memberships)
    
  
    (cons (irc-membership channel client null) memberships)))

(define (client-part-all server client)
  (let ([memberships (irc-listener-memberships server)])
    (define-values (deleted-memberships updated-memberships)
      (partition (λ (m) (eq? (irc-membership-client m) client)) memberships))
    (for* ([d (in-list deleted-memberships)]
           [m (in-list memberships)])
      (when (eq? (irc-membership-channel d) (irc-membership-channel m))
        (fprintf (irc-client-output (irc-membership-client m)) "~a PART ~a~%" 
                 (irc-channel-name (irc-membership-channel m)))))
    (set-irc-listener-memberships! server updated-memberships)))
 
(define (channels-ref channels channel-name)
  (findf (λ (c) (string=? channel-name (irc-channel-name c))) channels))

(define (client-in-channel? memberships client channel)
  (and channel
       (findf (λ (m) (and (eq? (irc-membership-client m) client)
                          (eq? (irc-membership-channel m) channel))) memberships)))

(define (client-join server client channel-name)
  (let* ([channels    (irc-listener-channels server)]
         [channel     (channels-ref channels channel-name)]
         [memberships (irc-listener-memberships server)])
    (cond [(not channel)
           (let ([new-channel (irc-channel channel-name "" null)])
             (set-irc-listener-channels! server
               (cons new-channel channels))
             (set-irc-listener-memberships! server
               (cons (irc-membership new-channel client null) memberships)))]
          [(not (client-in-channel? memberships client channel))
           (set-irc-listener-memberships! server
             (cons (irc-membership channel client null) memberships))])))

(define (channel-memberships memberships channel)
  (filter (λ (m) (eq? channel (irc-membership-channel m))) memberships))

(define (client-privmsg-channel server client channel-name message)
  (let* ([channel     (channels-ref (irc-listener-channels server) channel-name)]
         [memberships (and channel (channel-memberships (irc-listener-memberships server) channel))])
    (cond [(not channel)
           (printf "channel does not exist ~a~%" channel-name)]
          [(not (client-in-channel? memberships client channel))
           (printf "user '~a' is not on channel ~a~%" client channel-name)]
          [else (for* ([m (in-list memberships)])
                  (unless (eq? client (irc-membership-client m))
                    (fprintf (irc-client-output (irc-membership-client m)) "~a PRIVMSG ~a :~a~%"
                             (make-prefix client)
                             channel-name
                             message)))])))  
  


(define (irc-close listener)
  (let ([tcp-listener (irc-listener-server listener)]
        [thread-obj (irc-listener-thread listener)])
    (tcp-close tcp-listener)
    (kill-thread thread-obj)
    (set-irc-listener-server! listener #f)
    (set-irc-listener-thread! listener #f)))


(define (spawn-server-loop server)  
  (set-irc-listener-thread! server (thread (thunk (irc-server-loop server)))))



(define (get-host i) (let-values ([(_ f) (tcp-addresses i)]) f))

(define (destination-channel? name) #t)

(define (irc-server-loop server)
  (define (accept-client input output)
    (let ([client (irc-client #f #f #f (get-host input) null input output)])
      (let loop ()
        (match (irc-read input)
          [(? eof-object? m) #f]
          [(irc-message _ #f 'user (list user mode _ name))
           (set-irc-client-user! client user)
           (set-irc-client-modes! client (list mode))
           (set-irc-client-name! client name)]
          [(irc-message _ #f 'nick (list nick))
           (set-irc-client-nick! client nick)])
        (cond [(and (irc-client-nick client)
                    (irc-client-name client))
               (set-irc-listener-clients! server (cons client (irc-listener-clients server)))
               (fprintf output ":~a 001 ~a :~a~%"
                        (irc-listener-host server)
                        (irc-client-nick client)
                        (irc-listener-welcome-message server))
               
               (handle-client client)]
              [else (loop)]))))
  (define (handle-client client)
    (let loop ()
      (match (irc-read (irc-client-input client))
        [(irc-message _ _ 'privmsg (list destination message))
         (if (destination-channel? destination)
             (client-privmsg-channel server client destination message)
             #f #;(client-privmsg-client server client destination message))
         (loop)]
        [(irc-message _ _ 'join (list destination))
         (client-join server client destination)
         (loop)]
        [(? eof-object? message) #f]
        [else (loop)])))
  (define (main-loop)
    (let-values ([(input output) (tcp-accept (irc-listener-server server))])
      (file-stream-buffer-mode output 'none)
      (thread (thunk (accept-client input output))))
    (main-loop))
  (define (start-listen)
    (set-irc-listener-server! server (tcp-listen (irc-listener-port server)))
    (main-loop))
  (start-listen)
  (error "server died"))


(provide (all-defined-out))








