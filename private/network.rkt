#lang racket/base

(require racket/match)

(define (network->host n)
  (bytes
   (bitwise-and (arithmetic-shift n -24) 255)
   (bitwise-and (arithmetic-shift n -16) 255)
   (bitwise-and (arithmetic-shift n -8) 255)
   (bitwise-and n 255)))
         
(define (host->network n)
  (bitwise-ior
   (arithmetic-shift (bytes-ref n 0) 24)
   (arithmetic-shift (bytes-ref n 1) 16)
   (arithmetic-shift (bytes-ref n 2) 8)
   (bytes-ref n 3)))

(define (ip-string->bytes ip)
  (match ip
    [(regexp #px"^([0-9]{1,3})[.]([0-9]{1,3})[.]([0-9]{1,3})[.]([0-9]{1,3})$" (list _ a b c d))
     (bytes (string->number a) (string->number b) (string->number c) (string->number d))]
    [else (error 'ip-string->bytes "not a valid ip address")]))

(define (bytes->ip-string b)
  (unless (= 4 (bytes-length b)) (error 'bytes->ip-string "provided bytes do not form a valid ip"))
  (string-append
   (number->string (bytes-ref b 0)) "."
   (number->string (bytes-ref b 1)) "."
   (number->string (bytes-ref b 2)) "."
   (number->string (bytes-ref b 3))))


(provide (all-defined-out))