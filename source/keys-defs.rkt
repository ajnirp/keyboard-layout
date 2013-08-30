#lang racket

(include "helpers.rkt")

; File containing methods and variables to initialize the population

; Evolution parameters
(define charset "qwertyuiop[]asdfghjkl;'zxcvbnm,./") ; String
(define shiftset "QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>?") ; String
(define shiftmap (apply hash (splice-strings charset shiftset))) ; Hash

(define population-size 300) ; Integer
(define generation-limit 500) ; Integer
(define mutation-probability 0.1) ; Float

; String Integer -> (Listof String)
(define (initialize-population charset population-size)
  (map (λ (x) (list->string (shuffle (string->list x))))
       (build-list population-size (λ (x) charset))))

; (Listof String)
(define initial-population (initialize-population charset population-size))

; String -> Integer
(define (fitness layout) 1)

; (Listof String) -> (Listof String)
(define (rank-fittest-first population)
  (sort population >
        #:key fitness
        #:cache-keys? #t))

; (Listof String) -> String
(define (fittest population)
  (car (rank-fittest-first population)))

; Swaps two elements of the string
; Might leave the string unchanged (unlikely)
; String -> String
(define (mutate-layout layout)
  (let ((ls (string->list layout)))
    (list->string (swap ls (random (length ls)) (random (length ls))))))

; Mutate each member of the population
; each with probability 'probability'
; (Listof String) -> (Listof String)
(define (mutate-probable population probability)
  (map (if (< (random) probability) mutate-layout identity) population))

; (Listof String) -> (Listof String)
(define (run initial-population)
  (define (run-helper population generation)
    (if (<= generation generation-limit)
        1
        population))
  (run-helper initial-population 1))