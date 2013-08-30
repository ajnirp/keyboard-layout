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

; A higher order function for defining crossovers
; ((Listof A) (Listof A) -> (Listof A)) String String -> String
(define (crossover style parent-1 parent-2)
  (list->string (style (string->list parent-1) (string->list parent-2))))

; String String -> String
(define (alternating-crossover parent-1 parent-2) (crossover alternate parent-1 parent-2))
(define (uniform-crossover parent-1 parent-2) (crossover uniform parent-1 parent-2))
(define (halfway-crossover parent-1 parent-2) (crossover halfway parent-1 parent-2))

; A generalised version of halfway crossover
; String String Integer -> String
(define (single-point-crossover parent-1 parent-2 index)
  (list->string (single-point (string->list parent-1) (string->list parent-2) index)))

; A higher order function for defining breeds based on crossovers
; ((Listof A) (Listof A) -> (Listof A)) String String -> (String . String)
(define (breed style parent-1 parent-2)
  (list (crossover style parent-1 parent-2)
        (crossover style parent-2 parent-1)))

; String String -> (String String)
(define (breed-alternating parent-1 parent-2) (breed alternate parent-1 parent-2))
(define (breed-uniform parent-1 parent-2) (breed uniform parent-1 parent-2))
(define (breed-halfway parent-1 parent-2) (breed halfway parent-1 parent-2))

; String -> String
(define (mutate-layout layout)
  (let ((ls (string->list layout)))
    (list->string (swap ls (random (length ls)) (random (length ls))))))

; (Listof String) -> (Listof String)
(define (mutate population)
  (map (if (< (random) mutation-probability)
           mutate-layout
           identity)
       population))

; (Listof String) -> (Listof String)
(define (run initial-population)
  (define (run-helper population generation)
    (if (<= generation generation-limit)
        1
        population))
  (run-helper initial-population 1))