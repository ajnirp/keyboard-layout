(include "helpers.rkt")

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
