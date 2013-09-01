#lang racket

(include "data.rkt")
(include "keymaps.rkt")

; Helper functions and variables for computing the fitness of a layout

; Reads in a corpus file and returns its text
; with normalized spaces
; Docs for string-normalize spaces: Section 3.3 at the bottom
; String -> String
(define (read-file corpus-file)
  (string-normalize-spaces
   (call-with-input-file
       corpus-file
     (λ (input-port) (port->string input-port)))))


; Find the frequency of a given digraph or trigraph
; String -> Float
(define (frequency str)
  (hash-ref (if (= (string-length str) 2)
                bigram-frequency trigram-frequency)
            str))

; String -> Integer
(define (hand-transition-cost bigram)
  (if (xor (hand (string-ref bigram 0))
           (hand (string-ref bigram 1))) 0 1))

; Weights for the rows
; In order: upper, middle and lower
; (Listof Float)
(define row-weights '(1 2 3))

(define (row-transition-cost bigram) 0)
(define (finger-transition-cost bigram) 0)

(define (row-weight-cost bigram) 0)

(define (finger-weight-cost bigram) 0)

; A list of functions that take a bigram
; and return its cost
; (Listof (String -> Integer))
(define cost-functions
  (list hand-transition-cost
        row-transition-cost
        finger-transition-cost
        row-weight-cost
        finger-weight-cost))

; String -> (Listof Integer)
(define (costs bigram)
  (map (λ (x) (x bigram)) cost-functions))

; Weighted sum of the costs of a bigram
; String (Listof Float) -> (Listof Float)
(define (total-cost bigram weights)
  (apply + (map * weights (costs bigram))))
