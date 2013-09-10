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

; Find the frequency of a given bigram or trigram
; String -> Float
(define (frequency str)
  (hash-ref (if (= (string-length str) 2)
                bigram-frequency trigram-frequency)
            str))

; String -> Integer
(define (hand-transition-cost bigram)
  (let ((fst-char (string-ref bigram 0))
        (snd-char (string-ref bigram 1)))
    (if (xor (hand fst-char) (hand snd-char)) 0 1)))

; Weights for the rows
; In order: upper, middle and lower
; (Listof Float)
(define row-weights
  (hash 'top 1
        'middle 2
        'bottom 3))

; Cost for chars that require
; use of the Shift key
; Float
(define shift-penalty 1.5)

(define (finger-transition-cost bigram) 0)

; Row weight cost for a bigram
; Char -> Float
(define (row-weight char)
  (hash-ref row-weights (row char)))
; String -> Float
(define (row-weight-cost bigram)
  (let ((fst-char (string-ref bigram 0))
        (snd-char (string-ref bigram 1)))
    (+ (row-weight fst-char) (row-weight snd-char))))

(define (finger-weight-cost bigram) 0)

; Number of rows moved across in the same hand transition
; String -> Float
(define (row-transition-cost bigram)
  (let ((fst-char (string-ref bigram 0))
        (snd-char (string-ref bigram 1)))
    (abs (- (row-weight fst-char) (row-weight snd-char)))))
; Explanation: if the two chars are x rows apart, then
; row weight of the first minus row weight of the second
; will either be x or -x, where x is one of 0, 1 or 2
; so we return (abs x)

; A list of functions that take a bigram
; and return its cost
; (Listof (String -> Float))
(define cost-functions
  (list hand-transition-cost
        row-transition-cost
        finger-transition-cost
        row-weight-cost
        finger-weight-cost))

; String -> (Listof Float)
(define (costs bigram)
  (map (λ (cost-function) (cost-function bigram)) cost-functions))

; Weighted sum of the costs of a bigram
; String (Listof Float) -> (Listof Float)
(define (total-cost bigram weights)
  (apply + (map * weights (costs bigram))))
