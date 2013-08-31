#lang racket

; Reads in a corpus file and returns its text
; with normalized spaces
; Docs for string-normalize spaces: Section 3.3 at the bottom
; String -> String
(define (read-file corpus-file)
  (string-normalize-spaces
   (call-with-input-file
       corpus-file
     (λ (input-port) (port->string input-port)))))