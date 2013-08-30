;#lang racket
; File containing general purpose functions that are useful for this program

; (Listof A) (Listof A) -> (Listof (A . A))
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) null
      (cons (cons (car l1) (car l2))
            (zip (cdr l1) (cdr l2)))))

; String String -> (Listof (Char . Char))
(define (zip-strings s1 s2)
  (zip (string->list s1) (string->list s2)))

; returns a list containing alternate elements of l1 and l2
; starting from the first element of l1
; of length 2 * l1
; assumes that l1 and l2 have equal length
; (Listof A) (Listof A) -> (Listof A)
(define (splice l1 l2) (flatten (zip l1 l2)))

; String String -> (Listof Char)
(define (splice-strings s1 s2) (flatten (zip-strings s1 s2)))

; helper method for alternating crossover
; returns a list containing alternate elements of l1 and l2
; starting from the first element of l1
; of length l1
; assumes that l1 and l2 have equal length
; (Listof A) (Listof A) -> (Listof A)
(define (alternate l1 l2)
  (if (null? l1) null
      (cons (car l1) (alternate (cdr l2) (cdr l1)))))

; helper method for uniform crossover
; each element of the result list is
; equally likely to come from list1 or list2
; assumes that l1 and l2 have equal length
; (Listof A) (Listof A) -> (Listof A)
(define (uniform l1 l2)
  (if (null? l1) l1
      (cons (if (< (random) 0.5) (car l1) (car l2))
            (uniform (cdr l1) (cdr l2)))))

; helper method for single-point crossover
; i = number of elements we take from list 1
; ((Listof A) (Listof A) -> (Listof A))
(define (single-point l1 l2 i)
  (build-list (length l1) (λ (x) (list-ref (if (< x i) l1 l2) x))))

; helper method for halfway crossover
; defined as a special case of single-point crossover
; ((Listof A) (Listof A) -> (Listof A))
(define (halfway l1 l2) (single-point l1 l2 (/ (length l1) 2)))

; returns a new list identical to ls
; except that the i-th and j-th indices
; have been swapped
; (Listof A) -> (Listof A)
(define (swap ls i j)
  (build-list (length ls)
              (λ (x) (cond ((= x i) (list-ref ls j))
                           ((= x j) (list-ref ls i))
                           (else (list-ref ls x))))))