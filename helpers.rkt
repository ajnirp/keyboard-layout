#lang racket

(define (perms str)
  (list-perms (string->list str)))

; conses corresponding elements of ls1 and ls2
(define (zip ls1 ls2)
  (if (or (null? ls1) (null? ls2)) null
      (cons (cons (car ls1) (car ls2))
            (zip (cdr ls1) (cdr ls2)))))

(define (remove-index ls index)
  (call-with-values (λ () (split-at ls index))
                    (λ (x y) (cons (car y) (append x (cdr y))))))

; n = length of ls
; one-removed returns a list of lists of length n
; each list of which has one index of ls removed
(define (one-removed ls)
  (map (lambda (index) (remove-index ls index)) (range (length ls))))

; ls is a list of lists
; append-all conses x to each list in ls
(define (append-all elmt ls)
  (map (λ (x) (cons elmt x)) ls))

(define (list-perms ls)
  (if (null? ls) '(())
      (append-map (λ (x) (append-all (car x) (list-perms (cdr x)))) (one-removed ls))))