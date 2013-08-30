#lang racket

(define chars "qwertyuiop[]asdfghjkl;'zxcvbnm,./")

(define (initialize-population string population-size)
  (map (λ (x) (list->string (shuffle (string->list x))))
       (build-list population-size (λ (x) string))))

