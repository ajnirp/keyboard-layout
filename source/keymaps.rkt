; File containing mappings for keys
; and associated hash-map accessors

; String
(define right-hand-keys "yuiop[]hjkl;'nm,./<>?:\"{}")
(define left-hand-keys "qwertasdfgzxcvb")

; Hashmap mapping the 33 chars
; to #t (for right) and #f (for left)
; Hash
(define handmap
  (apply hash
         (append (add-between (string->list right-hand-keys) '(#t)
                              #:after-last '(#t) #:splice? #t)
                 (add-between (string->list left-hand-keys) '(#f)
                              #:after-last '(#f) #:splice? #t))))

; Find out which hand is used to type a character
; #t => right. #f => left
; Char -> Symbol
(define (hand char)
  (hash-ref handmap (char-downcase char)))
; Note: no need to check for symbols
; since char-downcase does nothing to them

; String
(define top-row-keys "qwertyuiop[]{}")
(define middle-row-keys "asdfghjkl;':\"")
(define bottom-row-keys "zxcvbnm,./.<>?")

; Hashmap mapping the 33 chars to their rows
; Hash
(define rowmap
  (apply hash
         (append (add-between (string->list top-row-keys) '(top)
                              #:after-last '(top) #:splice? #t)
                 (add-between (string->list middle-row-keys) '(middle)
                              #:after-last '(middle) #:splice? #t)
                 (add-between (string->list bottom-row-keys) '(bottom)
                              #:after-last '(bottom) #:splice? #t))))

; Find which row a char is in
; Char -> Symbol (one of 'top 'middle or 'bottom)
(define (row char) (hash-ref rowmap (char-downcase char)))

; Predicates for checking if a char is in a particular row
; Char -> Bool
(define (top-row? char) (eq? (row char) 'top))
(define (middle-row? char) (eq? (row char) 'middle))
(define (bottom-row? char) (eq? (row char) 'bottom))