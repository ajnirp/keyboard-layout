; Data

; Frequency distribution of some common bigrams
; Hash (String -> Float)
(define bigram-frequency
  (hash "th" 3.882543
        "he" 3.681391
        "in" 2.283899
        "er" 2.178042
        "an" 2.140460
        "re" 1.749394
        "nd" 1.571977
        "on" 1.418244
        "en" 1.383239
        "at" 1.335523
        "ou" 1.285484
        "ed" 1.275779
        "ha" 1.274742
        "to" 1.169655
        "or" 1.151094
        "it" 1.134891
        "is" 1.109877
        "hi" 1.092302
        "es" 1.092301
        "ng" 1.053385))

; Hash (String -> Float)
(define trigram-frequency
  (hash "the" 3.508232
        "and" 1.593878
        "ing" 1.147042
        "her" 0.822444
        "hat" 0.650715
        "his" 0.596748
        "tha" 0.593593
        "ere" 0.560594
        "for" 0.555372
        "ent" 0.530771
        "ion" 0.506454
        "ter" 0.461099
        "was" 0.460487
        "you" 0.437213
        "ith" 0.431250
        "ver" 0.430732
        "all" 0.422758
        "wit" 0.397290
        "thi" 0.394796
        "tio" 0.378058))