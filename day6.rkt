#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define test-inp (string->list (string-trim (load-file "inputs/day6.test"))))
(define real-inp (string->list (string-trim (load-file "inputs/day6.inp"))))

; Modified naive search
; Slowly grows a substring (slice), consuming input
; If a duplicate is encountered, drops the minimal prefix from
; the slice such that the duplicate is eliminated
(define (no-dup-substr inp len)
  ; invariant: slice does not contain a duplicate
  (let loop ([slice (list (car inp))]
             [slice-idx 0]
             [slice-len 1]
             [remaining (cdr inp)])
    (cond
      ; we have found ,len non-duplicate characters; success
      [(= slice-len len) (values (+ slice-idx len) slice)]
      ; we ran out of characters but haven't found a substring of suitable
      ; length; failure
      [(null? remaining) #f]
      [else
        ; checks if the next character would create a slice containing
        ; a duplicate. If so, not the index of the duplicate in the current
        ; slice
        (let ([dup-idx (index-of slice (car remaining))])
          (if dup-idx
            (loop
              ; drop everything in the slice until adding the next character would
              ; not create a duplicate
              (append (drop slice (+ 1 dup-idx)) (list (car remaining)))
              ; increase the starting index by the number of characters dropped
              (+ 1 slice-idx dup-idx)
              ; increase the length by one, decrease the length by the number of
              ; characters dropped
              (- slice-len dup-idx)
              (cdr remaining))
            ; no duplicate found, just append
            (loop (append slice (list (car remaining)))
                  slice-idx
                  (+ 1 slice-len)
                  (cdr remaining))))])))

(define (part-1 inp)
  (no-dup-substr inp 4))

(define (part-2 inp)
  (no-dup-substr inp 14))
