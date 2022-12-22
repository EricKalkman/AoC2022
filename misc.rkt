#lang racket


(provide
  load-file
  load-file-as-list
  ->
  ->>
  any->
  sum
  chunkify
  cart-product
  vector-foldl
  vector-foldr
  any
  all
  vector-update!
  alist-refv
  alist-upv
  alist-setv
  alist-keys
  alist-values)

; just gulp a file
(define (load-file fname)
  (port->string (open-input-file fname) #:close? #t))
(define (load-file-as-list fname)
  (string->list (load-file fname)))

; threading macro; passes the result of one function as the first argument
; of the next
(define-syntax ->
  (syntax-rules (=>>)
    [(-> val) val]
    ; =>> makes it so that the threaded value is actually passed as the last
    ; argument instead of the first
    [(-> val (=>> fn args ...) others ...) (-> (fn args ... val) others ...)]
    [(-> val (fn args ...) others ...) (-> (fn val args ...) others ...)]
    [(-> val fn others ...) (-> (fn val) others ...)]))

; same as ->, but passes the result as the last argument of the next function
(define-syntax ->>
  (syntax-rules (<<=)
    [(->> val) val]
    ; <<= makes it so that the threaded value is actually passed as the last
    ; argument instead of the first
    [(->> val (<<= fn args ...) others ...) (->> (fn val args ...) others ...)]
    [(->> val (fn args ...) others ...) (->> (fn args ... val) others ...)]
    [(->> val fn others ...) (->> (fn val) others ...)]))

; threading macro that allows one to specify a placeholder varialbe marking the
; position into which a result will be passed into the next function, i.e.,
; (any-> '(a b c) lst
;   (append lst '(d))
;   (map (lambda (x) (cons x 42)) lst))
; gives
; '((a . 42) (b . 42) (c . 42) (d . 42))
(define-syntax (any-> stx)
  (syntax-case stx ()
    [(_ value it) #'value]
    [(_ value it (fn ...) others ...)
      #'(let [(it value)]
          (any-> (fn ...) it others ...))]))

(define (sum lst)
  (foldl + 0 lst))

; breaks a list into groups of n
; assumes (length lst) mod n is 0
(define (chunkify n lst)
  (if (null? lst)
    '()
    (let-values ([(head tail) (split-at lst n)])
      (cons head (chunkify n tail)))))

(define (cart-product lst1 lst2)
  (if (null? lst1)
    '()
    (append
      (map (lambda (x) (cons (car lst1) x)) lst2)
      (cart-product (cdr lst1) lst2))))

(define (vector-foldl fn seed vec . vecs)
  (let loop ([i 0]
             [acc seed])
    (if (>= i (vector-length vec))
      acc
      (loop (+ i 1)
            (apply fn (append (map (lambda (v) (vector-ref v i)) (cons vec vecs))
                              (list acc)))))))

(define (vector-foldr fn seed vec . vecs)
  (let loop ([i (- (vector-length vec) 1)]
             [acc seed])
    (if (< i 0)
      acc
      (loop (- i 1)
            (apply fn (append (map (lambda (v) (vector-ref v i)) (cons vec vecs))
                              (list acc)))))))


(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

(define (all pred lst)
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (all pred (cdr lst))]
    [else #f]))

(define (vector-update! v idx fn)
  (let ([val (vector-ref v idx)])
    (vector-set! v idx (fn val))
    v))

(define (alist-refv alist k . default)
  (cond
    [(null? alist) (car default)]
    [(eqv? (caar alist) k) (cdar alist)]
    [else (apply alist-refv (cdr alist) k default)]))

(define (alist-upv alist k fn . default)
  (cond
    [(null? alist) (list (cons k (fn (car default))))]
    [(eqv? (caar alist) k) (cons
                             (cons k (fn (cdar alist)))
                             (cdr alist))]
    [else (cons (car alist)
                (apply alist-upv (cdr alist) k fn default))]))

(define (alist-setv alist k v)
  (cond
    [(null? alist) (list (cons k v))]
    [(eqv? (caar alist) k) (cons
                             (cons k v)
                             (cdr alist))]
    [else (cons (car alist)
                (alist-setv (cdr alist) k v))]))

(define (alist-keys alist) (map car alist))
(define (alist-values alist) (map cdr alist))
