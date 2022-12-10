#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define parse-command
  (group-output
    (and-then
      expect-uppercase
      skip-whitespace
      expect-natural
      skip-eol-or-eof)))

(define parse-input
  (at-least-one parse-command))

(define test-inp (load-file-as-list "inputs/day09.test"))
(define test-inp-2 (load-file-as-list "inputs/day09.test2"))
(define real-inp (load-file-as-list "inputs/day09.inp"))

(define (clamp x lo hi) (min (max x lo) hi))

(define (move-tail tail-pos head-pos)
  (let ([head-dx (- (car head-pos) (car tail-pos))]
        [head-dy (- (cdr head-pos) (cdr tail-pos))])
    (if (and (<= (abs head-dx) 1) (<= (abs head-dy) 1))
      tail-pos
      `(,(+ (car tail-pos) (clamp head-dx -1 1)) . ,(+ (cdr tail-pos) (clamp head-dy -1 1))))))

(define (move-head head-pos dir)
  (case dir
    [(#\R) `(,(add1 (car head-pos)) . ,(cdr head-pos))]
    [(#\L) `(,(sub1 (car head-pos)) . ,(cdr head-pos))]
    [(#\D) `(,(car head-pos) . ,(sub1 (cdr head-pos)))]
    [(#\U)  `(,(car head-pos) . ,(add1 (cdr head-pos)))]))

(define (move-snake n-knots cmds)
  (->> cmds
       ; iterate through cmds, updating the position of each knot along the way
       ; returns a state
       ; first item of state = list of knot positions; second = accumlated list
       ; of tail positions
       (foldl (lambda (cmd state)
                (let* ([head-pos (car (first state))]
                       [rest-knots (cdr (first state))]
                       [all-tail-poses (second state)]

                       [new-head-pos (move-head head-pos cmd)]
                       ; update the rest of the knots' positions after having updated
                       ; the position of the first knot
                       [new-rest-poses
                         ; the accumulator is the list of new knot positions, in reverse order
                         ; this fold looks at the most recently updated knot (the car of
                         ; new-poses), and uses that as the head for the next knot
                         (foldl (lambda (next-knot-pos new-poses)
                                  (let* ([local-head (car new-poses)]
                                         [new-knot-pos (move-tail next-knot-pos local-head)])
                                    (cons new-knot-pos
                                          new-poses)))
                                (list new-head-pos)
                                rest-knots)])
                  ; return new state
                  (list (reverse new-rest-poses)  ; updated knot positions
                        (cons (car new-rest-poses) all-tail-poses)))) ; keep track of tail position
              (list (make-list n-knots '(0 . 0)) '((0 . 0))))
       second  ; list of tail positions
       remove-duplicates
       length))


(define (part-1 inp)
  (let-values ([(_ parsed) (run-parser parse-input inp)])
    (->> parsed
         ; expand multi-space movement commands into individual one-space commands
         (append-map (lambda (cmd) (make-list (second cmd) (first cmd))))
         (move-snake 2))))

(define (part-2 inp)
  (let-values ([(_ parsed) (run-parser parse-input inp)])
    (->> parsed
         (append-map (lambda (cmd) (make-list (second cmd) (first cmd))))
         (move-snake 10))))
