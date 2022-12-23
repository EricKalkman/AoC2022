#lang racket

;answer should be 1613

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-recipe
  (and-then
    (group-output
      (and-then
      skip-whitespace
      (skip-string "Each ")
      (or-else
        (expect-string "ore")
        (expect-string "clay")
        (expect-string "obsidian")
        (expect-string "geode"))
      (mod-top string->symbol)
      (skip-string " robot costs ")
      (expect-list
        (and-then expect-natural
                 (skip (expect #\space))
                 (or-else
                   (expect-string "ore")
                   (expect-string "clay")
                   (expect-string "obsidian"))
                 (mod-top string->symbol)
                 (mod-stack (lambda (stack) (cons (second stack) (cons (first stack) (cddr stack))))))
        (expect-string " and "))
      (skip (expect #\.))))
	(mod-top (lambda (x) (cons (first x) (apply hash (second x)))))))

(define expect-blueprint
  (and-then
    (group-output
      (and-then
        (skip-string "Blueprint ")
        expect-natural
        (skip-string ": ")
        (at-least-one expect-recipe)
        skip-eol-or-eof))
    (mod-top (lambda (x) (cons (car x) (make-hash (cdr x)))))))

(define expect-input (at-least-one expect-blueprint))

(define test-inp (parse-file expect-input "inputs/day19.test"))
(define real-inp (parse-file expect-input "inputs/day19.inp"))

(define (has-necessary-robots-to-build? bp bots bot)
  (let ([recipe (hash-ref bp bot)])
    (all (lambda (mat) (hash-ref bots mat #f)) (hash-keys recipe))))


(define (max-robots bp)
  (for/fold ([result (hash 'geode +inf.0)])
            ([(rob recipe) bp])
    (for/fold ([result result])
              ([(mat amt) recipe])
      (hash-update result mat (lambda (x) (max x amt)) 0))))


(define (time-to-build bp inv bots bot)
  (if (not (has-necessary-robots-to-build? bp bots bot))
    #f
    (let ([recipe (hash-ref bp bot)])
      (foldl max 0
             (map (lambda (mat) (ceiling (/ (- (hash-ref recipe mat) (hash-ref inv mat 0))
                                            (hash-ref bots mat))))
                  (hash-keys recipe))))))

(define (idle-n-rounds inv bots n)
  (for/fold ([inv inv])
            ([(bot count) bots])
    (hash-update inv bot (lambda (x) (+ x (* n count))) 0)))

(define (remove-resources inv mats)
  (for/fold ([inv inv])
            ([(mat count) mats])
    (hash-update inv mat (lambda (x) (- x count)))))

(define (best-geodes bp cur-inv cur-bots time-remaining cur-best)
  (let* ([best-from-current-geodes (+ (hash-ref cur-inv 'geode 0)
                                      (* time-remaining
                                         (hash-ref cur-bots 'geode 0)))]
         [cur-best (max best-from-current-geodes cur-best)])
    (cond
      [(or (<= time-remaining 0)
           ; even if we were to build a new geode bot every round until done, would we be able
           ; to achieve a good score?
           (>= cur-best (+ best-from-current-geodes (/ (* time-remaining (sub1 time-remaining))
                                                       2))))
       cur-best]
      [else
        (let* ([time-to-build (filter-map (lambda (bot)
                                            (if (or (not (has-necessary-robots-to-build? bp cur-bots bot))
                                                    (>= (hash-ref cur-bots bot 0) (hash-ref (max-robots bp) bot)))
                                              #f
                                              (cons bot (time-to-build bp cur-inv cur-bots bot))))
                                          '(geode obsidian clay ore))])
          (for/fold ([cur-best cur-best])
                    ([bot-and-time (in-list time-to-build)])
            (let* ([bot (car bot-and-time)]
                   [build-time (cdr bot-and-time)]
                   [finish-time (- time-remaining build-time 1)])
              (if (< finish-time 0)
                cur-best
                (let ([tmp-best (best-geodes
                                  bp
                                  (-> (idle-n-rounds cur-inv cur-bots (+ 1 build-time))
                                      (remove-resources (hash-ref bp bot)))
                                  (hash-update cur-bots bot add1 0)
                                  finish-time
                                  cur-best)])
                  (max tmp-best cur-best))))))])))

(define (do-rounds inp n)
  (->> inp
       (map (lambda (bp)
              (begin
                (display "Doing ")
                (pretty-print (car bp))
                (cons (car bp) (best-geodes (cdr bp) (hash) (hash 'ore 1) n 0)))))
       (map (lambda (p) (* (car p) (cdr p))))
       sum))

(define (part-1 inp)
  (do-rounds inp 24))

(define (part-2 inp)
  (->> inp
       (<<= take 3)
       (map (lambda (bp) (best-geodes (cdr bp) (hash) (hash 'ore 1) 32 0)))
       (foldl * 1)))