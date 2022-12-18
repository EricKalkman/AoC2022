#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-cube
  (and-then
    (expect-list expect-int (expect #\,))))
(define expect-input (at-least-one (and-then expect-cube skip-eol-or-eof)))

(define test-inp (parse-file expect-input "inputs/day18.test"))
(define real-inp (parse-file expect-input "inputs/day18.inp"))


; returns all faces of the cube at ,cube-pos = `(,x ,y ,z). The coordinates
; of the cube are of the corner with the least positive x, y, and z values
; for example, a cube with a center at (0.5 0.5 0.5) is represented by (0 0 0)
;
; faces are of the form (cons (list x y z) 'plane), where x, y, z represent
; the corner of the face with the least positive x, y, z values
; for example, the -x face of thee above cube is represented as
; '((0 0 0) . yz)
(define (cube-faces cube-pos)
  (let ([x (first cube-pos)]
        [y (second cube-pos)]
        [z (third cube-pos)])
  (list (cons cube-pos 'xy)
        (cons cube-pos 'yz)
        (cons cube-pos 'xz)
        (cons (list x y (add1 z)) 'xy)
        (cons (list x (add1 y) z) 'yz)
        (cons (list (add1 x) y z) 'xz))))

; find all faces of all cubes. if we find a face shared by multiple cubes,
; remove it. the remaining faces represent the surface area
(define (part-1 inp)
  (->
    (for/fold ([all-faces (set)])
              ([cube-pos (in-list inp)])
      (for/fold ([all-faces all-faces])
                ([face (cube-faces cube-pos)])
        (if (set-member? all-faces face)
          (set-remove all-faces face)
          (set-add all-faces face))))
    set-count))

; when moving from points ,from to ,to, which direction are we heading?
; returns a symbol /[+-][xyz]/
(define (dir-of-travel from to)
  (cond
    [(< (first from) (first to)) 'x]
    [(> (first from) (first to)) '-x]
    [(< (second from) (second to)) 'y]
    [(> (second from) (second to)) '-y]
    [(< (third from) (third to)) 'z]
    [(> (third from) (third to)) '-z]
    [else (error "Points are identical")]))

; any direction has a plane to which it is perpendicular; return that plane
(define (dir-to-plane dir)
  (case dir
    [(x -x) 'yz]
    [(y -y) 'xz]
    [(z -z) 'xy]
    [else (error dir)]))

; return the face crossed when moving from ,from to ,to (see above for
; the format for a face)
(define (get-joining-face from to)
  (let ([dir (dir-of-travel from to)])
    (case dir
      [(x y z) (cons to (dir-to-plane dir))]
      [(-x -y -z) (cons from (dir-to-plane dir))]
      [else (error "Unrecognized dir of travel; shouldn't get here")])))

; all facial neighbors of coord
(define (neighbors coord)
  (let ([x (first coord)] [y (second coord)] [z (third coord)])
    (list (list (add1 x) y z) (list (sub1 x) y z)
          (list x (add1 y) z) (list x (sub1 y) z)
          (list x y (add1 z)) (list x y (sub1 z)))))

(define (part-2 inp)
  ; calculate the bounds on a box for the following DFS
  (let ([min-x (sub1 (foldl min (first (car inp)) (map first (cdr inp))))]
        [max-x (add1 (foldl max (first (car inp)) (map first (cdr inp))))]
        [min-y (sub1 (foldl min (second (car inp)) (map second (cdr inp))))]
        [max-y (add1 (foldl max (second (car inp)) (map second (cdr inp))))]
        [min-z (sub1 (foldl min (third (car inp)) (map third (cdr inp))))]
        [max-z (add1 (foldl max (third (car inp)) (map third (cdr inp))))]
        [cube-set (list->set inp)])
    (->
      (call-with-values
        (lambda ()
          ; use DFS to find the exposed surface. every time we consider a neighboring
          ; coordinate, check if we're moving into a block. if so, store the face we
          ; just crossed
          (let dfs ([pos (list min-x min-y min-z)]
                    [surface-faces (set)]
                    [seen-coords (set)])
            ; we've visited the current coord; mark it as visited
            (let ([seen-coords (set-add seen-coords pos)])
              ; if we're currently inside a block of lava, don't consider any
              ; neighbors; dead-end the search
              (if (set-member? cube-set pos)
                (values surface-faces seen-coords)
                ; for each adjacent neighbor; returns multiple values:
                ; a set of surface faces, and a set of visited coordinates
                (for/fold ([surface-faces surface-faces]
                           [seen-coords seen-coords])
                          ; filter all facially-adjacent neighbors to make sure
                          ; they are in bound
                          ([neigh (filter (lambda (n)
                                            (and (<= min-x (first n) max-x)
                                                 (<= min-y (second n) max-y)
                                                 (<= min-z (third n) max-z)))
                                          (neighbors pos))])
                  ; the face intersected when moving from pos to neigh
                  (let ([joining-face (get-joining-face pos neigh)])
                    ; only continue the DFS if we haven't been to the neighbor yet
                    (if (set-member? seen-coords neigh)
                      ; neigh seen, dead-end the search
                      (values (if (set-member? cube-set neigh)  ; crossed into cube
                                (set-add surface-faces joining-face)  ; keep track of face
                                surface-faces)
                              seen-coords)
                      ; otherwise, continue the DFS at neigh
                      (dfs neigh
                           (if (set-member? cube-set neigh)  ; crossed into cube
                             (set-add surface-faces joining-face)  ; keep track of face
                             surface-faces)
                           seen-coords))))))))
        ; handle multiple values; only return the surface faces
        (lambda (surface-faces seen-coords) surface-faces))
      ; count the surface faces
      set-count)))
