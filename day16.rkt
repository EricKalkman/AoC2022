#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-valve-name
  (and-then
    (group-output (at-least-one expect-uppercase))
    (mod-top list->string)))

(define expect-valve
  (group-output
    (and-then
      (skip-string "Valve ")
      expect-valve-name
      (skip-string " has flow rate=")
      expect-natural
      (or-else
        (skip-string "; tunnels lead to valves ") ; really? grammar? what is this?
        (skip-string "; tunnel leads to valve "))
      (expect-list expect-valve-name (expect-string ", ")))))

(define expect-input (at-least-one (and-then expect-valve skip-eol-or-eof)))

(define test-inp (parse-file expect-input "inputs/day16.test"))
(define real-inp (parse-file expect-input "inputs/day16.inp"))

; distances between each vertex in space-ish
; includes virtual vertices of distance 1 minute from each valve
; representing the "activation" step, which takes 1 min
(define (floyd-warshall verts) ; adj list
  (let-values ([(dists next)
    (for/fold ([dists (hash)]
               [next (hash)])
              ([vert verts])
      (let ([u (car vert)]
            [vs (cdr vert)])
        (values (apply hash-set* dists
                        (append-map (lambda (v) (list (cons u v) 1)) vs))
                (apply hash-set* next
                        (append-map (lambda (v) (list (cons u v) v)) vs)))))])
    (for/fold ([dists (apply hash-set* dists (append-map (lambda (v) (list (cons (car v) (car v)) 0)) verts))]
               [next (apply hash-set* next (append-map (lambda (v) (list (cons (car v) (car v)) (car v))) verts))])
              ([k (map car verts)])
      (for/fold ([dists dists]
                 [next next])
                ([j (map car verts)])
        (for/fold ([dists dists]
                   [next next])
                  ([i (map car verts)])
          (let ([ij (hash-ref dists (cons i j) +inf.0)]
                [ik (hash-ref dists (cons i k) +inf.0)]
                [kj (hash-ref dists (cons k j) +inf.0)])
            (if (> ij (+ ik kj))
              (values (hash-set dists (cons i j) (+ ik kj))
                      (hash-set next (cons i j) (hash-ref next i k)))
              (values dists next))))))))


(require racket/hash)

; recursively generates a graph representing the state space (valve × time)
; returns hash with `((u . arrive-time) . ((v v-arrive-time accumulated-flow-by-valve) ...))
; assumes act-verts is pruned so as to only consider distances between valve activation vertices
; returns a directed acyclic graph for vertices of the above form. All vertices can reach a special
; sink node '(end . -1)
(define (construct-time-dag dag start-vert act-verts flows uv-dists depth)
  (cond
    [(< depth 0) (error "shouldn't get here")]
    [(hash-ref dag (cons start-vert depth) #f) dag]
    [else
      (let* ([arrival-nodes (filter-map (lambda (v)
                                          (let* ([cost (hash-ref uv-dists (cons start-vert v))]
                                                 [arrive-at (- depth cost)])
                                            (if (or (= cost 0) (< arrive-at 0))  ; covers start-vert -> start-vert
                                              #f
                                              ; off by ones abound...
                                              (list v arrive-at (* arrive-at (hash-ref flows v))))))
                                        act-verts)])
        (for/fold ([dag (hash-set dag
                                  (cons start-vert depth)
                                  (cons (list 'end -1 0) arrival-nodes))])
                  ([v (in-list arrival-nodes)])
          (construct-time-dag dag
                              (first v)
                              act-verts
                              flows
                              uv-dists
                              (second v))))]))

; Activation vertices have a plus added to them, i.e., the activation node
; for valve AA is called AA+
(define (activation-name v)
  (string-append v "+"))
(define (valve-name act-vert)
  (substring act-vert 0 (sub1 (string-length act-vert))))
(define (activation-vert? v)
  (eqv? #\+ (string-ref v (sub1 (string-length v)))))


; trace the shorted path from stop to start using ,prevs
; ,prevs having been obtained from dijkstra (below)
(define (trace-path prevs start stop)
  (if (equal? start stop)
    '()
    (cons stop (trace-path prevs start (hash-ref prevs stop)))))

; dijkstra for finding the path of maximum accumulated flow, with
; pruning to prevent repeats in a given path (i.e., can't encounter AA twice)
; doesn't use a priority queue because I'm lazy and O(V^2) (or maybe O(V^3) if
; we consider that every relaxation step traverses the current path to make sure
; we're not duplicating)
(define (dijkstra dag start stop)
  (let loop ([dists (hash start 0)]
             [prevs (hash)]
             [q (list start)])
    (if (or (null? q) (eqv? (car q) stop))
      (values dists prevs)
      (let* ([u (argmin (lambda (u) (hash-ref dists u +inf.0)) q)]
             [vs (hash-ref dag u)])
        (call-with-values
          (lambda ()
            (for/fold ([dists dists]
                       [prevs prevs]
                       [q (remove u q)])
                      ([v-data (in-list vs)])
              (let* ([v (first v-data)]
                     [rnd (second v-data)]
                     [uv-cost (- (third v-data))]
                     [alt-cost (+ (hash-ref dists u) uv-cost)]
                     [cur-v-cost (hash-ref dists (cons v rnd) +inf.0)])
                (let ([cur-path-to-u (trace-path prevs start u)])
                  (cond
                    [(memv v (map car cur-path-to-u))
                     (values dists prevs q)]
                    [(< alt-cost cur-v-cost)
                     (values (hash-set dists (cons v rnd) alt-cost)
                             (hash-set prevs (cons v rnd) u)
                             (cons (cons v rnd) q))]
                    [else (values dists prevs q)])))))
          loop)))))

; helper function to create the time DAG for a given number of minutes
; returns two values: the dag and the labels for activation vertices
(define (create-time-dag-from-input inp ROUNDS)
  (let ([verts (map (lambda (valve)
                      (cons (first valve) (cons (activation-name (first valve))
                                                (third valve))))
                    inp)])
    (let ([verts (append verts
                         (map (lambda (vert) (cdr vert)) verts))]
          [flows (apply hash (append-map list (map (compose activation-name car) verts) (map second inp)))])
      ;(pretty-print (hash-ref flows "HH+"))
      (let-values ([(uv-dists uv-next) (floyd-warshall verts)])
        (let ([act-verts (filter activation-vert? (map car verts))])
          (values
            (-> (construct-time-dag (hash) "AA"
                                    ; remove valves that are stuck/give no flow
                                    ; not a huge in speed for this version, but much more important
                                    ; in algorithms finding all paths
                                    (filter (lambda (v) (> (hash-ref flows v) 0)) act-verts)
                                    flows uv-dists ROUNDS)
                (hash-set (cons 'end -1) (list)))
            act-verts))))))

(define ROUNDS 30)
(define (part-1 inp)
  (let-values ([(dag _) (create-time-dag-from-input inp ROUNDS)])
    (let-values ([(dists prevs) (dijkstra dag (cons "AA" ROUNDS) (cons 'end -1))])
      (- (hash-ref dists (cons 'end -1))))))

; helper function for finding all paths starting with ,steps
; ,dag is an adjacency list the graph of vertices of the form
; `(valve . time valve encountered). The edges keyed by that form also
; contain the flow payout (i.e., (hash-ref dag u) -> ((v time-v-encountered total-flow) ...)
(define (remaining-path dag steps left stop)
  (cond
    [(null? left) (list steps)]
    [(or (equal? (car steps) stop)) (list steps)]
    [else
      (foldl append
             '()
             (let* ([u (car steps)]
                    [vs (hash-ref dag u)])
               (filter-map (lambda (v)
                             (if (memv (first v) left)
                               ; continue the path given by each of the neighbors to the
                               ; latest vertex
                               (remaining-path dag
                                               (cons (cons (first v) (second v)) steps)
                                               (remv (first v) left)
                                               stop)
                               #f))
                           vs)))]))
(define (all-paths dag start stop act-verts)
  (map reverse (remaining-path dag (list start) act-verts stop)))

; calculate the total flow ("cost") allowed by the given path
(define (calc-costs dag path)
  (if (null? (cdr path))
    0
    (+ (let* ([u (first path)]
              [v (second path)]
              [uvdata (findf (lambda (vdata) (eqv? (first vdata) (car v)))
                             (hash-ref dag u))])
         (third uvdata))
       (calc-costs dag (cdr path)))))

(define ROUNDS-ALL-PATHS 30)
; this is a different version that runs slightly slower than the dijkstra algorithm above
; given that the dijkstra algorithm doesn't use a priority queue, I'm not too surprised
(define (part-1-all-paths inp)
  (let-values ([(dag act-verts) (create-time-dag-from-input inp ROUNDS-ALL-PATHS)])
    (let ([paths (all-paths dag (cons "AA" ROUNDS-ALL-PATHS) (cons 'end -1) (cons 'end act-verts))])
      (pretty-print "All paths done")
      ; find path with the highest cost/flow
      (let ([mx (argmax (lambda (x) (calc-costs dag x)) paths)])
        (values mx (calc-costs dag mx))))))

; determines whether two paths share caves that are not the start or end
(define (share-important-caves? p1 p2)
  (> (set-count (set-intersect p1 p2)) 1))

(define ROUNDS-ALL-PATHS-2 26)
(define (part-2-all-paths inp)
  (let-values ([(dag act-verts) (create-time-dag-from-input inp ROUNDS-ALL-PATHS-2)])
    (let ([scored-paths
            ; find all paths, convert the paths to sets of valves, remove trivial paths
            ; (cost = 0), and convert to vector for convenient O(n^2) indexing
            (->> (all-paths dag (cons "AA" ROUNDS-ALL-PATHS-2) (cons 'end -1) (cons 'end act-verts))
                 (map (lambda (path) (cons (calc-costs dag path) (list->seteq (map car (cdr path))))))
                 (filter (lambda (path) (> (car path) 0)))
                 list->vector)])
      (pretty-print "Found and filtered all paths")
      (pretty-print (vector-length scored-paths))
      ; compare every path to every other path that does not intersect, keeping track
      ; of the maximum combined total flow
      (for/fold ([max-pair (cons '(0) '(0))]
                 [max-score 0])
                ([i (in-range (sub1 (vector-length scored-paths)))])
        (let ([a (vector-ref scored-paths i)])
          (for/fold ([max-pair max-pair]
                     [max-score max-score])
                    ([j (in-range (add1 i) (vector-length scored-paths))])
            (let ([b (vector-ref scored-paths j)])
              (if (> (+ (car a) (car b)) max-score)
                (if (share-important-caves? (cdr a) (cdr b))
                  (values max-pair max-score)
                  (values (cons a b) (+ (car a) (car b))))
                (values max-pair max-score)))))))))
