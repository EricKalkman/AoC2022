#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

; In this parsecomb, the stack is a tree with current working dir info
; each node in the tree is `(dir ,dirname ,@[files]* ,@[dirs]*) or `(file ,fname ,size)
; The root of the tree is special; the first element of the list is actually a list
; representing the current directory, with the parent directory as the first element of this list
; and the root directory as the last element, represented as '(). /a/b/c is represented as
; '(c b a) Taking the cdr of this list is the same as moving up a directory.
; The rest of the root node is as normal for a directory (i.e., root node = `(,cwd dir "/" ...))

; parser for a file entry in the output for ls
(define expect-file
  (and-then
    expect-natural
    skip-whitespace
    (group-output (at-least-one (expv-not-set '(#\newline))))
    skip-eol-or-eof
    (mod-stack (lambda (stack)
                 (cons
                   (list 'file (list->string (first stack)) (second stack))
                   (drop stack 2))))))

; parser for a directory entry in the output of ls
(define expect-dir
  (and-then
    (skip (expect-string "dir "))
    (group-output (at-least-one (expv-not-set '(#\newline))))
    skip-eol-or-eof
    (mod-top (lambda (dirname) (list 'dir (list->string dirname))))))

; traverses the directory tree to insert dir-contents into the directory
; specified by dir
; note that dir represents the path in the forward direction, i.e.,
; /a/b/c -> '(a b c)
; Contents are deduplicated; this is to prevent shenanigans if ls is
; called more than once in a given directory
(define (insert-into-tree tree dir dir-contents)
  (if (null? dir)
    ; we've reached the desired directory
    (append (take tree 2) ; directory name
            ; deduplicate contents of the directory just in case
            (foldl (lambda (item contents)
                     (if (member item contents)
                       contents
                       (cons item contents)))
                   (drop tree 2)
                   dir-contents))
    ; otherwise, look for the next level of dir in the current directory and traverse through it
    (let-values ([(head-node tail-node) (splitf-at (drop tree 2)
                                                   (lambda (x) (not (and (eqv? (first x) 'dir)
                                                                         (string=? (second x) (car dir))))))])
      (append
        (take tree 2)
        head-node
        (list (insert-into-tree (car tail-node) (cdr dir) dir-contents))
        (cdr tail-node)))))

; parser for the ls command and its output
(define expect-ls
  (and-then
    (group-output
      (and-then
        (skip (expect-string "$ ls"))
        skip-eol-or-eof
        (at-least-none
          (or-else
            expect-file
            expect-dir))))
    ; inserts just-parsed directory contents (the head of stack) into the directory
    ; tree
    (mod-stack (lambda (stack)
                 (let ([contents (first stack)]
                       [cwd (second stack)]
                       [tree (drop stack 2)])
                   (cons cwd
                         (insert-into-tree tree (reverse cwd) contents)))))))

; handles $ cd /
(define expect-cd-root
  (and-then
    (skip (expect-string "/"))
    skip-eol-or-eof
    (mod-top (lambda (cwd) '()))))
; handles $ cd ..
(define expect-cd-up
  (and-then
    (skip (expect-string ".."))
    skip-eol-or-eof
    (mod-top cdr)))
; handles $ cd dirname
(define expect-cd-dir
  (and-then
    (group-output (at-least-one (expv-not-set '(#\newline))))
    skip-eol-or-eof
    ; modifies the current working directory to reflect the cd command
    (mod-stack (lambda (stack) (cons (cons (list->string (first stack))
                                           (second stack))
                                     (drop stack 2))))))

; handles the cd command
(define expect-cd
  (and-then
    (skip (expect-string "$ cd "))
    (or-else
      expect-cd-root
      expect-cd-up
      expect-cd-dir)))

; handles all commands
(define expect-input
  (at-least-none
    (or-else
      expect-ls
      expect-cd)))

(define test-inp (load-file-as-list "inputs/day7.test"))
(define real-inp (load-file-as-list "inputs/day7.inp"))

(define (run-command-parser inp)
  (let-values ([(_ output) (expect-input inp '(() dir "/"))])
    output))

; recursively calculates the size of each directory and adds this information to each
; dir entry, i.e., `(dir ,name ...) becomes `(dir ,name ,total-size ...)
(define (decorate-tree-with-sizes tree)
  (let-values ([(files dirs) (partition (lambda (x) (eqv? (car x) 'file)) (drop tree 2))])
    (let ([sized-subdirs (map (lambda (subdir) (decorate-tree-with-sizes subdir)) dirs)]
          [size-of-files (sum (map third files))])
      (append (take tree 2)
              (list (+ (sum (map third sized-subdirs)) size-of-files))
              sized-subdirs
              files))))

(define (part-1 inp)
  (let loop ([flat-tree (-> (run-command-parser inp)
                            cdr
                            decorate-tree-with-sizes
                            flatten)]
             [directories '()]
             [total-size 0])
    (cond
      [(null? flat-tree) (values total-size directories)]
      ; consider only sufficiently small files
      [(and (eqv? (first flat-tree) 'dir) (<= (third flat-tree) 100000))
       (loop (drop flat-tree 3)
             (cons (second flat-tree) directories)
             (+ total-size (third flat-tree)))]
      [else (loop (drop flat-tree 3)
                  directories
                  total-size)])))

; returns a pair in which the first element is the total size and the second is
; a list of directories contributing to this count
(define (part-1-rewritten inp)
  (->> (run-command-parser inp)
       cdr
       decorate-tree-with-sizes
       flatten
       ; input is now a concatenated series of entries of the form `@(dir|file ,name ,size)
       ; fortunately, both file and directory records have only three elements, so we can
       ; group everything into chunks of 3 to recover FS object information
       (chunkify 3)
       (foldl (lambda (fs-entry count-and-dirs)
                (if (and (eqv? (first fs-entry) 'dir) (<= (third fs-entry) 100000))
                  (cons (+ (car count-and-dirs) (third fs-entry))
                        (cons (second fs-entry) (cdr count-and-dirs)))
                  count-and-dirs))
              (cons 0 '()))))

; returns the directory object of the directory whose deletion represents the smallest
; recovery of free space such that the amount of free space exceeds 30 MB
(define (part-2 inp)
  (let* ([tree (-> (run-command-parser inp)
                   cdr
                   decorate-tree-with-sizes)]
         [total-size (third tree)]
         [unused-space (- 70000000 total-size)])
    (->> (flatten tree)
         (chunkify 3)
         (filter (lambda (x) (eqv? (first x) 'dir)))
         (<<= sort (lambda (a b) (< (third a) (third b))))
         (<<= dropf (lambda (dir) (< (+ unused-space (third dir)) 30000000)))
         first)))
