#lang planet neil/sicp

; ex 2.1

(define (make-rat n d)
  (cond ((< d 0)
         (make-rat (- n) (- d)))
        (else
         (let ((g (gcd n d)))
           (cons (/ n g) (/ d g))))))

; ex 2.2

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (define (average x y) (/ (+ x y) 2))
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; ex 2.5

(define (cons x y) (* (expt 2 x) (expt 3 y)))
(define (remainder-count x mod)
  (define (iter n count)
    (if (= (remainder n mod) 0)
        (iter (/ n mod) (+ count 1))
        count))
  (iter x 0))
(define (car z) (remainder-count z 2))
(define (cdr z) (remainder-count z 3))

; ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((b f) ((a f) x)))))

(define (append-# s) (string-append s "#"))
(define five (add two three))
((five append-#) "")
((five inc) 0)


; ex 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound z) (min (car z) (cdr z)))
(define (upper-bound z) (max (car z) (cdr z)))

; ex 2.8

(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y))
                  (- (lower-bound y)))))


; ex 2.17

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

; ex 2.18

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

; ex 2.19

(define (cc amount coin-values)
  (define (first-denomination coin-values)
    (car coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; ex 2.20

(define (same-parity x . y)
  (define (same-parity-with-x y) (even? (- x y)))
  (define (recu l)
    (cond ((null? l) l)
          ((same-parity-with-x (car l))
           (cons (car l) (recu (cdr l))))
          (else (recu (cdr l)))))
  (cons x (recu y)))

; ex e.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) 
            (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

; ex 2.23

(define (for-each func items)
  (define (do)
    (func (car items))
    (for-each func (cdr items)))
  (if (not (null? items))
      (do)))

(for-each (lambda (x) (newline) (display x))
          (list 1 2 3 4 5))

; ex 2.27

(define (deep-reverse items)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs)
              (cons (deep-reverse (car xs)) ys))))
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else (iter items '()))))

(define mytree '((1 2 3 4) (5 6) (7 (8 9 (10 11) (12 13)) 14)))
(deep-reverse mytree)

; ex 2.28

(define (fringe tree)
  (define (recu sons)
    (if (null? sons)
        '()
        (append (fringe (car sons))
                (recu (cdr sons)))))
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (recu tree))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

; ex 2.30
; ex 2.31
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (son)
         (if (pair? son)
             (square-tree-map son)
             (* son son)))
       tree))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree-treemap tree)
  (tree-map (lambda (x) (* x x)) tree))

(define mytree '((1 2 3 4) (5 6) (7 (8 9 (10 11) (12 13)) 14)))
(square-tree mytree)
(square-tree-map mytree)
(square-tree-treemap mytree)

; ex 2.31

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

(subsets '(1 2 3))

; ex 2.34

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

; ex 2.35

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (tree)
                     (cond ((null? tree) 0)
                           ((not (pair? tree)) 1)
                           (else (count-leaves tree))))
                   t)))

(define mytree '((1 2 3 4) (5 6) (7 (8 9 (10 11) (12 13)) 14)))
(count-leaves mytree)

; ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define mylistlist '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 mylistlist)

; ex 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define mat1 '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
(define mat2 '((13 1) (14 2) (15 3) (16 4)))
(define vec1 '(13 14 15 16))
(matrix-*-vector mat1 vec1)
(matrix-*-matrix mat1 mat2)

; ex 2.39

(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define mylist '(1 2 3 4 5 6 7 8 9))
(reverse-fold-left mylist)
(reverse-fold-right mylist)

; ex 2.40

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime? n)
  (define (iter d)
    (cond ((> (* d d) n) #t)
          ((= (remainder n d) 0) #f)
          (else (iter (+ 1 d)))))
  (if (< n 2) #f (iter 2)))
        

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (let ((a (car pair))
          (b (cadr pair)))
      (list a b (+ a b))))
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(unique-pairs 5)
(prime-sum-pairs 6)


; ex 2.41

(define (ordered-triple-sum n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j (- n i j)))
          (enumerate-interval 1 (- n i 1))))
   (enumerate-interval 1 n)))

(ordered-triple-sum 5)


; ex 2.42

(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position row col board)
    (cons (cons row col) board))
  (define (safe? board)
    (define row (caar board))
    (define col (cdar board))
    (define (recu rest)
      (if (null? rest)
          #t
          (and (let ((r (caar rest))
                     (c (cdar rest)))
                 (not (or (= (- r c) (- row col))
                          (= (+ r c) (+ row col))
                          (= r row))))
               (recu (cdr rest)))))
    (recu (cdr board)))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         safe?
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define before (current-inexact-milliseconds))
(length (queens 12))
(define after (current-inexact-milliseconds))
(/ (- after before) 1000.0)