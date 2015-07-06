; cons:
; ex 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p))

(define (car z)
  (z (lambda (p q) q))

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

(define (mul a b)
  (lambda (f)
    (lambda (x)
      ((b (a f)) x))))

(define fifteen (mul five three))
((fifteen append-#) "")
((fifteen inc) 0)