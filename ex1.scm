#lang planet neil/sicp

; ex 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; ex 1.3
(define (sum-max-2 a b c)
  (cond ((and (>= a b) (>= b c)) (+ a b))
        ((and (>= a b) (>= c b)) (+ a c))
        ((and (>= b a) (>= c a)) (+ b c))))

; ex 1.6
; else-clause in new-if will always be calculated
; thus never stop

; ex 1.7
(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.00001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; ex 1.8
(define (cubic-root x)
  (define (improve y)
    (/ (+ (/ x (* y y))
          (* 2 y))
       3))
  (define (good-enough? guess)
    (< (/ (abs (- (improve guess) guess)) guess) 1e-8))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

; ex 1.9
; recursive
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

; iterative
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

; ex 1.11

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (* 1 (f-recursive (- n 1)))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (iter i a b c)
    (if (> i n)
        a
        (iter (+ 1 i)
              (+ (* 1 a) (* 2 b) (* 3 c))
              a
              b)))
  (iter 3 2 1 0))

; ex 1.12
(define (pascal n m)
  (cond ((= n 0) 1)
        ((= m 0) 1)
        ((= n m) 1)
        (else (+ (pascal (- n 1) m)
                 (pascal (- n 1) (- m 1))))))


; ex 1.16
(define (fast-expt base expt)
  (define (odd? x)
    (= 1 (remainder x 2)))
  (define (iter a b n)
    (cond ((= n 0) a)
          ((odd? n) (iter (* a b) (* b b) (/ (- n 1) 2)))
          (else (iter a (* b b) (/ n 2)))))
  (iter 1 base expt))


; ex 1.17
(define (fast-* a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (odd? x) (= 1 (remainder x 2)))
  (cond ((= b 0) 0)
        ((odd? b) (+ a (fast-* a (- b 1))))
        (else (double (fast-* a (halve b))))))


; ex 1.18
(define (fast-times a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (odd? x) (= 1 (remainder x 2)))
  (define (iter res base n)
    (cond ((= n 0) res)
          ((odd? n) (iter (+ res base) (double base) (halve (- n 1))))
          (else (iter res (double base) (halve n)))))
  (iter 0 a b))


; ex 1. 19
(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q))
                 (/ count 2)))
          (else
           (iter (+ (* b q) (* a q) (* a p))
                 (+ (* b p) (* a q))
                 p
                 q
                 (- count 1)))))
  (iter 1 0 0 1 n))


; ex 1. 21
(define (prime? n)
  (define (divides? a b) (= 0 (remainder a b)))
  (define (smallest-divisor d)
    (cond ((> (* d d) n) n)
          ((divides? n d) d)
          (else (smallest-divisor (+ d 1)))))
  (and (> n 1)
       (= n (smallest-divisor 2))))


; ex 1. 22
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))
        #f))
  (define (report-prime elapsed-time)
    (display n)
    (display " *** ")
    (display elapsed-time)
    (newline)
    #t)
  (start-prime-test n (runtime)))

(define (search-for-primes n)
  (define (iter now remain)
    (if (> remain 0)
        (if (timed-prime-test now)
            (iter (+ now 2) (- remain 1))
            (iter (+ now 2) remain))))
  (if (even? n)
      (iter (+ n 1) 3)
      (iter (+ n 2) 3)))


; ex 1. 24
(define (expmod base n mod)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (remainder (square (expmod base (/ n 2) mod)) mod))
        (else (remainder (* (expmod base (- n 1) mod) base) mod))))

(define (fast-prime? n times)
  (define (try-it a) (= (expmod a n n) a))
  (define (fermat-test n)
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (define test-times 1000)
  (define (start-prime-test n start-time)
    (if (fast-prime? n test-times)
        (report-prime (- (runtime) start-time))
        #f))
  (define (report-prime elapsed-time)
    (display n)
    (display " *** ")
    (display elapsed-time)
    (newline)
    #t)
  (start-prime-test n (runtime)))


; ex 1. 29
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (if (even? k)
        (* 2.0 (y k))
        (* 4.0 (y k))))
  (* (/ h 3.0)
     (+ (y 0)
        (y n)
        (sum term 1 inc (- n 1)))))


; ex 1. 30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


; ex 1. 31
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (factorial n)
  (product-recursive identity 1 inc n))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-term x)
  (* (/ (- x 1) x)
     (/ (+ x 1) x)))
(define (pi-next x) (+ x 2.0))
(define pi
  (* 4.0
     (product pi-term 3 pi-next 1000000)))

; ex 1. 32
(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


; ex 1. 33
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (filtered-term a)
    (if (filter a)
        (term a)
        null-value))
  (if (> a b)
      null-value
      (combiner (filtered-term a)
                (filtered-accumulate combiner filter null-value term (next a) next b))))

(define (sum-prime a b)
  (filtered-accumulate + prime? 0 identity a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-coprime n)
  (define (filter x) (= 1 (gcd x n)))
  (filtered-accumulate * filter 1 identity 2 inc n))


; ex 1. 36
(define tolerance 1e-6)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
   (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))
  
(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x)))) 2.0)


; ex 1. 37
(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1e2)

(define (cont-frac n d k)
  (define (iter i sum)
    (if (< i 1)
        sum
        (iter (- i 1)
              (/ (n i) (+ (d i) sum)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1e6)

; ex 1. 38

(+ 2
   (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (cond ((< i 3) i)
                      ((= (remainder i 3) 2) (* 2. (/ (+ i 1.) 3.)))
                      (else 1.)))
              1e6))

; ex 1.39

(define (tan-cf x k)
  (define xx (- (* x x)))
  (cont-frac (lambda (i)
              (cond ((= i 1) x)
                    (else xx)))
             (lambda (i) (- (* i 2) 1))
             k))

; ex 1. 42

(define (compose f g)
  (lambda (x) (f (g x))))

; ex 1. 43

(define (repeated f n)
  (cond ((= n 1) f)
        (else (compose (repeated f (- n 1)) f))))

; ex 1. 44
(define (smooth f)
  (define dx 1e-5)
  (define (average x y z) (/ (+ x y z) 3))
  (lambda (x) (average (f (- x dx))
                       (f x)
                       (f (+ x dx)))))