
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)

;; put your code below

(define cool (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define ones (lambda () (cons 1 ones)))

(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low
                  (sequence (+ low stride) high stride))])) 
        
(define (string-append-map xs suffix)
  (let ([f (lambda(x) (string-append x suffix))])
    (map f xs)))

(define (list-nth-mod xs n)
  (cond [(< n 0)    (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
    (cond [(<= n 0) null]
          [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream
  (letrec ([f (lambda (x)
                 (cons (if (= (remainder x 5) 0) (- 0 x) x)
                       (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda() (cons "dan.jpg" dog))]
           [dog (lambda() (cons "dog.jpg" dan))])
    dan))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car (x))) 
                      (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
              (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                    (lambda() (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([vlen (vector-length vec)]
           [f (lambda (n)
                (cond [(>= n vlen) #f]
                      [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v)
                                                      (vector-ref vec n)
                                                      (f (+ n 1)))]
                      [#t (f (+ n 1))]))])
    (f 0)))
  
