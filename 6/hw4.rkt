
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