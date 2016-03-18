#lang racket
;; 1a returns the largest element of lis
(define (maxElement lis)
	(cond ((eqv? 1 (length lis)) (car lis))
		(else (max (car lis) (maxElement(cdr lis))))
	)
)

;; 1b retuns the n-th element from lis
(define (elementAtIndex lis n)
	(cond ((eqv? 1 n) (car lis))
		(else (elementAtIndex (cdr lis) (- n 1)))
	)
)