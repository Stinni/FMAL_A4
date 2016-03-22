#lang racket

;; Student: Kristinn Heidar Freysteinsson
;; Kt.: 011081-3209
;; Username: kristinnf13
;; Email: kristinnf13@ru.is

;; 1a returns the largest element of lis
(define (maxElement lis)
	(cond ((eqv? 1 (length lis)) (car lis))
		(else (max (car lis) (maxElement(cdr lis))))
	)
)

;; 1b returns the n-th element from lis
(define (elementAtIndex lis n)
	(cond ((eqv? 1 n) (car lis))
		(else (elementAtIndex (cdr lis) (- n 1)))
	)
)

;; 1c returns a compressed list
(define (compress lis)
	(cond ((null? lis) '())
		((null? (cdr lis)) lis)
		((eqv? (car lis) (car(cdr lis))) (compress (cdr lis)))
		(else (cons (car lis) (compress(cdr lis)))))
)

;; 1d returns the maximum element in a list of lists
(define (maxofmax lis)
	(maxElement(map maxElement lis))
)

;; 1e returns a split list at position n
	(define (splithelp lis n)
	  (cond ((eqv? 0 n) '())
		(else (cons (car lis) (splithelp (cdr lis) (- n 1))))
	)
)
(define (getsecond lis n)
	(cond ((eqv? 0 n) lis)
		(else (getsecond (cdr lis) (- n 1)))
	)
)
(define (split lis n)
	(list (splithelp lis n) (getsecond lis n))
)

;; 2a returns the sum of a list of arguments
(define (sumList lis)
	(cond ((null? lis) 0)
		(else (+ (car lis) (sumList(cdr lis))))
	)
)

;; 2b returns a list of lengths of words
(define (charCount sym) (string-length (symbol->string sym)))
(define (charactersPerWord lis)
	(map charCount lis)
)

;; 2c returns a list of lengths of sentences
(define (charactersPerSentence lis)
	(map sumList (map charactersPerWord lis))
)

;; 2d returns a list of number of words in sentences
(define (wordsPerSentence lis)
	(map length lis)
)

;; 2e returns a list of stats of a list of sentences
(define (statsDocument lis)
	(list
		(sumList (charactersPerSentence lis))
		(sumList (wordsPerSentence lis))
		(length lis)
	)
)

;; ------------------ TEST CASES ------------------

(maxElement '(5 3 7 2 6 1 4))
(elementAtIndex '(a b c d e f) 4)
(compress '(a a a b b c c c d d d d e f f))
(maxofmax '((5 3 6 2) (1 6 2 7) (7 3 8 2 9) (6 2 4 1)))
(split '(a b c d e f g) 3)

(sumList '(5 4 3))
(charactersPerWord '(Scheme is so wonderful))
(charactersPerSentence '((Hello Mary) (Scheme is so wonderful) (See you later) (John)))
(wordsPerSentence '((Hello Mary) (Scheme is so wonderful) (See you later) (John)))
(statsDocument '((Hello Mary) (Scheme is so wonderful) (See you later) (John)))
