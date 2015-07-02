

;;;2.32 
(define (subsets s)
 	(if (null? s)
 		(list nil)
 		(let ((rest (subsets (cdr s))))
 			(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;; 2.33 - 2.35 scheme

;;; 2.33

(define (mapping p sequence)
  (accumulator (lambda (x y) (cons (p x) y)) nil sequence))

(define (appending seq1 seq2)
  (accumulator cons seq2 seq1))

(define (len sequence)
  (accumulator (lambda (x y) (+ 1 y)) 0 sequence))


