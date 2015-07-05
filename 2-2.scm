

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


;;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulator (lambda (this-coeff higher-terms) (+ (* x higher-terms)  this-coeff))
              0
              coefficient-sequence))

;;; 2.35
(define (count-leaves tree)
  (accumulator + 0 (map (lambda (x) (/ x x)) (enumerate-tree tree))))

;;; 2.36
(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
             nil
             (cons (accumulator op initial (map (lambda (x) (car x)) sequences))
                   (accumulate-n op initial (map (lambda (x) (cdr x)) sequences)))))

;;; 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j) (list i j))  
                   (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;;; 2.41

(define (get-first pair)
  (car pair))

(define (get-second pair)
  (car (cdr pair)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-pair-sum pair)
  (list (get-first pair) (get-second pair) (+ (get-first pair) (get-second pair))))


(define (unique-triples n s)
  (map make-pair-sum
       (filter (lambda (pair) (= s (+ (get-first pair) (get-second pair))))
               (unique-pairs n))))
