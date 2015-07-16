
; Exercise 2.1

(define (make-rat n d)
  
  (define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

  (define (make-rat-reduce n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  
  (cond ((and (< n 0) (< d 0)) (make-rat-reduce (- n) (- d)))
        ((and (> n 0) (< d 0)) (make-rat-reduce (- n) (- d)))
        (else (make-rat-reduce n d))))
