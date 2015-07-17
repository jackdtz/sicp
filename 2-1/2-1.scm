
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.1

; solution 1

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


; solution 2
(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
      a
      (gcd b (remainder a b))))
  
  (define (make-rat-reduce n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  
  (if (< d 0)
      (make-rat-reduce (- n) (- d))
      (make-rat-reduce n d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))


(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))

        (make-point (/ (+ (x-point start)
                          (x-point end)) 
                       2)
                    (/ (+ (y-point start)
                          (y-point end)) 
                       2))))


; Exercise 2.3

(define (make-rectangle p1 p2)
  (make-segment p1 p2))

(define (height rectangle)
  (abs (- (y-point (end-segment rectangle))
          (y-point (start-segment rectangle)))))

(define (width rectangle)
  (abs (- (x-point (end-segment rectangle))
          (x-point (start-segment rectangle)))))

(define (abs x)
    (if (> x 0)
        x
        (- x)))

(define (perimeter rectangle)
  (* 2 (+ (height rectangle)
          (width rectangle))))

(define (area rectangle)
  (* (height rectangle)
     (width rectangle)))
