##Section 3.1

#### Exercise 3.1
```
(define (make-accumulator init)
  (lambda (number)
    (set! init (+ init number))
    init))
```

#### Exercise 3.2

```
(define (make-monitored function)
  (let ((start-count 0))
    (lambda (number)
      (if (equal? number 'how-many-calls?)
          start-count
          (begin 
            (set! start-count (+ 1 start-count))
            (function number))))))
```

#### Exercise 3.3

```

(define (make-account balance init-password)
  (let ((user-password init-password))    
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
              "Insufficient funds"))
    
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define (dispatch input-password action)
      (if (equal? input-password user-password)
          (cond ((eq? action 'withdraw) withdraw)
                ((eq? action 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" action)))
          (error "wrong password")))
    
    dispatch))
```

#### Exercise 3.4

```
(define (make-account balance init-password)
  (let ((user-password init-password)
        (wrong-psword-count 0))  
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
              "Insufficient funds"))
    
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define call-the-cop
      (lambda (number)
        (display "we are calling the cops now")))
    
    (define wrong-password
      (lambda (number)
        (display "wrong password")))
    
    (define (dispatch input-password action)
      (cond ((equal? input-password user-password)
            (cond ((eq? action 'withdraw) withdraw)
                ((eq? action 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" action))))
            ((= 7 wrong-psword-count) call-the-cop)
            (else
               (begin (set! wrong-psword-count (+ 1 wrong-psword-count))
                      wrong-password))))
    
    dispatch))
```


#### Exercise 3.5

```
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (area-rec x1 y1 x2 y2)
  (* (abs (- x1 x2)) (abs (- y1 y2))))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (* (area-rec x1 y1 x2 y2) (monte-carlo trials
                                         (lambda ()
                                           (predicate (random-in-range x1 x2) (random-in-range y1 y2))))))

```


#### Exercise 3.6

```
(define rand
  (let ((init 45))
    (lambda (type)
      (cond ((equal? type 'generate)
             (random init))
            ((equal? type 'reset)
             (lambda (new-init)
               (set! init new-init)))))))

```
 
 
#### Exercise 3.7
 
```
(define (make-account balance init-password)
  (let ((user-password-list (list init-password))
        (wrong-psword-count 0))
    
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
              "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define (check-password password)
      (contains? password user-password-list))
    
    (define (contains? element lst)
      (cond ((null? lst) false)
            ((equal? element (car lst)) true)
            (else (contains? element (cdr lst)))))

    (define call-the-cop
      (lambda (number)
        (display "we are calling the cops now")))

    (define wrong-password
      (lambda (number)
        (display "wrong password")))
    
    (define (add-new-password new-password)
      (set! user-password-list (cons new-password user-password-list)))

    (define (dispatch input-password action)
      (cond ((check-password input-password)
            (cond ((eq? action 'withdraw) withdraw)
                  ((eq? action 'deposit) deposit)
                  ((eq? action 'check-password) check-password)
                  ((eq? action 'add-new-password) add-new-password)
                  (else (error "Unknown request -- MAKE-ACCOUNT" action))))
            ((= 7 wrong-psword-count) call-the-cop)
            (else
               (begin (set! wrong-psword-count (+ 1 wrong-psword-count))
                      wrong-password))))

    dispatch))


(define (make-joint account password new-password)
  (if (account password 'check-password)
      (begin ((account password 'add-new-password) new-password)
             account)
      (display "wrong password")))
      
      

(define acc (make-account 100 'pass))

(define new-acc (make-joint acc 'pass 'new))

((acc 'pass 'withdraw) 40)

((new-acc 'new 'withdraw) 20)


```

#### Exercise 3.9

![3.9](https://github.com/thejackz/sicp/blob/master/3-1/sicp-3-9.png)
 