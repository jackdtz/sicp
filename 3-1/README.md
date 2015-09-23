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


#### Exercise 3.10

After expand the ```let``` expression, we get the following function:

```
(define (make-withdraw init-amount)
  (lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient amount")) init-amount))

```

When we call ```(define w1 (make-withdraw 100))```, it creates an environment E1, whose enclosing environment is the global environment, and E1 bounds init-amount to 100, and E1 also creates a unnamed procedure object, the lambda function whose parameter is balance, and the body is ```(lambda (amount) <body>)```. The unnamed procedure object will be called right after E1 gets created. Its formal parameter is ```balance```, and its environment E2, who bounds balance to init-amount.

![3.9](https://github.com/thejackz/sicp/blob/master/3-1/sicp-3-10.png)

#### Exercise 3.12

The first ```append``` will create a new list which is referred by z, so ```cdr x``` will return ```(b)```. The second ```append!```, however, attach ```y``` to the end of ```x```, so ```x``` and ```w``` are referring to the same list, which means ```cdr x``` returns ```(b c d)``` 


#### Exercise 3.13

The last pointer of the list will point to the beginning of the list, so we have a list whose end is pointing to its beginning.

If we try ```last-point``` on such list, we will never get an answer, since such list does not have an end.

#### Exercise 3.14

This mystery function is used to reverse a list.

#### Exercise 3.16
```
(define a (cons 1 2))
(define b (cons 1 2))
(define c (cons a b))

(bad-count a b)
=> 3
```

```
(define a (cons 1 2))
(define b (cons 1 2))
(define c (cons a b))
(set-car! b a)

(bad-count c)
=> 4
```


```
(define a (cons 1 2))
(define b (cons a a))
(define c (cons b b))

(bad-count c)
=> 7

```

#### Exercise 3.17
```
(define (count-pairs x)
  (let ((table '()))
    (define (traverse x)
      (cond ((null? x) 0)
            ((not (pair? x)) 0)
            ((memq x table) 0)
            (else
             (begin (set! table (cons x table))
                    (+ (traverse (car x))
                       (traverse (cdr x))
                       1)))))
    (traverse x)))
```

#### Exercise 3.18
```
(define (cycle? lst)
  (let ((table '()))
    (define (traverse lst)
      (cond ((or (null? lst) (not (pair? lst))) false)
            ((memq (car lst) table) true)
            (else (begin
                    (set! table (cons (car lst) table))
                    (traverse (cdr lst))))))
    (traverse lst)))
```

#### Exercise 3.19
```
(define (cycle? lst) 
  (cond ((or (null? lst) (null? (cdr lst))) false)
        (else
         (let ((slow lst)
               (fast (cddr lst)))
           (define (loop)
             (cond ((null? fast) false)
                   ((eq? fast slow) true)
                   (else (begin (set! slow (cdr slow))
                                (set! fast (cddr fast))
                                (loop)))))
           (loop)))))
```


#### Exercise 2.21

```
(define singleton?
  (lambda (queue)
    (null? (cdr (front-ptr queue)))))

(define delete-queue
  (lambda (queue)
    (cond ((empty-queue? queue)
           (error "Empty queue"))
          ((singleton? queue)
           (begin (set-front-ptr! queue nil)
                  (set-rear-ptr! queue nil)
                  queue))
          (else
           (begin
             (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))))
             
(define print-queue
  (lambda (queue)
    (display (front-ptr queue))))

```

The reason why ben was getting such result, is that the delete-queue function did not update the rear-ptr pointer. When there is only one element left in the queue, which is pointed by both front-ptr and rear-ptr, we still need to make rear-ptr point to nil in order to get the correct result.


#### Exercise 2.22

```
(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))

    (define (empty?)
        (null? front-ptr))

    (define (singleton?)
      (null? (cdr front-ptr)))

    (define set-front-ptr!
      (lambda (pair)
        (set! front-ptr pair)))

    (define set-rear-ptr!
      (lambda (pair)
        (set! rear-ptr pair)))

    (define (front-queue)
      (if (empty?)
          (error "empty queue")
          (car front-ptr)))

    (define enqueue
      (lambda (item)
        (let ((new-pair (cons item nil)))
          (cond ((empty?)
                 (begin (set! front-ptr new-pair)
                        (set! rear-ptr new-pair)))
                (else
                 (begin
                   (set-cdr! rear-ptr new-pair)
                   (set-rear-ptr! new-pair)))))))
      
      (define print-queue
        (lambda ()
          (begin (display (list front-ptr rear-ptr))
                 (display "\n"))))
        
    (define dequeue
      (lambda ()
        (cond ((empty?) (error "empty queue"))
              ((singleton?)
               (begin (set-front-ptr! nil)
                      (set-rear-ptr! nil)))
              (else
               (set-front-ptr! (cdr front-ptr))))))
      
    (define dispatch
      (lambda (action)
        (cond ((eq? action 'enqueue) enqueue)
              ((eq? action 'print-queue) (print-queue))
              ((eq? action 'front-queue) (front-queue))
              ((eq? action 'dequeue) (dequeue))
              (else "error action" action))))

    dispatch))


(define queue (make-queue))

((queue 'enqueue) 3)

((queue 'enqueue) 4)
((queue 'enqueue) 5)
((queue 'enqueue) 6)

(queue 'print-queue)

(queue 'front-queue)
(queue 'dequeue)
(queue 'dequeue)
(queue 'dequeue)

(queue 'print-queue)


```
