## 2.3 Symbolic Data


#### Exercise 2.53

```
(list 'a 'b 'c) => (a b c)

(list (list 'george)) => ((george))

(cdr '((x1 x2) (y1 y2))) => ((y1 y2))

(cadr '((x1 x2) (y1 y2))) => (x1 x2)

(pair? (car '(a short list))) => false


(memq 'red '((red shoes) (blue socks))) => false

(memq 'red '(red shoes blue socks)) => (red shoes blue socks)
```



#### Exercise 2.54

```


(define (equal? list-1 list-2)
  (cond ((and (null? list-1) (null? list-2)) #t)
        ((or (null? list-1) (null? list-2)) #f)
        ((and (not (pair? list-1)) (not (pair? list-2)) 
              (eq? list-1 list-2))
         #t)
        ((and (pair? list-1) (pair? list-2)
              (equal? (car list-1) (car list-2))
              (equal? (cdr list-1) (cdr list-2)))
        #t)
        (else #f)))


```


#### Exercise 2.55

The first quotation mark will treat the second one as a literal, so 

```''abracadabra```  will be 

```(quote abracadabra)```

So ```(car ''abracadabra)``` will return ```quote```


#### Exercise 2.56

```

((define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (make-product (base exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "Unknown expression type -- DERIV" exp))))


(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

(define (make-sum exp-1 exp-2)
  (cond ((=number? exp-1 0) exp-2)
        ((=number? exp-2 0) exp-1)
        ((and (number? exp-1) (number? exp-2)) (+ exp-1 exp-2))
        (else (list '+ exp-1 exp-2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (addend exp)
  (cadr exp))

(define (augend exp)
  (caddr exp))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (multiplier exp)
  (cadr exp))

(define (multiplicand exp)
  (caddr exp))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exponentiation)
  (cadr exponentiation))

(define (exponent exponentiation)
  (caddr exponentiation))

(define (make-exponentiation base exponent)
  (cond ((and (number? exponent) (= 1 exponent)) base)
        ((and (number? exponent) (= 0 exponent)) 1)
        (else (list '** base exponent))))
  

```


#### Exercise 2.57


We only need to change two functions

```

(define (multiplicand exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (make-product (caddr exp)
                    (cadddr exp))))


(define (augend exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (make-sum (caddr exp)
                (cadddr exp))))


```


#### Exercise 2.58

#### a.


Changes for sum:

```
(define (sum? exp)
  (and (pair? exp) (eq? (cadr exp) '+)))

(define (addend exp)
  (car exp))

(define (augend exp)
  (caddr exp))

(define (make-sum exp-1 exp-2)
  (cond ((=number? exp-1 0) exp-2)
        ((=number? exp-2 0) exp-1)
        ((and (number? exp-1) (number? exp-2)) (+ exp-1 exp-2))
        (else (list exp-1 '+ exp-2))))

```


Changes for product:

```

(define (product? exp)
  (and (pair? exp) (eq? (cadr exp) '*)))

(define (multiplier exp)
  (car exp))

(define (multiplicand exp)
  (caddr exp))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

```


#### b.

```
(define (deriv exp var)
  (let ((flat-exp (flatten exp))) 
    (cond ((number? flat-exp) 0)
          ((variable? flat-exp)
           (if (same-variable? flat-exp var) 1 0))
          ((sum? flat-exp)
           (make-sum (deriv (addend flat-exp) var)
                     (deriv (augend flat-exp) var)))
          ((product? flat-exp)
           (make-sum 
            (make-product (multiplier flat-exp)
                          (deriv (multiplicand flat-exp) var))
            (make-product (multiplicand flat-exp)
                          (deriv (multiplier flat-exp) var))))
          ((exponentiation? flat-exp)
           (make-product
            (make-product (base flat-exp)
                          (make-exponentiation (base flat-exp)
                                               (- (exponent flat-exp) 1)))
            (deriv (base flat-exp) var)))
          (else
           (error "Unknown expression type -- DERIV" flat-exp)))))

(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (prefix op exp)
  (if (eq? (car exp) op)
      '()
      (cons (car exp) (prefix op (cdr exp)))))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (singleton? lst)
  (= 1 (length lst)))

(define (contains? item x)
  (cond ((null? x) #f)
        ((atom? x) #f)
        ((eq? item (car x)) #t)
        (else (contains? item (cdr x)))))

(define (postfix op exp)
  (if (eq? (car exp) op)
      (cdr exp)
      (postfix op (cdr exp))))

(define (lowest-precedence exp)
  (cond ((contains? '+ exp) '+)
        ((contains? '* exp) '*)
        (else nil)))
        

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((number? a1) (append (list a1 '+) a2))
        ((number? a2) (append a1 (list '+ a2)))
        (else (append a1 (cons'+ a2)))))

(define (make-product m1 m2)
  (let ((op-m1 (lowest-precedence m1))
        (op-m2 (lowest-precedence m2)))
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          ((and (number? m1) (eq? op-m2 '*))
           (append (list m1 '*) m2))
          ((and (number? m2) (eq? op-m1 '*))
           (append m1 (list '* m2)))
          ((and (eq? op-m1 '*) (eq? op-m2 '*))
           (append m1 (cons '* m2)))
          ((eq? op-m1 '*) (append m1 (list '* m2)))
          ((eq? op-m2 '*) (append (list m1 '*) m2))
          (else (list m1 '* m2)))))

(define (addend exp)
  (let ((term (prefix '+ exp)))
    (if (singleton? term)
        (car term)
        term)))

(define (augend exp)
  (let ((term (postfix '+ exp)))
    (if (singleton? term)
        (car term)
        term)))


(define (multiplier exp)
  (let ((term (prefix '* exp)))
    (if (singleton? term)
        (car term)
        term)))

(define (multiplicand exp)
  (let ((term (postfix '* exp)))
    (if (singleton? term)
        (car term)
        term)))

(define (sum? x)
  (eq? (lowest-precedence x) '+))

(define (product? x)
  (eq? (lowest-precedence x) '*))


(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exponentiation)
  (cadr exponentiation))

(define (exponent exponentiation)
  (caddr exponentiation))

(define (make-exponentiation base exponent)
  (cond ((and (number? exponent) (= 1 exponent)) base)
        ((and (number? exponent) (= 0 exponent)) 1)
        (else (list '** base exponent))))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (strip lst)
  (if (or (null? lst) (atom? lst) (not (null? (cdr lst))))
      lst
      (strip (car lst))))

(define (flatten lst)
  (cond ((or (null? lst) (atom? lst))
         lst)
        ((null? (strip (car lst)))
         (flatten (cdr lst)))
        (else
         (cons (flatten (strip (car lst))) (flatten (cdr lst))))))

```

#### Exercise 2.59

```
(define (union-set set-1 set-2)
  (cond ((null? set-1) set-2)
        ((null? set-2) set-1)
        ((not (element-of-set? (car set-1) set-2))
         (cons (car set-1) (union-set (cdr set-1) set-2)))
        (else (union-set (cdr set-1) set-2))))

```
#### Exercise 2.61
```
(define (adjoin-set element set)
  (cond ((null? set) (list element))
        ((< element (car set))
         (cons element set))
        (else (cons (car set) (adjoin-set element (cdr set))))))

```

#### Exercise 2.62

```
(define (union-set set-1 set-2)
  (cond ((null? set-1) set-2)
        ((null? set-2) set-1)
        ((< (car set-1) (car set-2))
         (cons (car set-1) (union-set (cdr set-1) set-2)))
        (else (union-set (cdr set-1) set-2))))
```

#### Exercise 2.63

a. They have the same result
b. 1: ```n*log(n)```  2: linear-time

#### Exercise 2.64

This is my own implementation of list-to-tree convertion, which I think more clearer compared against the one in the book

```
(define (length-of-list lst)
  (define (helper lst start-count)
    (if (null? lst)
        start-count
        (helper (cdr lst) (+ 1 start-count))))
  (helper lst 0))

(define (extract-left lst size)
  (define (helper lst size start-count)
    (if (= size start-count)
        nil
        (cons (car lst) (helper (cdr lst) size (+ 1 start-count)))))
  (if (or (null? lst) (= 0 size))
      nil
      (helper lst size 0)))

(define (extract-right lst size)
  (define (helper lst size start-count)
    (if (= size start-count)
        lst
        (helper (cdr lst) size (+ 1 start-count))))
  (if (or (null? lst) (= 0 size))
      nil
      (helper lst (- (length-of-list lst) size) 0)))

(define (extract-mid-point lst)
  (define (helper lst size start-count)
          (if (= size start-count)
              (car lst)
              (helper (cdr lst) size (+ 1 start-count))))
  (if (null? lst)
      nil
      (let ((length (length-of-list lst)))
        (helper lst (quotient length 2) 0))))
       
(define (list-to-tree lst)
  (cond ((null? lst) nil)
        ((= 1 (length-of-list lst)) (make-tree (car lst) nil nil))
        (else
         (let ((length (length-of-list lst)))
           (let ((left-size (quotient length 2)))
             (let ((right-size (- length left-size 1)))    
               (let ((left-result (list-to-tree (extract-left lst left-size)))
                     (right-result (list-to-tree (extract-right lst right-size))))
                 (make-tree (extract-mid-point lst)
                            left-result
                           right-result))))))))

(extract-left '(1 2 3) 1)
(extract-right '(1 2 3) 1)
(extract-mid-point '(1 2 3))      
(list-to-tree '(1 2 3))

```

#### Exercise 2.65

```
(define (intersection-set-listformat set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-listformat (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-listformat (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-listformat set1 (cdr set2))))))) 

(define (union-set-listformat set-1 set-2)
  (cond ((null? set-1) set-2)
        ((null? set-2) set-1)
        ((< (car set-1) (car set-2))
         (cons (car set-1) (union-set-listformat (cdr set-1) set-2)))
        (else (union-set-listformat (cdr set-1) set-2))))


(define (intersection-set set-1 set-2)
  (let ((list-1 (tree-to-list-2 set-1))
        (list-2 (tree-to-list-2 set-2)))
    (list-to-tree (intersection-set-listformat list-1 list-2))))

(define (union-set set-1 set-2)
  (let ((list-1 (tree-to-list-2 set-1))
        (list-2 (tree-to-list-2 set-2)))
    (list-to-tree (union-set-listformat list-1 list-2))))

```


#### Exercise 2.66

```
(define (lookup given-key set-of-record)
  (cond ((null? set-of-record) false)
        ((equal? given-key (entry set-of-record))
         (entry set-of-record))
        ((< given-key (entry set-of-record))
         (lookup given-key (left-branch set-of-record)))
        ((> given-key (entry set-of-record))
         (lookup given-key (right-branch set-of-record)))))

```


### Huffman encoding

**This is my own implementation of huffman encoding without looking at the book. It includes how to parse a message and break it into a set of sorted pairs(base on freq) of symbol and frequency**

```

#lang planet neil/sicp



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                 set                    ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first
  (lambda (lst)
    (car lst)))

(define second
  (lambda (lst)
    (cadr lst)))

(define length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))

(define element-of-set?
  (lambda (element set)
    (cond ((null? set) false)
          ((equal? (car (first set)) element) true)
          (else (element-of-set? element (cdr set))))))

(define add-to-set
  (lambda (element set)
    (if (element-of-set? element set)
        set
        (cons (list element 1) set))))

(define get-right-half-helper
  (lambda (lst size start-count)
    (if (= size start-count)
        lst
        (get-right-half-helper (cdr lst) size (+ 1 start-count)))))

(define get-right-half
  (lambda (lst)
    (let ((len (length lst)))
      (cond ((null? lst) nil)
            (else (get-right-half-helper lst (quotient len 2) 0))))))

(define get-left-half
  (lambda (lst)
    (let ((len (length lst)))
      (cond ((null? lst) nil)
            (else (get-left-half-helper lst (quotient len 2) 0))))))

(define get-left-half-helper
  (lambda (lst size start-count)
    (if (= size start-count)
        nil
        (cons (car lst) (get-left-half-helper (cdr lst) size (+ 1 start-count))))))

(define add-1-freq
  (lambda (symbol set)
    (if (equal? symbol (first (car set)))
        (cons (list symbol (+ 1 (second (car set))))
              (cdr set))
        (cons (car set) (add-1-freq symbol (cdr set))))))


(define merge
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((> (second (first l1)) (second (first l2)))
           (cons (car l1) (merge (cdr l1) l2)))
          (else
           (cons (car l2) (merge l1 (cdr l2)))))))

(define sort-pair-list
  (lambda (set)
      (cond ((null? set) nil)
            ((null? (cdr set)) set)
            (else
             (let ((left (get-left-half set))
                  (right (get-right-half set)))
              (merge (sort-pair-list left)
                     (sort-pair-list right)))))))

(define reverse
  (lambda (collection)
    (if (null? collection)
        nil
        (append (reverse (cdr collection))
                (list (car collection))))))

;;;;;;;;;;;;;;;;;;;;;;

(define make-leaf
  (lambda (symbol weight)
    (list 'leaf symbol weight)))

(define symbol-of-leaf
  (lambda (leaf)
    (cadr leaf)))

(define weight-of-leaf
  (lambda (leaf)
    (caddr leaf)))

(define leaf?
  (lambda (node)
    (equal? 'leaf (car node))))

(define make-code-tree
  (lambda (left right)
    (list left 
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right)))))

(define symbols
  (lambda (node)
    (if (leaf? node)
        (list (symbol-of-leaf node))
        (caddr node))))

(define weight
  (lambda (node)
    (if (leaf? node)
        (weight-of-leaf node)
        (cadddr node))))

(define parse-message
  (lambda (message)
    (define (helper message set)
      (cond ((null? message) set) 
            ((element-of-set? (car message) set)
             (helper (cdr message) (add-1-freq (car message) set)))
            (else
             (helper (cdr message) (add-to-set (car message) set)))))
    (sort-pair-list (helper message nil))))

(define make-leaf-set
  (lambda (set)
    (map (lambda (pair) (make-leaf (first pair)
                                   (second pair)))
         set)))



(make-leaf-set (reverse (parse-message '(T H I S A T E S T M E S S A G E))))

```

#### Exercise 2.67

```
{a d a b b c a}

```

#### Exercise 2.68

```
(define (element-of-list? element lst)
  (cond ((null? lst) false)
        ((equal? element (car lst)) true)
        (else (element-of-list? element (cdr lst)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define encode-symbol
  (lambda (symbol tree)
    (if (or (null? tree) (leaf? tree)) 
        nil
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (cond ((element-of-list? symbol  (symbols left))
                 (cons 0 (encode-symbol symbol left)))
                ((element-of-list? symbol (symbols right))
                 (cons 1 (encode-symbol symbol right))))))))

```

#### Exercise 2.69

```
(define generate-tree-from-set
  (lambda (set)
    (if (null? (cdr set))
        (car set)
        (let ((first (car set))
              (second (cadr set))
              (rest (cddr set)))
          (generate-tree-from-set (adjoin-set (make-code-tree first second) 
                                              rest))))))

(define generate-huffman-tree-from-message
  (lambda (message)
    (generate-tree-from-set (reverse (make-leaf-set (parse-message message))))))

```


