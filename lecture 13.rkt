(define grade 85)

; (if (condition) <true> <false>)

(if (>= grade 90) "A"
    (if (>= grade 80) "B"
        (if (>=grade 70) "C"
            (if (>= grade 60) "D" "F")
         )
      )
)

; multibranch conditionals

;(cond (<c1><e1>)
;      (<c2><e2>)
;          ...
;      (<cn><en>)
;      (else <else-expression>))

(cond ((>= grade 90) "A")
      ((>= grade 80) "B")
      ((>= grade 70) "C")
      ((>= grade 60) "D")
      (else "F")
)

;lamda
;evaluate to anonymous functions
;(lambda (<formal-parameter>) <body>)

((lambda (x) (+ 1 x)) 5); anonymous

;bind the function
;(define <name>(lambda(<formal-parameter>) <body>))
(define increment(lambda(x) (+ 1 x)))
;== (define (<name> <formal-parameter>) <body>)
(define (increment2 x) (+ 1 x))


(increment 5)
(increment2 6)

;square

(define (square x) (* x x))

(square 6)

;celsius->fahrenheit

(define (celsius->fahrenheit x) (+ (* 1.8 x) 32))

(celsius->fahrenheit 0)

;cons build pairs
;list built on top of pair
(cons 1 2)

(cons 1 (cons 2 '()))

(define a (cons 1 (cons 2 (cons 3 '()))))

a
(list? a)
(car a)

(cdr a)

(cdr (cdr a))
"eq?"
;eq? compare object not value
(eq? 5 5) ;->#t same type
(eq? 2 2.0) ;#f int and double
(eq? "a" "a")

"="
;= 2 num are equal
(= 2 2)
(= 2.5 2.5)
(= 2 3)
;(= '() '()) ;error
"eqv"
;eqv? sane as eq? #t for the same primitive type
(eqv? 2 3)
(eqv? 2 2.0)
(eqv? "a" "a")

"equal" ; like eq? eqv? list, vector ...
(define x '(1 2))
(define y '(1 2))
(equal? x y)
(eqv? x y)
;factorial
;non-recursive
(define (nrfactorial n)
        (if (= n 0) 1
            (* n (nrfactorial (- n 1)))
        )
)

(nrfactorial 5)

;count element in a list
(define (count e list)
        (cond ((null? list) 0)
              ((eqv? e (car list)) (+ 1 (count e (cdr list))))
              (else (count e (cdr list))))
)

(count 4 '("hello" 2 3 4 4))
(count 3 '(2))

(define l '(1 (2 4) (2 5)))
(define k '(1))
l

(list? (car l))

(append l k)