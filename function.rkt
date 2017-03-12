(define increment (lambda(x) (+ 1 x)))
(define CSgrades '(100 80 90))

(define averageList (lambda (grades)   (/ (apply + grades)  (length grades))))  ; list
(define averageNumber (lambda grades (/ (apply + grades) (length grades))))
