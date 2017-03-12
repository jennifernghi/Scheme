(define a 2)
(define b 3)
(if (= a 0) 0  ; if a = 0 then return 0
    (/ 1 a))   ; else return 1/a

(cond((= b 0) 0)  ; if b=0 then return 0
     ((= b 1) 1)  ; elsif b=1 then return 1
     (else (/ 1 b))) ; else return 1/b