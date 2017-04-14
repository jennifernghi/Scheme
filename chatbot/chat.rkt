;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Name: Nghi Nguyen                                  ;;;
;;;SJSU ID: 010872316                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We'll use the random function implemented in Racket
;;; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;some input and output helper functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; prompt:  prompt the user for input
;;; return the input as a list of symbols
(define (prompt)
   (newline)
   (display "talk to me >>>")
   (read-line))

;;; read-line: read the user input till the eof character
;;; return the input as a list of symbols
(define (read-line)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-line)))))

;;; output: take a list such as '(how are you?) and display it
(define (output lst)
       (newline)
       (display (to-string lst))
       (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;helper methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to-string: convert a list such as '(how are you?)
;;; to the string  "how are you?"
(define (to-string lst)       
  (cond ((null? lst) "")
        ((eq? (length lst) 1) (symbol->string (car lst)))
        (else (string-append (symbol->string (car lst))
                              " "
                             (to-string (cdr lst))))))

;(in? e s):  return #t if the element e is in the set s, #f otherwise.
(define (in? e s)
        (if (null? s) #f ; #f if set null
                           (or (eqv? e (car s))   ; #t when car s eqv  w/e
                               (in? e (cdr s))))) ; if not recursive call on cdr s


;change pronoun helper
(define (change-pronoun symbol)
        (cond ((or (eqv? symbol 'I) (eqv? symbol 'i)) 'you)
              ((eqv? symbol 'am) 'are)
              ((eqv? symbol 'my) 'your)
              ((eqv? symbol 'your) 'my)
              ((eqv? symbol 'me) 'you)
              ((eqv? symbol 'you) 'me)
              )
)

;replace all pronouns in an input
(define (replace-pronoun-in-sentence input)
        (cond
              ((empty? input) input) 
              ((pronoun? (car input)) (cons (change-pronoun (car input)) (replace-pronoun-in-sentence (cdr input))))
              (else (cons (car input) (replace-pronoun-in-sentence (cdr input))))
              )
)

; is this symbol a pronoun
(define (pronoun? symbol)
        (cond ((in? symbol pronouns) #t)
              (else #f)))
;;check if input is a question
(define (question? input)
         (cond ((in? (car input) questionword) #t)
               ((eqv? '? (last-word input)) #t)
               ((last-char? input #\?) #t)
               (else #f))
)


;;extract last chracter of a symbol
(define (extract-last-character symbol)
  (cond (symbol? symbol) (last-word (string->list (symbol->string symbol))))
)

;call tail-recursive
(define (intersection s1 s2)
  (tintersection s1 s2 '())
)

;(intersection s1 s2):  return a new set that contains all the elements that appear in both sets.
(define (tintersection s1 s2 sofar)
        (cond ((empty? s1) sofar); base case: return empty set if either s1 empty
              ((in? (car s1) s2) (tintersection (cdr s1) s2 (append sofar (list (car s1))))); create a new list with elements of s1 in s2
              (else (tintersection (cdr s1) s2 sofar)))
)

;(disjoint? s1 s2): return #t if s1 and s2 have no elements in common, #f otherwise.
(define (disjoint? s1 s2)
      (if (empty? (intersection s1 s2)) #t #f); #t if intersection is empty   
)
;check a list is empty
(define (empty? s)        
       (if (null? s) #t #f) ; #t if list s null
)

;add symbol to the end of the list
(define (add-tail list symbol)
     (cond ((empty? list) (cons symbol list))
           (else (cons (car list) (add-tail (cdr list) symbol))))
)


; check if the compare-character-symbol is the last charater of an input
(define (last-char? input compare-character-symbol)
        (cond ((eqv? compare-character-symbol (last-word (string->list (symbol->string (last-word input)))) ) #t)
              (else #f)
          ) 
)

;;remove a character of a symbol
(define (remove-character symbol removed-character)
        (cond ((in? removed-character (string->list (symbol->string symbol))) (string->symbol (list->string (discard removed-character (string->list (symbol->string symbol)))))))
       
  )

;; remove the last question mark in the last word of an input
;;tail recursive
(define (remove-question-mark input sofar)
       (cond ((and (eqv? (length input) 1) (last-char? input #\?)) (add-tail sofar (remove-character (car input) #\?)))
             (else (remove-question-mark (cdr input) (add-tail  sofar (car input))))
             )
)

;; add the question mark in the last word of an input
;;tail recursive
(define (add-question-mark input sofar)
       (cond ((eqv? (length input) 1)   (add-tail sofar (string->symbol (list->string (add-tail (string->list (symbol->string (car input))) #\?)))))
             (else (add-question-mark (cdr input) (add-tail  sofar (car input))))
             )
)
;; get the last word
;; input: symbol
(define (last-word input)
        (cond 
              ((eqv? (length input) 1) (car input))
              (else (last-word (cdr input)))
        )
)
;; discard e from set s
(define (discard e s)
        (cond ((or (empty? s) (not (in? e s))) s) ; base case: s is empty or e not in s -> return s
              ((eqv? e (car s)) (discard e (cdr s))); if e == car s, keep calling on cdr s
              (else (cons (car s) (discard e (cdr s))))) ; if not; return a list by cons cars + recursive call on cdrs 
             
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;predefined lists;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic responses for rule 11
(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))


;;responses for rule 5
(define what-response '((what do you think?)
                          (why do you ask?)
                          (let me know your thoughts first!))

)
;;responses for rule 4
(define how-response    '((how would an answer to that help you?)
                          (why do you ask?))
)
;;special topic
(define special-topics    (list 'family 'friend 'friends 'job 'love 'mom 'dad 'brother 'sister 'girlfriend 'boyfriend 'children
                                'son 'daughter 'child  'wife 'husband  'home 'dog 'cat  'pet))

;;responses for rule 6
(define other-questions-response '((I don't know)
                                   (Maybe)
                                   (I have no idea)
                                   (I have no clue))

)

;verbs used for rule 10
(define verb (list 'give 'talk 'tell 'say 'draw 'make))
;question word used for rules 1,3,4,5,6
(define questionword (list 'why 'what 'how 'do 'can 'will 'would))

;helping verbs used for rule 8 
(define helpingverb (list 'think 'need 'have 'want))
;pronouns list
(define pronouns (list 'you 'are 'your 'my 'me 'I 'am))

;yes no
(define yesno (list 'yes 'no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pick one random element from the list choices
(define (pick-random choices)
  (list-ref choices (random (length choices))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  main function
;;;  usage:  (chat-with 'your-name)

(define (chat-with name)
  (output (list 'hi name))
  (chat-loop name))

;;; chat loop
(define (chat-loop name)
  (let ((input (prompt))) ; get the user input
    (if (eqv? (car input) 'bye)
        (begin
          (output (list 'bye name))
          (output (list 'have 'a 'great 'day!)))
        (begin
	  (reply input name)
          (chat-loop name)))))


;;; your task is to fill in the code for the reply function
;;; to implement rules 1 through 11 with the required priority
;;; each non-trivial rule must be implemented in a separate function
;;; define any helper functions you need below
(define (reply input name)
  (cond
     
       ((question?  input) (question-rules input name) );;rule  1, 3, 4, 5, 6
       ((special-topic? input) (special-topic-rule input name));rule2
        ((in? 'because input) (output (list (string->symbol "is that the real reason?"))));;rule 7
        ((or (eqv? (car input) 'i) (eqv? (car input) 'I)) (i-rules input));; rule 8,9, 11
        ( (in? (car input) verb) (output (list 'you (string->symbol (to-string input)))));rule 10
        (else  (output (pick-random generic-response))) ; rule 11
  )
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;rules for input start with i: 8,9 and 11;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (i-rules input)
        (cond ((eqv? 'too (last-word input))
                             (output (pick-random generic-response)));; rule 11
              ((in? (car (cdr input)) helpingverb)
                             (output  (cons (string->symbol "why do") (add-question-mark (replace-pronoun-in-sentence input) '()))));rule8
              (else (output (list (string->symbol (to-string input)) 'too)))) ;; rule 9
                    
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;rules for questions: rules 1, 3, 4, 5, 6;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (question-rules input name)
        (cond ((eqv? (car input) 'why) (output (list (string->symbol "why not?")))) ;;rule 3
        ((eqv? (car input) 'how) (output (pick-random how-response))) ;;rule 4
        ((eqv? (car input) 'what) (output (pick-random what-response))) ;;rule 5
        ((or (eqv? (car input) 'do) (eqv? (car input) 'can) (eqv? (car input) 'will) (eqv? (car input) 'would))   (yesno-rules input name)); rule 1 
        ((or (eqv? '? (last-word input)) (last-char? input #\?))
             (output (pick-random other-questions-response)) ); rule 6
        )

)
;;;;;;;;;;yesno-rules;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;yesno-rules: randomly select yes/no and apply appropricate rules for yes or no
(define (yesno-rules input name)
        (cond ((eqv? (pick-random yesno) 'yes) (yes-rules input name)); call yes-rules if yes selected
              (else (no-rules input name))); no-rules; otherwise
  )

;;yes-rules
(define (yes-rules input name)
       (output (list (list-ref yesno 0) 'i (car input))) ;; print yes i + first verb
)
;;no-rules
(define (no-rules input name)
  (cond ((last-char? input #\?) ;;if last character of the last word has a ?
                  ;;steps:
                  ;;1.remove question mark ?
                  ;;2. replace pronouns
                  ;;3. call to-string to make a string
                  ;;4. call string->symbol on the string
                  ;;print: no + i + first verb + not + symbol made by step 4
                  (output (list (list-ref yesno 1) name 'i (car input) 'not (string->symbol (to-string (replace-pronoun-in-sentence (remove-question-mark (cddr input) '())))))))
        ;;otherwise, skip step 1
        (else (output (list (list-ref yesno 1) name 'i (car input) 'not (string->symbol (to-string (replace-pronoun-in-sentence (cddr input)))))))
        )        
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;rule 2: special topics;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check if contain special-topic
(define (special-topic? input)
  (cond ((disjoint? input special-topics) #f) ; no common special-topic in the input
        (else #t)) ; if input contain any symbol that in special-topics list
)

;;rule 2;;;;;;
(define (special-topic-rule input name)
     (let ((intersection-topics (intersection input special-topics))) ; get the intersection of input and special-topics list

       (cond ((eqv? (length intersection-topics) 1) ; only 1 item in intersection
                     ; print  tell me about your + intersection
                     (output (list (string->symbol "tell me about your") (car  intersection-topics) name)))

             ;otherwisw, print tell me about your + item (randomly select from intersection)
             (else  (output (list (string->symbol "tell me about your")  (pick-random  intersection-topics) name)))
             ))  

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


