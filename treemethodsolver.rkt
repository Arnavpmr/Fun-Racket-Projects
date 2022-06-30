#lang racket

(define hypothesis '("A->(B->C)" "-C->(-B&D)" "D->A|-C"))
(define conclusion "-A->(D->B)")

(define operators "|&()")
(define precedence-dict #hash(("&" . 2) ("|" . 1) ("->" . 1) ("<->" . 1) ("(" . 1)))

;;add support for more flexible variable naming like A_2 or _Hello123 rather than just letters
;;add support for biconditionals
;;make sure to handle single variable hypothesis and/or conclusion (done)


;;(A->B->(C&D)) gets turned into (-A|-B|(C&D))
;;(simplify-operators "(A&B)->C" 0 '() '())
(define (simplify-operators expression [i 0] [paranthesis-stack '()] [operand-stack '()])
  (cond [(equal? (string-length expression) i)
        expression]
        [(string-contains? "|&" (string (string-ref expression i)))
         (simplify-operators expression (+ i 1) paranthesis-stack operand-stack)]
        [(equal? "<" (string (string-ref expression i)))
         (if (equal? (string (string-ref expression (- i 1))) ")")
             (let ([neg-end (neg-end-pos expression (+ i 3))])
               (if (equal? (string-ref expression neg-end) #\()
                   (let ([par-end (par-end-pos expression neg-end)])
                     (let ([par-beg (car paranthesis-stack)])
                       (let ([neg-beg (neg-beg-pos expression (- par-beg 1))])
                         (simplify-operators (string-insert (string-remove expression neg-beg par-end)
                                                            (string-append "(" "-" (substring expression neg-beg i) "|" (substring expression (+ i 3) par-end) ")" "&"
                                                                           "(" "-" (substring expression (+ i 3) par-end) "|" (substring expression neg-beg i) ")") neg-beg)))))
                   (let ([var-end (var-end-pos expression neg-end)])
                     (let ([par-beg (car paranthesis-stack)])
                       (let ([neg-beg (neg-beg-pos expression (- par-beg 1))])
                         (simplify-operators (string-insert (string-remove expression neg-beg var-end)
                                                            (string-append "(" "-" (substring expression neg-beg i) "|" (substring expression (+ i 3) var-end) ")" "&"
                                                                           "(" "-" (substring expression (+ i 3) var-end) "|" (substring expression neg-beg i) ")") neg-beg)))))))
             (let ([neg-end (neg-end-pos expression (+ i 3))])
               (if (equal? (string-ref expression neg-end) #\()
                   (let ([par-end (par-end-pos expression neg-end)])
                     (let ([var-beg (var-beg-pos expression (- i 1))])
                       (simplify-operators (string-insert (string-remove expression var-beg par-end)
                                                          (string-append "(" "-" (substring expression var-beg i) "|" (substring expression (+ i 3) par-end) ")" "&"
                                                                         "(" "-" (substring expression (+ i 3) par-end) "|" (substring expression var-beg i) ")") var-beg))))
                   (let ([var-end (var-end-pos expression neg-end)])
                     (let ([var-beg (var-beg-pos expression (- i 1))])
                       (simplify-operators (string-insert (string-remove expression var-beg var-end)
                                                          (string-append "(" "-" (substring expression var-beg i) "|" (substring expression (+ i 3) var-end) ")" "&"
                                                                         "(" "-" (substring expression (+ i 3) var-end) "|" (substring expression var-beg i) ")") var-beg)))))))]
        [(char-alphabetic? (string-ref expression i))
         (let ([var-end (var-end-pos expression i)])
           (simplify-operators expression var-end paranthesis-stack (append (list i) operand-stack)))]
        [(equal? (string-ref expression i) #\-)
         (cond [(and (>= (- (string-length expression) (+ i 1)) 1) (equal? (string-ref expression (+ i 1)) #\>))
                (if (equal? (string-ref expression (- i 1)) #\))
                    (simplify-operators (string-insert (string-remove (string-insert expression "-" (car paranthesis-stack)) (+ i 1) (+ i 3)) "|" (+ i 1)) (+ i 2) (cdr paranthesis-stack) operand-stack)
                    (simplify-operators (string-insert (string-remove (string-insert expression "-" (car operand-stack)) (+ i 1) (+ i 3)) "|" (+ i 1)) (+ i 2) paranthesis-stack (cdr operand-stack)))]
               [(and (>= (- (string-length expression) (+ i 1)) 1) (char-alphabetic? (string-ref expression (+ i 1))))
                (let ([var-end (var-end-pos expression (+ i 1))])
                  (simplify-operators expression var-end paranthesis-stack (append (list i) operand-stack)))]
               [(and (>= (- (string-length expression) (+ i 1)) 1))
                (let ([neg-end (neg-end-pos expression i)])
                 (if (equal? (string-ref expression neg-end) #\()
                     (simplify-operators expression (+ neg-end 1) (append (list neg-end) paranthesis-stack) operand-stack)
                     (let ([var-end (var-end-pos expression neg-end)])
                       (simplify-operators expression var-end paranthesis-stack (append (list neg-end) operand-stack)))))])]
        [(equal? (string-ref expression i) #\()
         (simplify-operators expression (+ i 1) (append (list i) paranthesis-stack) operand-stack)]
        [(equal? (string-ref expression i) #\))
         (simplify-operators expression (+ i 1) paranthesis-stack operand-stack)]))

(define (par-end-pos expression i)
  (if (and (< i (string-length expression)) (not (equal? (string-ref expression i) #\))))
      (par-end-pos expression (+ i 1))
      (+ i 1)))

(define (neg-beg-pos expression i)
  (if (and (> i 0) (equal? (string-ref expression i) #\-))
      (neg-beg-pos expression (- i 1))
      (if (< i 0)
          0
          i)))

(define (var-beg-pos expression i)
  (if (and (>= i 0) (char-alphabetic? (string-ref expression i)))
      (var-beg-pos expression (- i 1))
      (if (and (>= i 0) (equal? (string-ref expression i) #\-))
          (neg-beg-pos expression (- i 1))
          (if (< i 0)
          0
          (+ i 1)))))

(define (string-insert target inserted-string pos)
  (string-append (substring target 0 pos) inserted-string (substring target pos)))

(define (string-remove target start end)
  (string-append (substring target 0 start) (substring target end)))


;;(simplify-negations "----A->---(B|--(C&--D->E|F|-G))" 0 0 '())
;;(expression-parser (generate-exp-list (simplify-negations "A->-B" 0 0 '())) '() '() "0")
(define (simplify-negations expression [i 0] [num-negs 0] [neg-list '(0)])
  (cond [(equal? (string-length expression) i)
         ""]
        [(string-contains? "|&" (string (string-ref expression i)))
         (string-append (negate-operator (string (string-ref expression i)) (string-length (reduce-negation-group num-negs))) (simplify-negations expression (+ i 1) num-negs neg-list))]
        [(char-alphabetic? (string-ref expression i))
         (let ([var-end (var-end-pos expression i)])
           (if (>= (string-length (reduce-negation-group num-negs)) 1)
               (string-append (make-string (string-length (reduce-negation-group num-negs)) #\-) (substring expression i var-end) (simplify-negations expression var-end num-negs neg-list))
               (string-append (substring expression i var-end) (simplify-negations expression var-end num-negs neg-list))))]
        [(equal? (string-ref expression i) #\-)
         (cond [(and (>= (- (string-length expression) (+ i 1)) 1) (equal? (string-ref expression (+ i 1)) #\>))
                (string-append (substring expression i (+ i 2)) (simplify-negations expression (+ i 2) num-negs neg-list))]
               [(and (>= (- (string-length expression) (+ i 1)) 1) (char-alphabetic? (string-ref expression (+ i 1))))
                (let ([var-end (var-end-pos expression (+ i 1))])
                  (if (>= (string-length (reduce-negation-group (+ num-negs 1))) 1)
                      (string-append (make-string (string-length (reduce-negation-group (+ num-negs 1))) #\-) (substring expression (+ i 1) var-end) (simplify-negations expression var-end num-negs neg-list))
                      (string-append (substring expression (+ i 1) var-end) (simplify-negations expression var-end num-negs neg-list))))]
               [(and (>= (- (string-length expression) (+ i 1)) 1))
                (let ([neg-end (neg-end-pos expression i)])
                 (if (equal? (string-ref expression neg-end) #\()
                     (string-append "(" (simplify-negations expression (+ neg-end 1) (+ num-negs (string-length (reduce-negation-group (- neg-end i)))) (append (list (string-length (reduce-negation-group (- neg-end i)))) neg-list)))
                     (let ([var-end (var-end-pos expression neg-end)])
                       (string-append (reduce-negation-group (+ (- neg-end i) num-negs)) (substring expression neg-end var-end) (simplify-negations expression var-end num-negs neg-list)))))])]
        [(equal? (string-ref expression i) #\()
         (string-append "(" (simplify-negations expression (+ i 1) num-negs (append (list num-negs) neg-list)))]
        [(equal? (string-ref expression i) #\))
         (string-append ")" (simplify-negations expression (+ i 1) (cadr neg-list) (cdr neg-list)))]))

(define (neg-end-pos expression i)
  (if (and (< i (string-length expression)) (equal? (string-ref expression i) #\-))
      (neg-end-pos expression (+ i 1))
      i))

(define (reduce-negation-group num-negations)
  (if (equal? (modulo num-negations 2) 1)
      "-"
      ""))

(define (negate-operator operator num-negations)
  (if (equal? operator "&")
      (if (equal? num-negations 0)
          "&"
          "|")
      (if (equal? operator "|")
          (if (equal? num-negations 0)
              "|"
              "&")
          "")))


;;generate list from expression to make it easier for the infix parsing algo
;;everything is evaluated from right to left by default
(define (generate-exp-list expression)
  (cond [(equal? expression "")
         '()]
        [(string-contains? operators (string (string-ref expression 0)))
         (append (list (string (string-ref expression 0))) (generate-exp-list (substring expression 1)))]
        [(equal? (string-ref expression 0) #\-)
         (cond [(and (>= (string-length expression) 2) (equal? (string-ref expression 1) #\>))
                (append (list (substring expression 0 2)) (generate-exp-list (substring expression 2)))]
               [(and (>= (string-length expression) 2) (char-alphabetic? (string-ref expression 1)))
                (let ([var-end (var-end-pos expression 1)])
                 (append (list (substring expression 0 var-end)) (generate-exp-list (substring expression var-end))))])]
        [(char-alphabetic? (string-ref expression 0))
         (let ([var-end (var-end-pos expression 0)])
           (append (list (substring expression 0 var-end)) (generate-exp-list (substring expression var-end))))])
)

;;helper for the generate expression function
(define (var-end-pos expression i)
  (if (and (< i (string-length expression)) (char-alphabetic? (string-ref expression i)))
      (var-end-pos expression (+ i 1))
      i))

;;(expression-parser '("A" "->" "(" "B" "->" "C" ")") '() '() "0")
;;(expression-parser (generate-exp-list "A&B|C") '() '() "0")
;;converts expression from '("A" "->" "(" "B" "->" "C" ")") to '(("|" "-B" "C") ("|" "-A" "0")) ------- & has higher precedence than |
(define (expression-parser expression-list [operand-stack '()] [operator-stack '()] [operation-id "0"])
  (if (or (element? expression-list ">") (element? expression-list "&") (element? expression-list "|"))
      (expression-parser-helper expression-list '() '() "0")
      (if (> (length expression-list) 1)
          (list (list (list-ref (make-set expression-list) 1)))
          (list expression-list))))


(define (expression-parser-helper expression-list operand-stack operator-stack operation-id)
  (cond [(equal? expression-list '())
         (if (> (length operator-stack) 0)
             (expression-parser-empty-queue operand-stack operator-stack operation-id)
             '())]
        [(string-alphabetic? (car expression-list))
         (expression-parser-helper (cdr expression-list) (append (list (car expression-list)) operand-stack) operator-stack operation-id)]
        [(string-contains? "&|->" (car expression-list))
         (if (equal? operator-stack '())
             (expression-parser-helper (cdr expression-list) operand-stack (append operator-stack (list (car expression-list))) operation-id)
             (if (>= (dict-ref precedence-dict (car expression-list)) (dict-ref precedence-dict (car operator-stack)))
                 (expression-parser-helper (cdr expression-list) operand-stack (append (list (car expression-list)) operator-stack) operation-id)
                 (expression-parser-less-precedence expression-list operand-stack operator-stack operation-id)))]
        [(equal? (car expression-list) "(")
         (expression-parser-helper (cdr expression-list) operand-stack (append (list (car expression-list)) operator-stack) operation-id)]
        [(equal? (car expression-list) ")")
         (expression-parser-rightparanthesis (cdr expression-list) operand-stack operator-stack operation-id)]))

(define (expression-parser-rightparanthesis expression-list operand-stack operator-stack operation-id)
  (if (equal? (car operator-stack) "->")
      (if (equal? (string-ref (list-ref operand-stack 1) 0) #\-)
          (append (list (list "|" (substring (list-ref operand-stack 1) 1) (car operand-stack))) (expression-parser-rightparanthesis expression-list (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1))))
          (append (list (list "|" (string-append "-" (list-ref operand-stack 1)) (car operand-stack))) (expression-parser-rightparanthesis expression-list (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1)))))
      (if (equal? (car operator-stack) "(")
          (expression-parser-helper expression-list operand-stack (cdr operator-stack) operation-id)
          (append (list (list (car operator-stack) (list-ref operand-stack 1) (car operand-stack))) (expression-parser-rightparanthesis expression-list (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1)))))))

(define (expression-parser-empty-queue operand-stack operator-stack operation-id)
  (if (equal? operator-stack '())
      '()
      (if (equal? (car operator-stack) "->")
          (if (equal? (string-ref (list-ref operand-stack 1) 0) #\-)
              (append (list (list "|" (substring (list-ref operand-stack 1) 1) (car operand-stack))) (expression-parser-empty-queue (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1))))
              (append (list (list "|" (string-append "-" (list-ref operand-stack 1)) (car operand-stack))) (expression-parser-empty-queue (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1)))))
          (append (list (list (car operator-stack) (list-ref operand-stack 1) (car operand-stack))) (expression-parser-empty-queue (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1)))))))

(define (expression-parser-less-precedence expression-list operand-stack operator-stack operation-id)
  (if (equal? operator-stack '())
      (expression-parser-helper expression-list operand-stack operator-stack operation-id)
      (if (>= (dict-ref precedence-dict (car expression-list)) (dict-ref precedence-dict (car operator-stack)))
          (expression-parser-helper expression-list operand-stack operator-stack operation-id)
          (append (list (list (car operator-stack) (list-ref operand-stack 1) (car operand-stack))) (expression-parser-less-precedence expression-list (append (list operation-id) (drop operand-stack 2)) (cdr operator-stack) (number->string (+ (string->number operation-id) 1)))))))

(define (string-alphabetic? word) ;;checks if element is variable (any combination of letters as well as -(any combination of letters))
  (if (= (string-length word) 1)
      (char-alphabetic? (string-ref word 0))
      (if (or (char-alphabetic? (string-ref word 0)) (equal? (string-ref word 0) #\-))
          (string-alphabetic? (substring word 1))
          #f)))

(define (negate-expression expression)
  (string-append "-(" expression ")"))

(define (negate-format-exp expression)
  (expression-parser (generate-exp-list (simplify-negations (simplify-operators (negate-expression (string-replace expression " " "")))))))

(define (format-exp expression)
  (expression-parser (generate-exp-list (simplify-negations (simplify-operators (string-replace expression " " ""))))))

;;(tree-method-evaluator hypothesis-statements hypothesis-list conclusion-statement conclusion-list on-second-operand ancestors current-node)
;;(tree-method-conclusion-evaluator "-A->(D->B)" '("A->(B->C)" "D->A|-C" "-C->(-B&D)"))
;;(tree-method-evaluator '("A->(B->C)" "D->A|-C" "-C->(-B&D)") '("-A->(D->B)"))
(define (tree-method-evaluator hypothesis-statements [hypothesis-list '()] [on-second-and-operand #f] [ancestors '()] [current-node ""])
  (cond [(not (equal? current-node ""))
         (if (string-alphabetic? current-node)
             (if (element? ancestors (simplify-negations (string-append "-" current-node)))
                 '()
                 (cond [(not (equal? hypothesis-statements '()))
                        (let ([prefix-exp (last (format-exp (car hypothesis-statements)))])
                          (if (equal? (car prefix-exp) "&")
                              (if (equal? on-second-and-operand #f)
                                  (tree-method-evaluator hypothesis-statements hypothesis-list #t (append ancestors (list current-node)) (list-ref prefix-exp 1))
                                  (tree-method-evaluator hypothesis-statements (drop-right hypothesis-list 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 2)))
                              (if (equal? (car prefix-exp) "|")
                                  (append (tree-method-evaluator (cdr hypothesis-statements) (drop-right (format-exp (car hypothesis-statements)) 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 1)) (tree-method-evaluator (cdr hypothesis-statements) (drop-right (format-exp (car hypothesis-statements)) 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 2)))
                                  (tree-method-evaluator (cdr hypothesis-statements) hypothesis-list #f (append ancestors (list current-node)) (car prefix-exp)))))]
                       [(equal? on-second-and-operand #t)
                        (let ([prefix-exp (last hypothesis-list)])
                          (tree-method-evaluator hypothesis-statements (drop-right hypothesis-list 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 2)))]
                       [else
                        (list (append ancestors (list current-node)))]))
                 (cond [(not (equal? hypothesis-list '()))
                        (let ([prefix-exp (list-ref hypothesis-list (string->number current-node))])
                          (if (equal? (car prefix-exp) "&")
                              (if (equal? on-second-and-operand #f)
                                  (tree-method-evaluator hypothesis-statements hypothesis-list #t ancestors (list-ref prefix-exp 1))
                                  (tree-method-evaluator hypothesis-statements (drop-right hypothesis-list 1) #f ancestors (list-ref prefix-exp 2)))
                              (append (tree-method-evaluator hypothesis-statements (drop-right hypothesis-list 1) #f ancestors (list-ref prefix-exp 1)) (tree-method-evaluator hypothesis-statements (drop-right hypothesis-list 1) #f ancestors (list-ref prefix-exp 2)))))]
                       [(not (equal? hypothesis-statements '()))
                        (tree-method-evaluator (cdr hypothesis-statements) (format-exp (car hypothesis-statements)) #f ancestors current-node)]))]))

(define (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements [conclusion-list (negate-format-exp conclusion-statement)] [on-second-operand #f] [ancestors '()] [current-node ""])
 (cond [(equal? conclusion-list '())
        (tree-method-evaluator hypothesis-statements '() #f ancestors current-node)]
       [(equal? current-node "")
        (let ([prefix-exp (last conclusion-list)])
          (if (equal? (car prefix-exp) "&")
              (if (equal? on-second-operand #f)
                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements conclusion-list #t ancestors (list-ref prefix-exp 1))
                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 2)))
              (if (equal? (car prefix-exp) "|")
                  (append (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f ancestors (list-ref prefix-exp 1)) (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f ancestors (list-ref prefix-exp 2)))
                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f ancestors (car prefix-exp)))))]
       [(not (equal? current-node ""))
         (if (string-alphabetic? current-node)
             (if (element? ancestors (simplify-negations (string-append "-" current-node)))
                 '()
                 (cond [(not (equal? conclusion-list '()))
                        (let ([prefix-exp (last conclusion-list)])
                          (if (equal? (car prefix-exp) "&")
                              (if (equal? on-second-operand #f)
                                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements conclusion-list #t (append ancestors (list current-node)) (list-ref prefix-exp 1))
                                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 2)))
                              (append (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 1)) (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f (append ancestors (list current-node)) (list-ref prefix-exp 2)))))]))
                 (cond [(not (equal? conclusion-list '()))
                        (let ([prefix-exp (list-ref conclusion-list (string->number current-node))])
                          (if (equal? (car prefix-exp) "&")
                              (if (equal? on-second-operand #f)
                                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements conclusion-list #t ancestors (list-ref prefix-exp 1))
                                  (tree-method-conclusion-evaluator conclusion-statement hypothesis-statements (drop-right conclusion-list 1) #f ancestors (list-ref prefix-exp 2)))
                              (append (tree-method-evaluator hypothesis-statements (drop-right conclusion-list 1) #f ancestors (list-ref prefix-exp 1)) (tree-method-evaluator hypothesis-statements (drop-right conclusion-list 1) #f ancestors (list-ref prefix-exp 2)))))]))]))


;;these were pulled from one of the PROVIDED helper functions from a lab (relations 2c pt 2)
(define (element? S e)
  (cond [(null? S) #f]
        [(equal? e (car S)) #t]
        [else (element? (cdr S) e)]))

(define (make-set L)
  (cond [(null? L) '()]
        [(member (car L) (cdr L)) (make-set (cdr L))]
        [else (cons (car L) (make-set (cdr L)))]))


;;------------------------------------------------------------------------------------------------
;(with-handlers ([exn:fail? (lambda (exn)
;                             (displayln (exn-message exn))
 ;                            "Cannot parse a malformed expression!")])
  ;(make-set (map make-set (tree-method-conclusion-evaluator "Abc<->-(B&C)" '("Abc" "B" "C")))))

;;(make-set (map make-set (tree-method-conclusion-evaluator "Abc<->-(B&C)" '("Abc" "B" "C"))))
;;(make-set (map make-set (tree-method-conclusion-evaluator "A&B" '("A<->B"))))

;;Ex
;;Remove all spaces and convert conclusion expression to list

;;pop a hypothesis from the list
;;A->(B->C)
;;Remove all spaces and convert expression to list '("A" "->" "(" "B" "->" "C" ")")
;;Run the infix parser on it and return another list '(("|" "-B" "C") ("|" "-A" 0))
;;Recurse through list and pop out first element, then spawn two new calls with -A and 0 as well as cdr of list
;; if a number is detected as the node val, then you can spawn new calls until you see only letters

;;Base cases
;; if there is -[Letter] or letter in a node value, check if it exists higher up
;;--------if it doesn't, then check if the hypothesis list is empty
;;-------------if it is, then return the list of node values as a possible case
;;--------if it isn't pop out a new hypothesis and repeat the whole process again! ;)

