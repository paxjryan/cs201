#lang racket

(provide concat concat? concat-arg1 concat-arg2
         either either? either-arg1 either-arg2
         repeat repeat? repeat-arg1
         ok-string? reg-exp?
         flip pick
         generate-string-from-reg-exp
         dfa dfa? dfa-alphabet dfa-states dfa-start-state dfa-accepting-states dfa-transitions
         entry entry? entry-key entry-value
         dfa-accepts?
         cfg cfg? cfg-terminals cfg-nonterminals cfg-start-symbol cfg-rules
         rule rule? rule-lhs rule-rhs
         leaf leaf? leaf-label
         node node? node-label node-children
         list-leaf-labels
         generate-parse-tree-from-cfg generate-string-from-cfg
         dfa-mcd
         exp-mcd
         my-cfg)

; Please do not change lines above this one.

;************************************************************
; CS 201 HW #7  DUE Monday, November 29th at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name: Pax Ryan
; Email address: pax.ryan@yale.edu
;************************************************************

; Computer science topics: strings, languages, regular expressions,
; deterministic finite state acceptors and context free grammars.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The following structs are used in the representation of Regular Expressions.

(struct concat (arg1 arg2) #:transparent)
(struct either (arg1 arg2) #:transparent)
(struct repeat (arg1) #:transparent)

; A String is a list of Racket symbols.

; A Regular Expression is defined recursively as follows:
; (1) a String is a Regular Expression,
; (2) If exp1 and exp2 are Regular Expressions, then so are
; (concat exp1 exp2), (either exp1 exp2) and (repeat exp1).
; These correspond to concatenation, union ("or"), and Kleene star
; for regular expressions.

; Examples of Regular Expressions
; These are: the empty string, the string abbab, the expression ab(c|d),
; the expression (a|b)*, and the expression ((a|the)((mouse|cat)(ran|slept)))

(define exp1 '())                                ;; ""
(define exp2 '(a b b a b))                       ;; "abbab"
(define exp3 (concat '(a b) (either '(c) '(d)))) ;; "ab(c|d)" "ab[cd]"
(define exp4 (repeat (either '(a) '(b))))        ;; "(a|b)*" "[ab]*"
(define exp5 (concat (either '(a) '(the))        ;; "(a|the)(mouse|cat)(ran|slept)"
                     (concat (either '(mouse) '(cat)) 
                             (either '(ran) '(slept)))))


;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 4)

;************************************************************
; ** problem 1 ** (9 points)
; Write two procedures

; (ok-string? value)
; (reg-exp? value)

; The procedure (ok-string? value) takes an arbitrary Racket value
; and returns #t if it is a String (that is, a list of Racket symbols)
; and #f otherwise.
; The procedure (reg-exp? value) takes an arbitrary Racket value
; and returns #t if it is a Regular Expression according to the
; definition given above, and #f otherwise.

; Examples
; (ok-string? '()) => #t
; (ok-string? '(this is one)) => #t
; (ok-string? 'no) => #f
; (ok-string? '(0 1)) => #f
; (reg-exp? exp1) => #t
; (reg-exp? exp2) => #t
; (reg-exp? exp3) => #t
; (reg-exp? exp4) => #t
; (reg-exp? 'a) => #f
; (reg-exp? "abbab") => #f
; (reg-exp? '((a b))) => #f
;************************************************************

(define (ok-string? value)
  (cond
    [(not (list? value)) #f]
    [(empty? value) #t]
    [(not (symbol? (car value))) #f]
    [else (ok-string? (cdr value))]))

(define (reg-exp? value)
  (cond
    [(ok-string? value) #t]
    [(repeat? value) (reg-exp? (repeat-arg1 value))]
    [(either? value) (and (reg-exp? (either-arg1 value)) (reg-exp? (either-arg2 value)))]
    [(concat? value) (and (reg-exp? (concat-arg1 value)) (reg-exp? (concat-arg2 value)))]
    [else #f]))

;************************************************************
; ** problem 2 ** (10 points)
; Write two procedures

; (flip bias)
; (pick lst)

; For each of these procedures you should (probably) 
; use Racket's random function.
; 
; The procedure (flip bias) simulates flipping a biased coin, 
; where the bias is specified as a number between 0 and 1,
; and the result is #t with probability equal to the bias 
; and #f with probability (1 - bias).  You can test your procedure
; by making sure that 1000 calls to (flip bias) return about
; 1000*bias values of #t.

; The procedure (pick lst) takes a list lst and returns
; a randomly chosen element of lst.  If lst is empty, the
; value returned should be #f.  
; You can test it by picking 10000 times from
; a list with 10 elements, and making sure that each element is
; picked about 1000 times.

;; Note: in the test section, we set the random-seed to guarantee
;; the same set of random numbers each time.

; Examples

; (flip .5) => #t
; (flip .5) => #f
; (map (lambda (x) (flip .5)) '(1 2 3 4 5 6 7 8 9 10)) => '(#t #t #t #f #f #f #t #f #f #t)
; (map (lambda (x) (flip .1)) '(1 2 3 4 5 6 7 8 9 10)) => '(#f #f #f #f #f #f #f #f #f #f)
; (map (lambda (x) (flip .2)) '(1 2 3 4 5 6 7 8 9 10)) => '(#t #f #f #f #f #f #t #f #f #f)
; (map (lambda (x) (flip .2)) '(1 2 3 4 5 6 7 8 9 10)) => '(#f #f #f #f #f #t #f #f #f #f)

; (pick '(1 2 3 4 5 6 8 9 10)) => 8
; (pick '(1 2 3 4 5 6 8 9 10)) => 10
; (pick '(1 2 3 4 5 6 8 9 10)) => 3
; (pick '(1 2 3 4 5 6 8 9 10)) => 2

;************************************************************

(define (flip bias)
  (> bias (/ (random 100) 100)))

(define (testflip n bias)
  (cond
    [(= n 0) 0]
    [(equal? #t (flip bias)) (+ 1 (testflip (- n 1) bias))]
    [else (testflip (- n 1) bias)]))

(define (pick lst)
  (cond
    [(empty? lst) #f]
    [else (list-ref lst (random (length lst)))]))

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (generate-string-from-reg-exp exp)

; that takes a Regular Expression exp and
; returns a random element of the language
; denoted by exp.  Every string in the language
; must have a positive probability of being chosen,
; and every string not in the language must have a
; probability of 0 of being chosen.

;(define exp1 '())                                ;; ""
;(define exp2 '(a b b a b))                       ;; "abbab"
;(define exp3 (concat '(a b) (either '(c) '(d)))) ;; "ab(c|d)" "ab[cd]"
;(define exp4 (repeat (either '(a) '(b))))        ;; "(a|b)*" "[ab]*"
;(define exp5 (concat (either '(a) '(the))        ;; "(a|the)(mouse|cat)(ran|slept)"
;                     (concat (either '(mouse) '(cat)) 
;                             (either '(ran) '(slept)))))

; Examples (yours may randomly differ):
; (generate-string-from-reg-exp exp1) => '()
; (generate-string-from-reg-exp exp2) => '(a b b a b)
; (generate-string-from-reg-exp exp3) => '(a b c)
; (generate-string-from-reg-exp exp4) => '(a)
; (generate-string-from-reg-exp exp4) => '(b a)
; (generate-string-from-reg-exp exp5) => '(the cat slept)
;************************************************************

(define (generate-string-from-reg-exp exp)
  (cond
    [(ok-string? exp) exp]
    [(concat? exp) (append (generate-string-from-reg-exp (concat-arg1 exp)) (generate-string-from-reg-exp (concat-arg2 exp)))]
    [(either? exp) (if (flip 0.5) (generate-string-from-reg-exp (either-arg1 exp)) (generate-string-from-reg-exp (either-arg2 exp)))]
    [(repeat? exp) (if (flip 0.4)
                       (append (generate-string-from-reg-exp (repeat-arg1 exp)) (generate-string-from-reg-exp exp))
                       '())]
    [else #f]))

;************************************************************
; A (possibly incomplete) Deterministic Finite State Acceptor (DFA)
; is represented by the following struct.

(struct dfa (alphabet states start-state accepting-states transitions) #:transparent)

; where alphabet is a list of Racket symbols
; states is a list of Racket symbols
; start-state is one of the elements of states
; accepting-states is a list containing some of the elements of states
; and transitions is a table whose entries
;    have a key that is a list containing a state and a member of the alphabet
;         a value that is a state

(struct entry (key value) #:transparent)

; Examples of DFAs.
; Here is a DFA for the language of all strings of a's and b's with
; an even number of a's and any number of b's.

(define even-as
  (dfa
    '(a b) ;alphabet
    '(even odd) ;states
    'even ;start-state
    '(even) ;accepting-states
    (list ;transitions
     (entry '(even a) 'odd)
     (entry '(even b) 'even)
     (entry '(odd a) 'even)
     (entry '(odd b) 'odd))))

; Here is an (incomplete) DFA to accept the language of the
; regular expression c(a|d)(a|d)*r

(define car-cdr
  (dfa
   '(a c d r)
   '(start saw-c saw-a-or-d saw-r)
   'start
   '(saw-r)
   (list
    (entry '(start c) 'saw-c)
    (entry '(saw-c a) 'saw-a-or-d)
    (entry '(saw-c d) 'saw-a-or-d)
    (entry '(saw-a-or-d a) 'saw-a-or-d)
    (entry '(saw-a-or-d d) 'saw-a-or-d)
    (entry '(saw-a-or-d r) 'saw-r))))

;************************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

; (dfa-accepts? mach str)

; to take a DFA mach and a String str and determine whether the
; DFA accepts the String.

; Examples
; (dfa-accepts? even-as '(a b b a b)) => #t
; (dfa-accepts? even-as '(b b a b b b)) => #f
; (dfa-accepts? car-cdr '(c a d a r)) => #t
; (dfa-accepts? car-cdr '(c a r d)) => #f
;************************************************************

(define (find-matching-transition state alph-member transitions)
  (cond
    [(empty? transitions) #f]
    [(and (equal? state (car (entry-key (car transitions))))
          (equal? alph-member (cadr (entry-key (car transitions)))))
     (car transitions)]
    [else (find-matching-transition state alph-member (cdr transitions))]))

(define (dfa-accepts? mach str)
  (dfa-accepts?-helper mach str (dfa-start-state mach)))

(define (dfa-accepts?-helper mach str c-state)
  (cond
    [(empty? str) (if (member c-state (dfa-accepting-states mach)) #t #f)]
    [(not (find-matching-transition c-state (car str) (dfa-transitions mach))) #f]
    [else (dfa-accepts?-helper mach (cdr str) (entry-value (find-matching-transition c-state (car str) (dfa-transitions mach))))]))
     
;************************************************************
; A Context Free Grammar (CFG) is represented using the following.

(struct cfg (terminals nonterminals start-symbol rules) #:transparent)

(struct rule (lhs rhs) #:transparent)

; where
; terminals is a list of Racket symbols
; nonterminals is a list of Racket symbols
; (these two lists should have no elements in common)
; start-symbol is an element of the nonterminals list
; rules is a list of rule structs -- each of which has
; a lhs that is an element of the nonterminals list, and
; a rhs that is a list of elements from the terminals or nonterminals list


; Examples of CFGs.
; Here is the first example CFG from lecture.

(define grammar-mcd
  (cfg
   '(a the mouse cat dog it slept swam chased evaded dreamed believed that)
   '(s np vp det n pn vi vt v3)
   's
   (list
    (rule 's '(np vp))
    (rule 'np '(det n))
    (rule 'np '(pn))
    (rule 'det '(a))
    (rule 'det '(the))
    (rule 'n '(mouse))
    (rule 'n '(cat))
    (rule 'n '(dog))
    (rule 'pn '(it))
    (rule 'vp '(vi))
    (rule 'vp '(vt np))
    (rule 'vp '(v3 that s))
    (rule 'vi '(slept))
    (rule 'vi '(swam))
    (rule 'vt '(chased))
    (rule 'vt '(evaded))
    (rule 'v3 '(dreamed))
    (rule 'v3 '(believed)))))

; Here is the grammar for the set of strings consisting of
; n a's followed by n b's, for all nonnegative integers n.

(define grammar-anbn
  (cfg
   '(a b)
   '(s)
   's
   (list
    (rule 's '())
    (rule 's '(a s b)))))

;************************************************************
; A labeled Tree is defined using the following two structs.

(struct node (label children) #:transparent)
(struct leaf (label) #:transparent)

; A labeled Tree is either a leaf with a label
; or a node with a label and a list of labeled Trees as its children.

; Example of a tree.

(define tree1
  (node 'a
        (list
         (node 'b
               (list
                (leaf 'c)
                (leaf 'd)))
         (node 'e
               (list
                (node 'f
                      (list
                       (leaf 'g)
                       (leaf 'h)))
                (leaf 'i))))))

;************************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (list-leaf-labels tree)

; to return a list of the leaf labels of a labeled Tree.

; Example
; (list-leaf-labels tree1) => '(c d g h i)
;************************************************************

(define (list-leaf-labels tree)
  (cond
    [(leaf? tree) (leaf-label tree)]
    [(node? tree) (flatten (map (lambda (node) (list-leaf-labels node)) (node-children tree)))]))

;************************************************************
; ** problem 6 ** (15 points)
; Write two procedures 

; (generate-parse-tree-from-cfg grammar)
; (generate-string-from-cfg grammar)

; The procedure (generate-parse-tree-from-cfg grammar)
; takes a CFG grammar and produces a randomly chosen parse Tree from grammar,
; in which all of the node labels are nonterminal symbols and all of
; the leaf labels are terminal symbols.  Every possible such parse tree
; should have a positive probability of being generated, and every other
; parse tree should have probability 0 of being generated.

; The procedure (generate-string-from-cfg grammar)
; takes a CFG grammar and produces a randomly chosen String from grammar.
; Every String in the language of the grammar should have a non-zero
; probability of being generated, and every String not in the language
; should have probability 0 of being generated.

; (Hint: if you can generate a random parse tree, what procedure
; could you apply to generate a String from it?)

; Example (yours may randomly differ):
;> (generate-parse-tree-from-cfg grammar-mcd)
;(node
; 's
; (list
;  (node 'np (list (node 'pn (list (leaf 'it)))))
;  (node
;   'vp
;   (list
;    (node 'v3 (list (leaf 'dreamed)))
;    (leaf 'that)
;    (node
;     's
;     (list
;      (node 'np (list (node 'pn (list (leaf 'it)))))
;      (node 'vp (list (node 'vi (list (leaf 'swam)))))))))))

; (generate-string-from-cfg grammar-mcd) => '(it swam)
; (generate-string-from-cfg grammar-anbn) => '(a b)
; (generate-string-from-cfg grammar-anbn) => '(a a b b)
; (generate-string-from-cfg grammar-anbn) => '()
;************************************************************

(define (all-matching-rules label rules)
  (cond
    [(empty? rules) '()]
    [(equal? label (rule-lhs (car rules))) (cons (car rules) (all-matching-rules label (cdr rules)))]
    [else (all-matching-rules label (cdr rules))]))

(define (cfg-change-start-symb new-start-symb grammar)
  (cfg
   (cfg-terminals grammar)
   (cfg-nonterminals grammar)
   new-start-symb
   (cfg-rules grammar)))

; 1. look at the start-symbol
; 2. pick out all the rules that work for it and choose one randomly
; 3. for each element in the rhs of the rule from (2):
;     i. if it is in nonterminals, jump back to 1 for that element
;     ii. if it is in terminals, return a leaf with that label
; 4. create a node with the same label as the first element in nonterminals,
;    and a list of leaves (3ii) or nodes (3i)

(define (generate-parse-tree-from-cfg grammar)
  (let ([start-rule (pick (all-matching-rules (cfg-start-symbol grammar) (cfg-rules grammar)))])
    (node (cfg-start-symbol grammar)
          (generate-parse-subtree (rule-rhs start-rule) grammar))))

(define (generate-parse-subtree rhs grammar)
  (cond
    [(empty? rhs) '()]
    [(member (car rhs) (cfg-terminals grammar)) (cons (leaf (car rhs))
                                                      (generate-parse-subtree (cdr rhs) grammar))]
    [else (cons (generate-parse-tree-from-cfg (cfg-change-start-symb (car rhs) grammar))
                (generate-parse-subtree (cdr rhs) grammar))]))



(define (generate-string-from-cfg grammar)
  (list-leaf-labels (generate-parse-tree-from-cfg grammar)))

;************************************************************
; ** problem 7 ** (10 points)
; Define a DFA named dfa-mcd to recognize the language of 
; the CFG grammar-mcd.
;************************************************************

(define dfa-mcd
  (dfa
   '(a the mouse cat dog it slept swam chased evaded dreamed believed that)
   '(start saw-a-or-the saw-np saw-vt saw-vt-a-or-the saw-v3 end)
   'start
   '(end)
   (list
    (entry '(start a) 'saw-a-or-the)
    (entry '(start the) 'saw-a-or-the)
    (entry '(start it) 'saw-np)
    (entry '(saw-a-or-the mouse) 'saw-np)
    (entry '(saw-a-or-the cat) 'saw-np)
    (entry '(saw-a-or-the dog) 'saw-np)
    (entry '(saw-np slept) 'end)
    (entry '(saw-np swam) 'end)
    (entry '(saw-np chased) 'saw-vt)
    (entry '(saw-np evaded) 'saw-vt)
    (entry '(saw-vt a) 'saw-vt-a-or-the)
    (entry '(saw-vt the) 'saw-vt-a-or-the)
    (entry '(saw-vt it) 'end)
    (entry '(saw-vt-a-or-the mouse) 'end)
    (entry '(saw-vt-a-or-the cat) 'end)
    (entry '(saw-vt-a-or-the dog) 'end)
    (entry '(saw-np dreamed) 'saw-v3)
    (entry '(saw-np believed) 'saw-v3)
    (entry '(saw-v3 that) 'start))
   ))

;************************************************************
; ** problem 8 ** (10 points)
; Define a Regular Expression named exp-mcd to denote the language of 
; the CFG grammar-mcd.
;************************************************************

(define exp-mcd
  (concat
   (repeat
    (concat
     (either '(it)
             (concat
              (either '(a) '(the))
              (either '(mouse)
                      (either '(cat) '(dog)))))
     (concat
      (either '(dreamed) '(believed))
      '(that))))
   (concat
    (either '(it)
            (concat
             (either '(a) '(the))
             (either '(mouse)
                     (either '(cat) '(dog)))))
    (either
     (either'(slept) '(swam))
     (concat
      (either '(chased) '(evaded))
      (either '(it)
              (concat
               (either '(a) '(the))
               (either '(mouse)
                       (either '(cat) '(dog))))))))))

;(define exp-mcd-with-spacing
;  "( ( (a|the)(mouse|cat|dog) | it ) 
;     ( (dreamed|believed)that )
;   )*

;   ( ( (a|the)(mouse|cat|dog) | it ) 
;     ( (slept|swam) |
;       ( (chased|evaded)( (a|the)(mouse|cat|dog) | it ) )
;     )
;   )")
  
;************************************************************
; ** problem 9 ** (15 points)
; Define your own CFG named my-cfg of complexity at least as great as 
; that of grammar-mcd.  Give (as comments) some examples of sentences 
; generated by your grammar. 
; Note: your grammar should have a recursive element.
; (Please do more than just copy grammar-mcd with a few changes.)
;************************************************************

; Sample sentences include such one-hit wonders as:
; '(if he needs pizza then Tanya has boots)
; '(if he has boots then David wants boots because if she hates pizza then David has boots)
; '(David loves pizza)
; '(if he has children then Tanya hates children because David loves children)

; This grammar is recursive because it will sometimes create a sentence that ends with the word '(because) followed by
; a completely new sentence (see 'bc and 'ifthenbc rules)
(define my-cfg
  (cfg
   '(if he she David Tanya has wants needs hates loves pizza children boots then because)
   '(sentence simple ifthen bc ifthenbc subject verb object)
   'sentence
   (list
    (rule 'sentence '(simple))
    (rule 'sentence '(ifthen))
    (rule 'sentence '(bc))
    (rule 'sentence '(ifthenbc))
    (rule 'simple '(subject verb object))
    (rule 'ifthen '(if simple then simple))
    (rule 'bc '(simple because sentence))
    (rule 'ifthenbc '(if simple then simple because sentence))
    (rule 'subject '(he))
    (rule 'subject '(she))
    (rule 'subject '(David))
    (rule 'subject '(Tanya))
    (rule 'verb '(has))
    (rule 'verb '(wants))
    (rule 'verb '(needs))
    (rule 'verb '(loves))
    (rule 'verb '(hates))
    (rule 'object '(pizza))
    (rule 'object '(boots))
    (rule 'object '(children)))
   ))

;************************************************************



; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))

(test 'ok-string? (ok-string? '()) #t)
(test 'ok-string? (ok-string? '(this is one)) #t)
(test 'ok-string? (ok-string? 'no) #f)
(test 'ok-string? (ok-string? '(0 1)) #f)
(test 'reg-exp? (reg-exp? exp1) #t)
(test 'reg-exp? (reg-exp? exp2) #t)
(test 'reg-exp? (reg-exp? exp3) #t)
(test 'reg-exp? (reg-exp? exp4) #t)
(test 'reg-exp? (reg-exp? 'a) #f)
(test 'reg-exp? (reg-exp? "abbab") #f)
(test 'reg-exp? (reg-exp? '((a b))) #f)

;; NOTE: test results would normally vary due to random elements
;; However, by setting the random-seed, we generate the same sequence of 
;; pseudo random numbers every time.

(random-seed 100)

; (test 'flip (map (lambda (x) (flip .5)) '(1 2 3 4 5 6 7 8 9 10)) '(#t #f #t #f #f #t #t #f #t #f))
; (test 'pick (pick '(1 2 3 4 5 6 7 8 9 10)) 4)

(test 'flip-a-lot (let ((nots (length (filter not (map flip (build-list 1000 (lambda (x) 0.5))))))) (and (< 450 nots) (> 550 nots))) #t)

(test 'pick-a-lot (let ((total (apply + (map pick (build-list 1000 (lambda (x) '(1 2 3 4 5 6 7 8 9 10))))))) (and (< 5000 total) (> 6000 total))) #t)

(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp1) '())
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp2) '(a b b a b))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp3) '(a b d))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp4) '(b b a))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp4) '(b))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp5) '(the cat ran))

(test 'dfa-accepts? (dfa-accepts? even-as '(a b b a b)) #t)
(test 'dfa-accepts? (dfa-accepts? even-as '(b b a b b b)) #f)
(test 'dfa-accepts? (dfa-accepts? car-cdr '(c a d a r)) #t)
(test 'dfa-accepts? (dfa-accepts? car-cdr '(c a r d)) #f)

(test 'list-leaf-labels (list-leaf-labels tree1) '(c d g h i))

(test 'generate-parse-tree-from-cfg (generate-parse-tree-from-cfg grammar-mcd)
 (node
  's
  (list
   (node 'np (list (node 'pn (list (leaf 'it)))))
   (node 'vp (list (node 'vi (list (leaf 'swam))))))))

(test 'generate-string-from-cfg (generate-string-from-cfg grammar-mcd) '(a cat evaded it))
(test 'generate-string-from-cfg (generate-string-from-cfg grammar-anbn) '(a a a b b b))
(test 'generate-string-from-cfg (generate-string-from-cfg grammar-anbn) '(a a a b b b))
(test 'generate-string-from-cfg (generate-string-from-cfg grammar-anbn) '(a a b b))


;********************** end of hw7.rkt **********************
