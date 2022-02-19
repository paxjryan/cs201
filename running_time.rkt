#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
 count-compares
 make-queue)

; Please do not change lines above this one.


; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;*************************************************************

(define (lorint count bound)
  (cond
    [(<= count 0) '()]
    [else (cons (floor (random 0 bound)) (lorint (- count 1) bound))]))

(define (time-calls reps proc args)
  (time-calls-start reps proc args (current-inexact-milliseconds)))

(define (time-calls-start reps proc args start)
  (cond
    [(= reps 0) (/ (- (current-inexact-milliseconds) start) 1000)]
    [else (let ([result (apply proc args)])
            (time-calls-start (- reps 1) proc args start))]))

;************************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For the procedures length, take, and drop, replace length-runtime,
; take-runtime, and drop-runtime with either O(1) or O(n) to most accurately
; reflect each procedure's respective running time as a function of the length n
; of the list argument.

; Compare the times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest? Replace length-take-drop-ordering with an ordering of these
; procedures. Then, in the space below, explain *why* on the basis of how lists and
; their operations are implemented. (Complex statistical analysis is not
; necessary.)
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; Please report measurements here.

(define (test-proc-length k)
  (time-calls 100 length (list (lorint (* k 100000) 10))))

(define (test-proc-take-drop k proc)
  (time-calls 100 proc (list (lorint (* k 100000) 10) (* k 50000))))


(define length-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
; (test-proc-length 1)
; k = 1: 0.019028076171875
; k = 2: 0.044827880859375
; k = 3: 0.0903203125
; k = 4: 0.26962353515625
; runtime increases as k increases -> O(n)

(define take-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
; (test-proc-take-drop 1 take)
; k = 1: 0.067695068359375
; k = 2: 0.45811083984375
; k = 3: 0.72109375
; k = 4: 0.867898193359375
; rumtime increases as k increases


(define drop-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
; (test-proc-take-drop 1 drop)
; k = 1: 0.012367919921875
; k = 2: 0.030104736328125
; k = 3: 0.04756640625
; k = 4: 0.09074755859375
; runtime increases as k increases

; Please do not use commas, and please order from fastest to slowest
; e.g. (define length-reverse-powerset-ordering '(length reverse powerset))
(define length-take-drop-ordering '(drop length take))

; Please explain your ordering here.
; Drop has the fastest runtime because it only has to run through part of the list; after n elements, it can just call 'rest' which we know
; to be an O(1) operation
; Length has the second fastest runtime because it must run through the whole list, but only once, to count each element
; Take has the slowest runtime because it must run through parts of the list multiple times in order to copy the first n elements to a new list

;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
;           (compare? x y) 
;           #t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
;           (equal? x y) 
;           #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
;           (compare? x z) 
;           #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))

; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct. Replace "replace" in total-order-runtime
; with your answer.

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define total-order-runtime "O(n^3)")
; The number of permutations of n (stored in xyz-list) is n^3
; In the worst case, compare? is called on each permutation 7 times
; In the best case, compare? is called on each permutation 1 time
; But in big O notation, the constant multiples (1 and 7) doesn't matter, so we're left with O(n^3)

(define (total-order? compare? domain)
  (total-order-check-all compare? (build-xyz-list domain (build-xyz-list domain domain))))

(define (total-order-check-all compare? xyz-list)
  (cond
    [(empty? xyz-list) #t]
    [else (and (total-order-check compare? (caaar xyz-list) (second (caar xyz-list)) (second (car xyz-list)))
               (total-order-check-all compare? (cdr xyz-list)))]))

(define (total-order-check compare? x y z)
  (and
   (if (equal? x y) 
           (compare? x y) 
           #t)
   (if (and (compare? x y) (compare? y x)) 
           (equal? x y) 
           #t)
   (if (and (compare? x y)(compare? y z)) 
           (compare? x z) 
           #t)
   (or (compare? x y) (compare? y x))
   ))

(define (build-xyz-list domain lst)
  (cond
    ((null? lst) '())
    (else
     (append
      (map
       (lambda (domainelem)
         (list (car lst) domainelem))
       domain)
      (build-xyz-list domain (cdr lst))))))

;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangement"))
;'("the" "hello" "best" "arrangement")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (sorted? compare? lst)
  (cond
    [(= 1 (length lst)) #t]
    [(compare? (car lst) (cadr lst)) (sorted? compare? (cdr lst))]
    [else #f]))

(define (insert compare? item lst)
 (cond
   [(empty? lst) (append lst (list item))]
   [(compare? item (car lst)) (cons item lst)]
   [else (cons (car lst) (insert compare? item (cdr lst)))]))

(define (merge compare? lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(compare? (car lst1) (car lst2)) (cons (car lst1) (merge compare? (cdr lst1) lst2))]
    [else (cons (car lst2) (merge compare? lst1 (cdr lst2)))]))

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare? lst)
 (isort-helper compare? lst empty))

(define (isort-helper compare? lst sortedlst)
  (cond
    [(empty? lst) sortedlst]
    [else (isort-helper compare? (cdr lst) (insert compare? (car lst) sortedlst))]))

(define (msort compare? lst)
  (cond
    [(= 1 (length lst)) lst]
    [else (merge compare?
           (msort compare? (take lst (inexact->exact (ceiling (/ (length lst) 2)))))
           (msort compare? (drop lst (inexact->exact (ceiling (/ (length lst) 2))))))]))

;(require racket/trace)
;(trace msort)

;************************************************************
; ** problem 6 ** (20 points)
; (1)(a) Give empirical evidence that your implementation of insertion sort
;        (isort, above) has best case time Omega(n) and worst case time of
;        O(n^2).
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.
; (2)(a) Give empirical evidence that your implementation of merge sort
;        (msort, above) has best case and worst case times of Theta(n log n).
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.

; Be sure to use sufficiently long lists of integers and possibly repeat/average
; measurements.

; (3) Please identify inputs that give best and worst cases for your
; implementations of (a) isort and (b) msort. Be sure that you use sufficiently
; long lists of randomly chosen integers in a range larger than the length of
; the list, so that there are unlikely to be many duplicate values.

; (4) Roughly what is the longest list of random integers that your (a) isort
; procedure can sort in 10 seconds?  Same question for your (b) msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily be uniform over the whole range of feasible input lengths.
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

(define (test-sort-ordered my-sort proc k)
  (time-calls 1 my-sort (list proc (build-list k values))))

(define (test-sort-random my-sort proc k)
  (time-calls 1 my-sort (list proc (lorint k (* 100000 k)))))

; 3a) Please briefly describe best case inputs for your implementation of isort here.
; Best case input: a list of integers already sorted in ascending order that is sorted by >
; (isort will essentially reverse the list)
; e.g. '(1 2 3 4 5 ... 10000) -> (10000 ... 5 4 3 2 1)
; (test-sort-ordered isort > 10000)
; running time for k = 10000: 0.00297216796875

; 3a) Please briefly describe worst case inputs for your implementation of isort here.
; Worst case input: a list of integers already sorted in ascending order that is sorted by <
; (isort will essentially reproduce the list)
; e.g. '(1 2 3 4 5 ... 10000) -> '(1 2 3 4 5 ... 10000)
; (test-sort-ordered isort < 10000)
; running time for k = 10000: 6.636979248046875

; 1b) Please indicate average case running time of your implementation of isort here.
; (test-sort-random isort < 10000)
; average case running time for n = 10000: approx 3.32 sec
; thus, average case running time for isort is (1/2)O(n^2) ~= O(n^2)
; this is because on average, isort will have to go through half of the list to insert the next element,
; and since the worst-case list where isort goes through the whole list to insert the next element is O(n^2),
; average running time is half of that

; 3b) Please briefly describe best case inputs for your implementation of msort here.
; The input does not matter/there is no best case input. All inputs of msort will take approximately O(nlogn) time to run
; because msort will always recursively split the list in half and merge the two halves together,
; no matter what input it is given.

; 3b) Please briefly describe worst case inputs for your implementation of msort here.
; The input does not matter/there is no worst case input. All inputs of msort will take approximately O(nlogn) time to run
; because msort will always recursively split the list in half and merge the two halves together,
; no matter what input it is given.

; 2b) Please indicate average case running time of your implementation of msort here.
; All inputs of msort, including the average, will take approximately O(nlogn) time to run
; because msort will always recursively split the list in half and merge the two halves together,
; no matter what input it is given.

; 1a, 2a) Please provide evidence for the above claims here.
; I use n = 10000 for this problem.
; 1a) Best case runtime of isort is approx 0.0009 sec
; Worst case runtime of isort is approx 6.64 sec
; Best and worst case runtimes differ by a factor of ~10000 (i.e. a factor of n)
; Thus, if best case runtime is Omega(n) because isort runs through the list of length n exactly once,
; then worst case must be O(n^2)
; 2a) All inputs of msort, including the best and worst cases, will take approximately O(nlogn) time to run
; because msort will always recursively split the list in half and merge the two halves together,
; no matter what input it is given.

; 4a, 4b) Please indicate the longest list of random integers that a) your isort
; procedure can sort in 10 seconds and b) your msort procedure can sort
; in 10 seconds.
; 4a) isort: approximately length 19000
; 4b) msort: approximately length 2500000

;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************

(define count 0)
(define current-compare? equal?)

(define (count-compares sort compare? lst)
  (set! count 0)
  (set! current-compare? compare?)
  (sort compare?-wrapper lst)
  count)

(define (compare?-wrapper input1 input2)
  (set! count (add1 count))
  (current-compare? input1 input2))

;(require racket/trace)
;(trace compare?-wrapper)
  
;************************************************************
; ** problem 8 ** (10 points)

; In the Runtime lecture notes, we present a stack data structure.

(define (make-stack name (data empty))
  (let ((stack data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? stack))
        ((copy)
         (if (null? args)
             'Error:usage:copy_stack
	     (make-stack (first args) stack)))
        ((show)
	 stack)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_stack
	     (equal? stack ((first args) 'show))))
	
        ((push)
         (if (null? args)
             'Error:usage:push_element
             (begin
               (set! stack (cons (first args) stack))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? stack)
             'Error:stack-empty
	     (car stack)))
        ((pop)
         (if (null? stack)
             'Error:stack-empty
             (let ((result (car stack)))
               (set! stack (cdr stack))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))))


; Write a queue data structure, similar to the stack above.
; Whereas a stack is LIFO (last in first out), a queue is 
; FIFO = first in, first out

; Your queue data structure should implement all the same methods
; as the stack data structure.  However, push is called enqueue,
; and pop is called dequeue.  Here are examples.

; (define q1 (make-queue 'queue1))
; (q1 'name) => 'queue1
; (q1 'empty) => 'invalid-method
; (q1 'show) => '()
; (q1 'enqueue) => 'Error:usage:push_element
; (q1 'enqueue 4) => 4
; (q1 'enqueue 5) => 5
; (q1 'enqueue 6) => 6
; (q1 'peek) => 4
; (q1 'enqueue '(1 2 3)) => '(1 2 3)
; (q1 'size) => 4
; (define q2 (q1 'copy 'queue2))
; (q2 'name) => 'queue2
; (q2 'empty?) => #f
; (q2 'show) => '((1 2 3) 6 5 4)
; (q1 'equal? q2) => #t
; (q2 'equal q1) => 'invalid-method
; (q2 'equal? q1) => #t
; (q1 'equal? q1) => #t
; (q1 'dequeue) => 4
; (q1 'dequeue) => 5
; (q1 'dequeue) => 6
; (q1 'dequeue) => '(1 2 3)
; (q1 'dequeue) => 'Error:queue-empty
; (q1 'size) => 0

(define (make-queue name (data empty))
  (let ((queue data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? queue))
        ((copy)
         (if (null? args)
             'Error:usage:copy_queue
	     (make-queue (first args) queue)))
        ((show)
	 queue)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_queue
	     (equal? queue ((first args) 'show))))
	
        ((enqueue)
         (if (null? args)
             'Error:usage:push_element
             (begin
               (set! queue (cons (first args) queue))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? queue)
             'Error:queue-empty
	     (last queue)))
        ((dequeue)
         (if (null? queue)
             'Error:queue-empty
             (let ((result (last queue)))
               (set! queue (reverse (cdr (reverse queue))))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))))


;; ANSWER THIS QUESTION:
;; What is the Big-O complexity of enqueue, dequeue, size, and peek?

; enqueue O(1)
; dequeue O(n^2)s
; size O(1)
; peek O(n)


;************************************************************
; This is where the test code normally appears.
; For this assignment, write your own tests.  

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




;********* end of hw8, end of hws! **************************
