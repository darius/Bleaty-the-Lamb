;; So let's try this machinery out. What would it be like to
;; program with a fancier, more practical version of it? I'm
;; hoping it'd at least be useful for:

;; 1. Specific program-synthesis puzzles, like the reverse-
;; engineering subtasks of the 2010 ICFP contest.

;; 2. Coding in a test-driven or spec-driven way but without the
;; redundant work people accept as necessary nowadays. The human still
;; has to break up the problem into 'lemmas' small enough for the
;; synthesizer. That seems like it ought to be to my taste anyway
;; because I already like programs built out of very short pieces with
;; semiformal specs, in a mostly-declarative style.

;; More likely outcome: finding out why both of these are too hard
;; with current ideas and hardware.

(include "defspec.scm")

(define cfoldr
  (lambda (f)
    (lambda (z)
      (lambda (xs)
        (foldr (lambda (x result) ((f x) result)) z xs)))))

(define (ccons hd) (lambda (tl) (cons hd tl)))

(defspec cappend (ccons cfoldr)
  (equal? '(x y z) ((cappend '()) '(x y z)))
  (equal? '(x y z) ((cappend '(x y z)) '()))
  (equal? '(x y a b) ((cappend '(x y)) '(a b)))
  )

'(defspec creverse (ccons cfoldr)
  (equal? '() (creverse '()))
  (equal? '(x y z) (creverse '(z y x)))
  )

(defspec my-list (ccons '())
  (equal? (my-list 42) '(42)))

(defspec I ()
  (-> 0 0))

(defspec compose ()
  (-> (-> 1 2)
      (-> 0 1)
      (-> 0 2)))

(defspec K ()   ;; K x y = x
  (-> 0 1 0))

(defspec S ()   ;; (((S x) y) z) = ((x z) (y z))
  (-> (-> 0 1 2)
      (-> 0 1)
      (-> 0 2)))

(defspec true ()  (-> 0 1 0))
(defspec false () (-> 0 1 1))

(defspec pair () (-> 0 1 (-> 0 1 2) 2))

(defspec zero () (-> 0 1 1))

(define (zero f) (lambda (x) x))
(define (one f) f)
(define (two f) (lambda (x) (f (f x))))
(define (add1 n) (+ n 1))
(define (represents? n f)
  (equal? n ((f add1) 0)))

(defspec succ ()
  (represents? 1 (succ zero))
  (represents? 2 (succ (succ zero)))
  (represents? 3 (succ (succ (succ zero)))))

(defspec add ()
  (represents? 0 ((add zero) zero))
  (represents? 1 ((add zero) one))
  (represents? 1 ((add one) zero))
  (represents? 2 ((add one) one))
  (represents? 2 ((add two) zero))
  (represents? 2 ((add zero) two))
  (represents? 3 ((add two) one))
  (represents? 3 ((add one) two))
  (represents? 4 ((add two) two))
  (represents? 5 ((add one) ((add two) two)))
  )

(defspec mul ()
  (represents? 0 ((mul zero) zero))
  (represents? 0 ((mul zero) one))
  (represents? 0 ((mul zero) two))
  (represents? 0 ((mul one) zero))
  (represents? 0 ((mul two) zero))
  (represents? 1 ((mul one) one))
  (represents? 2 ((mul one) two))
  (represents? 2 ((mul two) one))
  (represents? 4 ((mul two) two))
  )

(defspec pow ()
  (represents? 0 ((pow zero) one))
  (represents? 0 ((pow zero) two))
  (represents? 1 ((pow one) zero))
  (represents? 1 ((pow one) one))
  (represents? 1 ((pow one) two))
  (represents? 1 ((pow two) zero))
  (represents? 4 ((pow two) two))
  )

;; to do:
;; http://en.wikipedia.org/wiki/Lambda_calculus#Encoding_datatypes
;; zero? <=
;; and or not
;; sub1 -

;; to do: datatype definitions
;; to do: some tests using datatypes

;; to do: catch only relevant errors
;; to do: exhaustion should raise an error, not invoke an escape continuation
