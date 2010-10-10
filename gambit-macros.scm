;;; copied from github.com/darius/selfcentered/matchscheme

;; I want to use (include "foo.scm") to share code between the
;; bootstrap implementation and the self-implementation. For the
;; bootstrap, the files must also (include "gambit-macros.scm") which
;; must be a no-op when seen by the self-implementation. We tell it to
;; treat us as a nop-op by starting with a special tag expression,
;; "'magic".
'magic

;; MCASE and MLAMBDA as Gambit-Scheme macros.

(define (expand-mlambda subject clauses)
  (letrec
      ((expand-clause 
        (lambda (clause else-exp)
          (let ((pattern (car clause))
                (then-exp `(begin . ,(cdr clause)))
                (fail (gensym)))
            `(let ((,fail (lambda () ,else-exp)))
               ,(expand-pattern pattern then-exp `(,fail))))))

       (expand-pattern
        (lambda (pattern then-exp else-exp)
          (let ((test-constant
                 (lambda (constant)
                   `(if (eqv? ,subject ',constant) ,then-exp ,else-exp))))
            (cond ((eqv? pattern '_)
                   then-exp)
                  ((starts-with? 'quote pattern)
                   (test-constant (cadr pattern)))
                  ((symbol? pattern)
                   `(let ((,pattern ,subject)) ,then-exp))
                  ((starts-with? ': pattern)
                   (let ((name (cadr pattern)) (predicate (caddr pattern)))
                     `(if (,predicate ,subject)
                          (let ((,name ,subject)) ,then-exp)
                          ,else-exp)))
                  ((pair? pattern)
                   `(if (pair? ,subject)
                        (mcase (car ,subject)
                          (,(car pattern) (mcase (cdr ,subject)
                                            (,(cdr pattern) ,then-exp)
                                            (_ ,else-exp)))
                          (_ ,else-exp))
                        ,else-exp))
                  (else
                   (test-constant pattern)))))))

    (foldr expand-clause '(%match-error) clauses)))

(define (starts-with? symbol x)
  (and (pair? x) (eqv? (car x) symbol)))

(define (foldr f z xs)
  (if (eqv? '() xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))

(define (%match-error)
  (error '"Match failure"))

(define-macro (mlambda . clauses)
  (let ((param (gensym)))
    `(lambda (,param) ,(expand-mlambda param clauses))))

(define-macro (mcase subject-exp . clauses)
  `((mlambda . ,clauses) ,subject-exp))

;; Also LOCAL

(define-macro (local defns . body)
  `(let ()
     ,@defns
     ,@body))
