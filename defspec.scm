;; A spec-syntax frontend to 'find-term'.
(define-macro (defspec name literals . specs)
  (define (expand-spec spec)
    (cond ((and (pair? spec) (eq? (car spec) '->))
           `(spec.checker ',spec))
          (else
           `(spec.checker (lambda (,name) ,spec)))))
  `(print-def ',name
              (list ,@literals)
              (conjoin (list ,@(map expand-spec specs)))))

;; Try 'find-term' and print the result.
(define (print-def name literals checker)
  (pp `(define ,name
         ,(or (find-term literals checker)
              '***))))

;; Return a smallest lambda-calculus term that passes 'checker', or
;; #f. It may use the given literals.
(define (find-term literals checker)
  (let growing ((size 1))
    (cond ((< size-limit size) #f)
          ((any checker (gen-de-bruijn-terms literals size))
           => named<-de-bruijn)
          (else (growing (+ size 1))))))


;; We'll want a macro for defining algebraic types when we have code
;; to use them:

'(deftype (list a)
   (nil)
   (cons a (list a)))

'(define-macro (deftype lhs . rhses)
   )
