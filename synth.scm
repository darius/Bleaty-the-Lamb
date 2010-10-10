(include "utils.scm")

(define (synthesize spec)
  (let growing ((size 1))
    (cond ((< size-limit size) #f)
          ((synthesize-sized size spec) => named<-de-bruijn)
          (else (growing (+ size 1))))))

(define (synthesize-sized size spec)
  (any (spec.checker spec) (gen-de-bruijn-terms '() size)))

(define size-limit 9)

(define (spec.checker spec)
  (cond ((procedure? spec)
         (lambda (e)
           (let ((value (maybe-error (lambda () (meaning e)))))
             (and value
                  (not (eq? value failure-tag))
                  (maybe-error (lambda () (spec value)))))))
        ((and (pair? spec) (eq? (car spec) '->))
         (let ((type (parse-type spec)))
           (lambda (e) (type-checks? type e))))
        ((pair? spec)
         (let ((checkers (map spec.checker spec)))
           (lambda (e)
             (all (lambda (checker) (checker e)) checkers))))
        (else
         (error "Unknown spec type" spec))))

(define (parse-type spec)
  (cond ((number? spec) spec)
        ((and (pair? spec) (eq? (car spec) '->))
         (let ((types (map parse-type (cdr spec))))
           (if (< (length types) 2)
               (error "Bad type syntax" spec))
           (foldr (lambda (type1 type2) `(-> ,type1 ,type2))
                  (car (reverse types))
                  (reverse (cdr (reverse types))))))
        (else
         (error "Unknown spec type" spec))))

(define (named<-de-bruijn e)
  (let cvt ((e e) (r '()))
    (cond ((number? e)
           (list-ref r e))
          ((eq? (car e) 'quote)
           e)
          ((eq? (car e) 'lambda)
           (let ((x (vector-ref '#(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                                (length r))))
             `(lambda (,x)
                ,(cvt (cadr e) (cons x r)))))
          (else
           (list (cvt (car e) r)
                 (cvt (cadr e) r))))))

(define (type-checks? type e)
  (maybe-error (lambda ()
                 (let ((value (meaning e)))
                   (and (not (eq? value failure-tag))
                        (checks? type value))))))

(define (checks? type value)
  (if (number? type)
      (equal? type value)
      (and (procedure? value)
           (let ((domain (cadr type))
                 (range (caddr type)))
             (checks? range
                      (value (make-instance domain)))))))

(define (make-instance type)
  (if (number? type)
      type
      (let ((domain (cadr type))
            (range (caddr type)))
        (lambda (argument)
          (if (not (checks? domain argument))
              (error 'argument-type-error)
              (make-instance range))))))

(define (expect expected-result thunk)
  (maybe-error (lambda () (equal? expected-result (thunk)))))

(define (maybe-error thunk)
  (with-exception-catcher (lambda (exc) #f) thunk))
