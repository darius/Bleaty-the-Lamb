(include "utils.scm")

(define failure-tag (list 'failure-tag))

(define (meaning e)
  (interpret 1000 e '()))

;; Return the value of de Bruijn term E in environment R, if 
;; evaluation succeeds in at most FUEL steps. Else FAILURE-TAG.
(define (interpret fuel e r)
  (call/cc
   (lambda (exit)

     (define (fail)
       (exit failure-tag))

     (define (terp e r)
       (set! fuel (- fuel 1))
       (cond ((< fuel 0)
              (fail))
             ((number? e)
              (list-ref r e))
             ((eq? (car e) 'lambda)
              (lambda (argument)
                (terp (cadr e) (cons argument r))))
             ((eq? (car e) 'quote)
              (cadr e))
             (else
              (let ((f (terp (car e) r)))
;                (if (not (procedure? f))
;                    (fail))
                (f (terp (cadr e) r))))))

;     (trace terp)

     (terp e r))))
