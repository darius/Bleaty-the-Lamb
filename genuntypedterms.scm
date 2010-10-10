(include "utils.scm")

;; Return a list of all closed de Bruijn terms of size exactly N,
;; which may use any of LITERALS.
(define (gen-de-bruijn-terms literals n)
  (flatmap (lambda (e) (fill-out e '()))
           (gen-closed-terms (map (lambda (lit) `',lit) literals)
                             n)))

;; In the following functions E means a template for a de Bruijn term,
;; where the symbol 'V stands for any variable (free or bound). ENV
;; means a list where the kth element is the binding for the kth de
;; Bruijn variable.

;; E = 'V
;;   | ('quote LITERAL)
;;   | ('lambda E)
;;   | (E E)

;; Return a list of all ways to instantiate the placeholders 'V in E
;; with variables from ENV.
(define (fill-out e env)
  (cond ((eq? e 'v) env)
        ((eq? (car e) 'quote) (list e))
        ((eq? (car e) 'lambda)
         (let ((env1 (cons (length env) env)))
           (map (lambda (e1) `(lambda ,e1))
                (fill-out (cadr e) env1))))
        (else
         (tensor-product list
                         (fill-out (car e) env)
                         (fill-out (cadr e) env)))))

(define (gen-closed-terms quotes n)
  (filter closed? (gen-terms quotes n)))

;; Return true iff E has no 'V.
(define (closed? e)
  (cond ((symbol? e) #f)
        ((eq? (car e) 'quote) #t)
        ((eq? (car e) 'lambda) #t)
        (else (and (closed? (car e))
                   (closed? (cadr e))))))

;; Return all E's of size exactly N, with QUOTES as possible literals.
(define (gen-terms quotes n)

  (define (gen n)
    (cond ((= n 0) '())
          ((= n 1) (cons 'v quotes))
          (else (append (gen-app n)
                        (gen-lambda n)))))

  ;; Pre: 2 <= n
  (define (gen-app n)
    (flatmap (lambda (k)
               (tensor-product list (gen k) (gen (- n k))))
             (iota 1 n)))

  ;; Pre: 1 <= n
  (define (gen-lambda n)
    (map (lambda (e) `(lambda ,e)) (gen (- n 1))))

  (gen n))
