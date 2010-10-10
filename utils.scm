(define (tensor-product f xs ys)
  (flatmap (lambda (x)
             (map (lambda (y) (f x y))
                  ys))
           xs))

(define (iota lo hibound)
  (if (<= hibound lo)
      '()
      (cons lo (iota (+ lo 1) hibound))))

(define (flatmap f xs)
  (flatten (map f xs)))

(define (flatten xss)
  (foldr append '() xss))

(define (filter ok? xs)
  (foldr (lambda (x oks)
           (if (ok? x) (cons x oks) oks))
         '()
         xs))

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs)
         (foldr f z (cdr xs)))))

(define (any ok? xs)
  (cond ((null? xs) #f)
        ((ok? (car xs)) (car xs))
        (else (any ok? (cdr xs)))))

(define (all ok? xs)
  (cond ((null? xs) #t)
        ((ok? (car xs)) (all ok? (cdr xs)))
        (else #f)))

(define (conjoin predicates)
  (lambda (x) (all (lambda (ok?) (ok? x))
                   predicates)))
