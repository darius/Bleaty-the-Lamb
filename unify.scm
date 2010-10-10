;; A real typechecker, not like the hack in synth.scm.
;; TODO: try it out with defspec

(include "gambit-macros.scm")

(define (typecheck term type tenv subst)
  (and subst
       (mcase term
         ((: n number?)
          (unify type (list-ref tenv n) subst))
         (('lambda subterm)
          (let ((domain (make-variable))
                (range  (make-variable)))
            (typecheck subterm range (cons domain tenv)
                       (unify type `(-> ,domain ,range) subst))))
         ((fterm xterm)
          (let ((domain (make-variable))
                (range  (make-variable)))
            (typecheck fterm `(-> ,domain ,range) tenv
                       (typecheck xterm range tenv subst)))))))

(define (make-variable)
  (gensym))


;; Following http://norvig.com/unify-bug.pdf

(define (unify x y subst)
  (cond ((not subst) #f)
        ((equal? x y) subst)
        ((var? x) (unify-variable x y subst))
        ((var? y) (unify-variable y x subst))
        (else (and (pair? x) (pair? y)
                   (unify (cdr x) (cdr y)
                          (unify (car x) (car y) subst))))))

(define (unify-variable var val subst)
  "Unify var with val, using (and possibly extending) subst."
  (cond ((eq? var val) subst)
        ((bound? var subst)
         (unify (lookup var subst) val subst))
        ((and (var? val) (bound? val subst))
         (unify var (lookup val subst) subst))
        ((occurs-in? var val subst) #f)
        (else (extend-subst var val subst))))

(define (occurs-in? var x subst)
  "Does var occur anywhere inside x?"
  (cond ((eq? var x) #t)
        ((bound? x subst)
         (occurs-in? var (lookup x subst) subst))
        ((pair? x) (or (occurs-in? var (car x) subst)
                       (occurs-in? var (cdr x) subst)))
        (else #f)))

(define (var? x)
  "Is x a variable?"
  (symbol? x))

(define (bound? x subst)
  "Is x a bound variable?"
  (assq x subst))

(define (lookup var subst)
  (cdr (assq var subst)))

(define (extend-subst var val subst)
  (cons (cons var val) subst))
