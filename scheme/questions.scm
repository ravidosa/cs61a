(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  (define (f s idx)
    (if (null? s)
      nil
      (cons (list idx (car s)) (f (cdr s) (+ idx 1)))
    )
  )
  (f s 0)
)

;; Problem 16

;; Merge two lists S1 and S2 according to ORDERED? and return
;; the merged lists.
(define (merge ordered? s1 s2)
  (cond
    ((and (null? s1) (null? s2)) nil)
    ((null? s1) (cons (car s2) (merge ordered? s1 (cdr s2))))
    ((null? s2) (cons (car s1) (merge ordered? (cdr s1) s2)))
    ((ordered? (car s1) (car s2)) (cons (car s1) (merge ordered? (cdr s1) s2)))
    (else (cons (car s2) (merge ordered? s1 (cdr s2))))
  )
)

;; Optional Problem 2

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons (map let-to-lambda params) (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (append (cons (cons 'lambda (cons (car (zip values)) (map let-to-lambda body))) nil) (map let-to-lambda (cadr (zip values))))
           ))
        (else
         (cons (car expr) (map let-to-lambda (cdr expr)))
         )))

; Some utility functions that you may find useful to implement for let-to-lambda

(define (zip pairs)
  (list (map car pairs) (map cadr pairs))
)
