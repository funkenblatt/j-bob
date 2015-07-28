#lang racket

(module j-bob-support racket
  (provide (rename-out [jbob/car car]
                       [jbob/cdr cdr]
                       [jbob/+ +]
                       [jbob/< <])
           num if/nil atom equal natp)
  
  (define (num x) (if (number? x) x 0))
  (define (if/nil Q A E)
    (if (equal? Q 'nil) (E) (A)))

  (define (atom x) (if (pair? x) 'nil 't))
  (define (jbob/car x) (if (pair? x) (car x) '()))
  (define (jbob/cdr x) (if (pair? x) (cdr x) '()))
  (define (equal x y) (if (equal? x y) 't 'nil))
  (define (natp x)
    (if (integer? x) (if (< x 0) 'nil 't) 'nil))
  (define (jbob/+ x y) (+ (num x) (num y)))
  (define (jbob/< x y)
    (if (< (num x) (num y)) 't 'nil)))

(module j-bob-macros racket
  (require (submod ".." j-bob-support))
  (provide (all-defined-out))

  (define-syntax if
    (syntax-rules ()
      ((_ Q A E)
       (if/nil Q (lambda () A) (lambda () E)))))

  (define-syntax defun
    (syntax-rules ()
      ((_ name (arg ...) body)
       (define (name arg ...) body))))

  (define-syntax dethm
    (syntax-rules ()
      ((_ name (arg ...) body)
       (define (name arg ...) body))))

  (defun size (x)
    (if (atom x)
        '0
        (+ '1 (+ (size (car x)) (size (cdr x)))))))
