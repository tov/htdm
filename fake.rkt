;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname client) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f () #f)))
;;; Fake macros in ISL:

(define (if-let-sexp/icky var test-e then-e else-e)
  (list 'local
        (list (list 'define var test-e))
        (list 'cond
              (list (list 'eq? var '#false) else-e)
              (list 'else                   then-e))))

(check-expect
 (if-let-sexp/icky 'x
                   '(string->number s)
                   '(add1 x)
                   '0)
 '(local
    [(define x (string->number s))]
    (cond
      [(eq? x #false) 0]
      [else           (add1 x)])))

;;; It's nicer with quasiquote:

(define (if-let-sexp var test-e then-e else-e)
  `(local
     [(define ,var ,test-e)]
     (cond
       [(eq? ,var #false) ,else-e]
       [else              ,then-e])))

(check-expect
 (if-let-sexp 'x
              '(string->number s)
              '(add1 x)
              '0)
 '(local
    [(define x (string->number s))]
    (cond
      [(eq? x #false) 0]
      [else           (add1 x)])))

;;; It's nicer with pattern matching:

(require racket/match)

(define (if-let-sexp* stx)
  (match stx
    [`(,_ [,var ,test-e] ,then-e ,else-e)
     `(local
        [(define ,var ,test-e)]
        (cond
          [(eq? ,var #false) ,else-e]
          [else              ,then-e]))]))

(check-expect
 (if-let-sexp* '(if-let [x (string->number s)]
                        (add1 x)
                        0))
 '(local
    [(define x (string->number s))]
    (cond
      [(eq? x #false) 0]
      [else           (add1 x)])))

; (require "enum.rkt")

; (define-enum card-dir [north south east west])