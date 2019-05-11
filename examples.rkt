;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro examples

(require "isl-macros.rkt")


(define-macro (if-let/0 [var:id test:expr] then:expr else:expr)
  (local [(define var test)]
    (if (eq? #false var) else then)))


(define (truthy? v)
  (or (not (boolean? v)) v))

(define-macro (if-let [var:id test:expr] then:expr else:expr)
  (local [(define temp test)]
    (if (truthy? temp)
        (local [(define var temp)] then)
        else)))

(define (inc-str s)
  (if-let [n (string->number s)]
          (number->string (add1 n))
          s))

(check-expect (inc-str "5")      "6")
(check-expect (inc-str "11/2")   "13/2")
(check-expect (inc-str "nine")   "nine")


(define-match-macro if-let*/helper
  [(_ () then:expr _)
   then]
  [(_ ([var0:id test0:expr] #:rest rest) then:expr else-fun:id)
   (if-let [var0 test0]
           (if-let*/helper rest then else-fun)
           (else-fun 0))])

(define-macro (if-let* ([var:id test:expr] ...+) then:expr else:expr)
  (local
    [(define (else-fun _dummy) else)]
    (if-let*/helper ([var test] ...) then else-fun)))

(define (string-+ a b)
  (if-let* ([a (string->number a)]
            [b (string->number b)])
    (number->string (+ a b))
    (string-append a b)))

(check-expect (string-+ "4" "5") "9")
(check-expect (string-+ "4" " 5") "4 5")
(check-expect (string-+ "4 " "5") "4 5")


(define-macro (define-enum/helper name [alt ...])
  (definition-local
    [(define-struct {~id-concat name -enum} [tag])]
    (define {~id-concat name ?} {~id-concat name -enum?})
    (define ({~id-concat name =?} x y)
      (symbol=? ({~id-concat name -enum-tag} x)
                ({~id-concat name -enum-tag} y)))
    (define {~id-concat (name -) alt}
      ({~id-concat (make-) name -enum} 'alt))
    ...
    (define ({~id-concat (name -) alt ?} x)
      (eq? x {~id-concat (name -) alt}))
    ...))

(define-macro (define-enum name:id [alt1:id alt2:id alts:id ...])
  (define-enum/helper name [alt1 alt2 alts ...]))


(define-enum card-dir [north south east west])

(check-expect (card-dir-north? card-dir-north) #true)
(check-expect (card-dir-north? card-dir-south) #false)

(check-expect (card-dir? card-dir-north) #true)
(check-expect (card-dir? card-dir-south) #true)
(check-expect (card-dir? "north") #false)

(check-expect (card-dir=? card-dir-north card-dir-north) #true)
(check-expect (card-dir=? card-dir-north card-dir-south) #false)
(check-error (card-dir=? card-dir-north 8))
