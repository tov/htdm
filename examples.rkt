;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro examples

(require "isl-macros.rkt")

;;;;;
;;;;; if-let
;;;;;

; (if-let/0 [id expr] expr expr) -> expr
;
; Stepping rules:
;  - (if-let/0 [x #false] t f) => {plug #false for x in f}
;  - (if-let/0 [x v]      t f) => {plug v for x in t}  (only if v ≠ #false)
(define-macro (if-let/0 [var:id test:expr] then:expr else:expr)
  (local [(define var test)]
    (if (eq? #false var) else then)))


;; Improved:

; (if-let [id expr] expr expr) -> expr
;
; Stepping rules:
;  - (if-let/0 [x #false] t f) => f
;  - (if-let/0 [x v]      t f) => {plug v for x in t}  (only if v ≠ #false)
(define-macro (if-let [var:id test:expr] then:expr else:expr)
  (local [(define temp test)]
    (if (eq? #false temp)
        else
        (local [(define var temp)] then))))


;; Example:

; inc-str : String -> String
(define (inc-str s)
  (if-let [n (string->number s)]
          (number->string (add1 n))
          s))

(check-expect (inc-str "5")      "6")
(check-expect (inc-str "11/2")   "13/2")
(check-expect (inc-str "nine")   "nine")


;;;;;
;;;;; define-enum
;;;;;


; (define-enum id [id ...]) -> defns
;
; (define-enum name [alt ...]) defines a new enumeration with an alternative
; for each `alt`. If we consider `Name` to be the name of the new class of
; data we’re defining, then the implied data definition is:
;
;   ; A Name is one of:
;   ;  - name-alt
;   ;  ⋮  (for all `alt`s)
;
; It defines the following values:
;
;  - For each `alt`:
;      ; name:alt : Name
;      ; A unique constant value different from all others:
;      (define name:alt {a distinct, new value})
;
;      ; name:alt? : Any -> Boolean
;      ; Returns true for name-alt and false otherwise.
;      (define (name:alt? x) (eq? x name:alt))
;
;  - ; name? : Any -> Boolean
;    ; Returns true for all `Name` constants defined by this
;    ; `define-enum` and false for every other value.
;
;  - ; name=? : Name Name -> Boolean
;    ; Determines whether two `Name` values are the same.
;
; ***OPTIONAL STARTS HERE***
;
;  - ; name->string : Name -> String
;    ; Converts a `Name` to its string representation (`alt` stringified).
;
;  - ; string->name : String -> [U Name #false]
;    ; Converts a `String` to a `Name` if it matches an alterative; returns
;    ; #false otherwise.
;
;  - ; name-list : [List-of Name]
;    ; A list of all `Name` values.
;
;  - A macro for cases on `Name`s:
;      ; (name-case expr [patt expr] ...+)            -> expr
;      ; (name-case expr [patt expr] ... [else expr]) -> expr
;      ;   where
;      ;     patt is one of:
;      ;       - alt
;      ;       ⋮    (for all `alt`s)
;      ;       - (patt ...)
;      ;       - (not patt ...+)
(definition-local
  [;; `define-enum` is defined below, but first we use `definition-local` to
   ;; define some helpers:
   
   ; (or* expr ...) -> expr
   ; Like `or` but allows < 2 forms as well.
   (define-match-macro or*
     [(_)             #false]
     [(_ e:expr)      (or e #false)]
     [(_ e:expr ...)  (or e ...)])

   ; any-append : Any Any -> String
   ; Sticks any two values together as a string.
   ;  - (any-append 5 6) => "56"
   ;  - (any-append "foo" 'bar) => "foobar"
   (define (any-append a b)
     (format "~a~a" a b))
  
   ; check : X [List-of Any] [Any -> Boolean] -> X
   ; Returns `x` if `(pred? x)` is true; otherwise produces an error
   ; message using `who` to say who is complaining.
   (define (check x who pred?)
     (if (pred? x)
         x
         (error (format "error: ~a: got ~e" (foldr any-append "" who) x))))

   ; (define-one-enum id expr) -> defns
   (define-macro (define-one-enum var:id value:expr)
     (define var value)
     (define ({~id-concat var "?"} other) (eq? var other)))]

  ;; Here is the main definition of `define-enum`:
  (define-macro (define-enum name:id [alt:id ...])
    (define-struct enum [name tag])
    (define ({~id-concat name "?"} a)
      (enum? a))
    (define ({~id-concat name "=?"} a b)
      (eq? (check a '(name =?) enum?) (check b '(name =?) enum?)))
    (define-one-enum {~id-concat (name ":") alt}
      (make-enum 'name 'alt))
    ...
    ;;
    ;; Optional but fun stuff:
    ;;
    (define ({~id-concat name "->string"} a)
      (symbol->string (enum-tag (check a '(name ->string) enum?))))
    (define ({~id-concat "string->" name} s)
      (local [(define sym (string->symbol s))]
        (cond
          [(symbol=? 'alt sym) {~id-concat (name ":") alt}]
          ...
          [else #false])))
    (define {~id-concat name "-list"}
      (list {~id-concat (name ":") alt} ...))
    ;;
    ;; Quite advanced stuff:
    ;;
    (define-match-macro matches?
      [(_ have:id (~literal alt))
       (eq? have {~id-concat (name ":") alt})]
      ...
      [(_ have:id want:id)
       (syntax-error want "error: ~a-case: unknown alternative" name)]
      [(_ have:id ({~literal not} not-want (... ...+)))
       (not (matches? have (not-want (... ...))))]
      [(_ have:id (want (... ...)))
       (or* (matches? have want) (... ...))])
    (define-match-macro case/helper
      [(_ var:id
          [guard answer:expr]
          (... ...)
          [{~literal else} else-answer:expr])
       (cond
         [(matches? var guard) answer]
         (... ...)
         [else else-answer])]
      [(_ var:id
          [guard answer:expr]
          (... ...))
       (cond
         [(matches? var guard) answer]
         (... ...)
         [else (error (format "error: ~a-case: no matches" 'name))])])
    (define-macro ({~id-concat name "-case"} value:expr
                                             [guard:expr rhs:expr]
                                             (... ...+))
      (local [(define var (check value '(name -case) enum?))]
        (case/helper var [guard rhs] (... ...))))))


;; Example:

; A CardDir is one of:
;  - card-dir:north
;  - card-dir:south
;  - card-dir:east
;  - card-dir:west
(define-enum card-dir [north south east west])

(check-expect (card-dir:north? card-dir:north) #true)
(check-expect (card-dir:north? card-dir:south) #false)
(check-expect (card-dir:north? "south")        #false)

(check-expect (card-dir? card-dir:north) #true)
(check-expect (card-dir? card-dir:south) #true)
(check-expect (card-dir? "north")        #false)

(check-expect (card-dir=? card-dir:north card-dir:north) #true)
(check-expect (card-dir=? card-dir:north card-dir:south) #false)
(check-error  (card-dir=? card-dir:north "north")
              "error: card-dir=?: got \"north\"")

(check-expect (card-dir->string card-dir:east) "east")
(check-error  (card-dir->string "east")
              "error: card-dir->string: got \"east\"")

(check-expect (string->card-dir "east") card-dir:east)
(check-expect (string->card-dir "East") #false)

(check-expect card-dir-list
              (list card-dir:north card-dir:south card-dir:east card-dir:west))

(define (test-fun/1 cd)
  (card-dir-case cd
                 [north         "north"]
                 [(south west)  "south or west"]))

(check-expect (test-fun/1 card-dir:north) "north")
(check-expect (test-fun/1 card-dir:south) "south or west")
(check-error  (test-fun/1 card-dir:east)  "error: card-dir-case: no matches")
(check-error  (test-fun/1 "north")        "error: card-dir-case: got \"north\"")

(define (test-fun/2 cd)
  (card-dir-case cd
                 [(not north)   "not north"]
                 [else          "must be north"]))

(check-expect (test-fun/2 card-dir:north) "must be north")
(check-expect (test-fun/2 card-dir:south) "not north")

(check-error (card-dir-case card-dir:north [boo #true] [else #false])
             "boo: error: card-dir-case: unknown alternative")

(check-expect (format "~a" card-dir?)       "#<procedure:card-dir?>")
(check-expect (format "~a" card-dir=?)      "#<procedure:card-dir=?>")
(check-expect (format "~a" card-dir:north?) "#<procedure:card-dir:north?>")
(check-expect (format "~e" card-dir:west)   "(make-enum 'card-dir 'west)")

;;;;;
;;;;; if-let*
;;;;;


(define-match-macro if-let*/rec
  [(_ () then _)
   then]
  [(_ (first rest ...) then else)
   (if-let first
           (if-let*/rec (rest ...)
                        then
                        else)
           else)])

(define-macro (if-let* ([var:id test:expr] ...+) then:expr else:expr)
  (local
    [(define (else-fun _unused) else)]
    (if-let*/rec ([var test] ...) then (else-fun 0))))


;; Example:

; string-+ : String String -> String
(define (string-+ a b)
  (if-let* ([a (string->number a)]
            [b (string->number b)])
           (number->string (+ a b))
           (string-append a b)))

(check-expect (string-+ "4" "5") "9")
(check-expect (string-+ "4" " 5") "4 5")
(check-expect (string-+ "4 " "5") "4 5")


(define (string-negate a)
  (if-let* ([a (string->number a)]
            [a (- a)]
            [a (number->string a)])
           a
           a))

(check-expect (string-negate "5")     "-5")
(check-expect (string-negate "-5")    "5")
(check-expect (string-negate "0")     "0")
(check-expect (string-negate "hello") "hello")