#lang racket/base


(require syntax/parse/define
         racket/splicing
         (prefix-in isl:   lang/htdp-intermediate)
         (prefix-in isl+λ: lang/htdp-intermediate-lambda))
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/parse/define))

(provide define-macro
         define-match-macro
         definition-group
         definition-local
         syntax-error
         ~id-concat
         (for-syntax ... ...+ id expr number ~literal))

(define-syntax syntax-error
  (syntax-parser
    [(_ cxt:expr msg:str param:id ...)
     (raise-syntax-error #f
                         (apply format
                                (map syntax-e (syntax->list #'(msg param ...))))
                         #'cxt)]))

(begin-for-syntax
  ; id-concat/fun : [Syntax-of Any] [NE-Syntax-List-of Symbol]
  ;                 -> [Syntax-of Symbol]
  (define (id-concat/fun ids-stx #:loc [cxt0 #false])
    (define ids (syntax-e ids-stx))
    (define cxt (or cxt0 (car ids)))
    (datum->syntax
     cxt
     (string->symbol
      (apply string-append
             (map (λ (id) (format "~a" (syntax-e id)))
                  ids)))
     cxt)))

; (id-concat name:id ...+) -> id
(define-syntax ~id-concat
  (syntax-parser
    [(_ name:id ...+)
     (id-concat/fun #'(name ...))]))


; (definition-group body:expr ...) -> expr
(define-simple-macro (definition-group body:expr ...)
  (splicing-let ()
    body ...
    (define-values () (values))))


(begin-for-syntax
  (define-simple-macro (syntax-mapper [patt rhs ...+] ...)
    (let ()
      (define (loop stx)
        (syntax-parse stx
          [patt rhs ...] ...
          [(first . rest)
           (datum->syntax stx (cons (loop #'first) (loop #'rest)) stx)]
          [_ stx]))
      loop))
  
  (define-syntax-class head-pattern
    (pattern _:id)
    (pattern (_:id _:id ...)))

  (define rewrite-define
    (syntax-parser
      [((~or (~literal isl:define)
             (~literal isl+λ:define))
        head:head-pattern rhs)
       #'(define head rhs)]    
      [((~or (~literal isl:define-struct)
             (~literal isl+λ:define-struct))
        name:id [field:id ...])
       #'(struct name [field ...]
           #:transparent
           #:constructor-name (~id-concat ("make-") name)
           #:methods gen:custom-write
           [(define (write-proc struct port mode)
              (parameterize ([current-output-port port])
                (printf "(make-" 'name)
                (printf " ~e" ((~id-concat name "-" field) struct))
                ...
                (display #\))))])]
      [stx #'stx])))


(define-syntax (definition-local stx)
  (syntax-parse stx
    [(_ (priv:expr ...) pub:expr ...)
     #`(splicing-local
           [#,@(map rewrite-define (syntax->list #'(priv ...)))]
         #,@(map rewrite-define (syntax->list #'(pub ...)))
         (define-values () (values)))]))



(begin-for-syntax
  (define-syntax-class id-or-str
    (pattern _:id)
    (pattern _:str))

  (define-splicing-syntax-class opt-literals
    (pattern (~seq #:literals param))
    (pattern (~seq) #:with param #'()))
           ;  #:attr attr #'(#:literals (lit ...))

    
          ;   #:attr attr #'(#:literals ())

    
  ; [Syntax-of X] -> [Syntax-of X]
  (define concat-ids
    (syntax-mapper
     [((~literal ~id-concat) loc:id-or-str after:id-or-str ...)
      (id-concat/fun #'(loc after ...) #:loc #'loc)]
     [((~literal ~id-concat) (before:id-or-str ...)
                             loc:id-or-str
                             after:id-or-str ...)
      (id-concat/fun #'(before ... loc after ...) #:loc #'loc)]
     [((~and loc (~literal ~id-concat)) (part:id-or-str ...+))
      (id-concat/fun #'(part ...) #:loc #'loc)]))

  (define macro-body
    (syntax-parser
      [(body)      (concat-ids #'body)]
      [(body ...)  (concat-ids #'(definition-group body ...))]))

  (define rest->dot
    (syntax-mapper
     [(#:rest rest) #'rest])))

(define-syntax (define-match-macro stx)
  (syntax-parse stx
    [(_ name:id lits:opt-literals
        [(_:id . args) body ...+]
        ...+)
     (with-syntax
         ([(args ...) (rest->dot #'(args ...))])
       #'(define-syntax name
           (syntax-parser #:literals lits.param
             [(_ . args) (macro-body #'(body ...))]
             ...)))]))

; (define-macro (name:id arg ...) body) -> ?
(define-simple-macro (define-macro (name:id . args) body ...+)
  (define-match-macro name [(_ . args) body ...]))
