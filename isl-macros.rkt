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
         ~id-concat
         (for-syntax ... ...+ id expr))


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
        head:head-pattern rhs:expr)
       #'(define head rhs)]    
      [((~or (~literal isl:define-struct)
             (~literal isl+λ:define-struct))
        name:id [field:id ...])
       #`(struct name [field ...]
           #:transparent
           #:constructor-name #,(format-id #'name "make-~a" #'name))]
      [stx #'stx])))


(define-syntax (definition-local stx)
  (syntax-parse stx
    [(_ (priv:expr ...) pub:expr ...)
     #`(splicing-local
           [#,@(map rewrite-define (syntax->list #'(priv ...)))]
         #,@(map rewrite-define (syntax->list #'(pub ...)))
         (define-values () (values)))]))

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
             (map (λ (id) (symbol->string (syntax-e id)))
                  ids)))
     cxt)))

; (id-concat name:id ...+) -> id
(define-syntax ~id-concat
  (syntax-parser
    [(_ name:id ...+)
     (id-concat/fun #'(name ...))]))


(begin-for-syntax
  ; [Syntax-of X] -> [Syntax-of X]
  (define concat-ids
    (syntax-mapper
     [((~literal ~id-concat) name:id ...+)
      (id-concat/fun #'(name ...))]
     [((~literal ~id-concat) (before:id ...) loc:id after:id ...)
      (id-concat/fun #'(before ... loc after ...) #:loc #'loc)]))

  (define macro-body
    (syntax-parser
      [(body)      (concat-ids #'body)]
      [(body ...)  (concat-ids #'(definition-group body ...))]))

  (define rest->dot
    (syntax-mapper
     [(#:rest rest) #'rest])))
  
(define-syntax (define-match-macro stx)
  (syntax-parse stx
    [(_ name:id [(_:id . args) body:expr ...+] ...+)
     (with-syntax
         ([(args ...) (rest->dot #'(args ...))])
       #'(define-syntax name
           (syntax-parser
             [(_ . args) (macro-body #'(body ...))]
             ...)))]))

; (define-macro (name:id arg ...) body:expr) -> ?
(define-simple-macro (define-macro (name:id . args) body:expr ...+)
  (define-match-macro name
    [(_ . args) body ...]))

