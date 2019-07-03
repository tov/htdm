#lang racket/base


(require syntax/parse/define
         racket/provide
         racket/splicing
         (prefix-in isl:   lang/htdp-intermediate)
         (prefix-in isl+λ: lang/htdp-intermediate-lambda))
(require (for-syntax racket/base
                     racket/format
                     racket/syntax
                     syntax/parse
                     syntax/parse/define))

(provide define-macro
         define-match-macro
         definition-group
         definition-local
         syntax-error
         ~id-concat
         ~@ ~@@
         (for-syntax
          (matching-identifiers-out #rx"^~" (all-from-out syntax/parse))
          ... ...+ id expr number))

(define-syntax ~@@ (syntax-rules ()))

(define-syntax syntax-error
  (syntax-parser
    [(_ cxt:expr msg:str param:id ...)
     (raise-syntax-error #f
                         (apply format
                                (map syntax-e (syntax->list #'(msg param ...))))
                         #'cxt)]))


; (id-concat name:id ...+) -> id
(define-syntax ~id-concat (syntax-rules ()))


; (definition-group body:expr ...) -> expr
(define-simple-macro (definition-group body:expr ...)
  (splicing-let ()
    body ...
    (define-values () (values))))


(begin-for-syntax
  (define syntax-deep-map
    (case-lambda
      [(visitor stx)
       (let loop ([stx stx])
         (cond
           [(visitor stx) => loop]
           [else
            (syntax-parse stx
              [(first . rest)
               (datum->syntax stx (cons (loop #'first) (loop #'rest)) stx stx)]
              [_ stx])]))]
      [(visitor)
       (λ (stx) (syntax-deep-map visitor stx))]))
  
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
  (define-syntax-class segment
    (pattern :id)
    (pattern :str))

  (define-splicing-syntax-class opt-literals
    (pattern (~seq #:literals param))
    (pattern (~seq) #:with param #'()))

  
  ; id-concat/fun : syntax? [syntax-list-of identifier?] srcloc? -> identifer?
  (define (id-concat/fun cxt ids-stx srcloc)
    (datum->syntax
     cxt
     (string->symbol (apply ~a (syntax->datum ids-stx)))
     srcloc))

  (define (parse-~id-concat stx)
    (syntax-parse stx
      [(_ (pre:segment ...) src-id:segment post:segment ...)
       (id-concat/fun #'src-id #'(pre ... src-id post ...) stx)]
      [(_ src-id:segment post:segment ...)
       (id-concat/fun #'src-id #'(src-id post ...) stx)]
      [(_ (only:segment ...+))
       (id-concat/fun stx #'(only ...) stx)]))
  
  ; [Syntax-of X] -> [Syntax-of X]
  (define concat-ids
    (syntax-deep-map
     (syntax-parser
       #:literals (~id-concat)
       [(~id-concat ~! . _)
        (parse-~id-concat this-syntax)]
       [_ #f])))

  (define process-~@@
    (syntax-deep-map
     (syntax-parser
       #:literals (~@@)
       [((~@@ form) . rest)
        #'((~@ . form) . rest)]
       [_ #f])))

  (define macro-body
    (syntax-parser
      [(body)      (process-~@@ (concat-ids #'body))]
      [(body ...)  (process-~@@ (concat-ids #'(definition-group body ...)))])))

(define-syntax (define-match-macro stx)
  (syntax-parse stx
    [(_ name:id lits:opt-literals
        [(_:id . args) body ...+]
        ...+)
     #'(define-syntax name
         (syntax-parser
           #:literals lits.param
           [(_ . args) (macro-body #'(body ...))]
           ...))]))

; (define-macro (name:id arg ...) body) -> ?
(define-simple-macro (define-macro (name:id . args) body ...+)
  (define-match-macro name [(_ . args) body ...]))
