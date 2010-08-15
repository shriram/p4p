#lang racket
;; REMEMBER to define AND and OR

(require (for-syntax racket syntax/stx))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [top-level #%module-begin]))

(define-for-syntax (process-sexp-stream sexp-stream)

  (define (process-fun: sexp-stream)
    (syntax-case sexp-stream () ;; _ is fun:
      [(_ arg-descriptor rest-of-stream ...)
       (let-values ([(defs*+body body-rest) 
                     (extract-definition*+expression (syntax (rest-of-stream ...)))])
         (with-syntax ([(body ...) defs*+body])
           (values (syntax (lambda arg-descriptor body ...))
                   body-rest)))]))

  (define (process-if: sexp-stream)
    ;; stx-cdr skips over the IF: keyword
    (let*-values ([(test-expr test-rest) (extract-one-expression (stx-cdr sexp-stream))]
                  [(then-expr then-rest) (extract-one-expression test-rest)])
      (let loop ([rest-stream then-rest]
                 [elifs empty])
        (syntax-case rest-stream (elif: else:)
          [(elif: more ...)
           (let*-values ([(elif-test-expr elif-test-rest) (extract-one-expression (syntax (more ...)))]
                         [(elif-then-expr elif-then-rest) (extract-one-expression elif-test-rest)])
             (loop elif-then-rest (cons (list elif-test-expr elif-then-expr) elifs)))]
          [(else: more ...)
           (let-values ([(else-expr else-rest) (extract-one-expression (syntax (more ...)))])
             (let ([clauses (cons (list test-expr then-expr) (reverse elifs))])
               (with-syntax ([((question answer) ...) clauses]
                             [else-answer else-expr])
                 (values (syntax (cond [question answer] ... [else else-answer]))
                         else-rest))))]))))
  
  (define (process-begin: sexp-stream)
    (syntax-case sexp-stream () ;; _ is begin:
      [(_ (body ...) rest-of-stream ...)
       (let ([body-exprs (process-expr-sequence (syntax (body ...)))])
         (with-syntax ([(body ...) body-exprs])
           (values (syntax (begin body ...))
                   (syntax (rest-of-stream ...)))))]))
  
  (define (process-app sexp-stream)
    (syntax-case sexp-stream ()
      [((fun+args ...) rest-of-stream ...)
       (let ([fun+args (process-expr-sequence (syntax (fun+args ...)))])
         (with-syntax ([fun (first fun+args)]
                       [(args ...) (rest fun+args)])
           (values (syntax (#%app fun args ...))
                   (syntax (rest-of-stream ...)))))]))

  (define (process-const sexp-stream)
    (values (stx-car sexp-stream)
            (stx-cdr sexp-stream)))

  ;; NOTE: process-expr-sequence doesn't return the rest of the stream
  ;; because it terminates precisely when this stream is empty.
  ;; This is because we demand that expression sequences have a delimiter,
  ;; which enables us to invoke process-expr-sequence on just the sub-stream
  ;; before the delimiter.
  (define (process-expr-sequence sexp-sub-stream)
    (if (stx-null? sexp-sub-stream)
        empty
        (let-values ([(one-exp rest-sub-stream)
                      (extract-one-expression sexp-sub-stream)])
          (cons one-exp
                (process-expr-sequence rest-sub-stream)))))

  (define (extract-one-expression sexp-stream)
    (if (stx-null? sexp-stream)
        (raise-syntax-error sexp-stream "program ended prematurely while expecting an expression")
        (syntax-case (stx-car sexp-stream) (fun: if: begin:)
          [fun:
           (process-fun: sexp-stream)]
          [if:
           (process-if: sexp-stream)]
          [begin:
            (process-begin: sexp-stream)]
          [(e ...)
           (process-app sexp-stream)]
          [v
           ;; Make sure this really is a constant! 
           ;; Not an improper list, definition, etc.
           (process-const sexp-stream)])))

  (define (process-deffun: sexp-stream)
    (syntax-case sexp-stream () ;; _ is deffun:
      [(_ (f-name . arg-descriptor) rest-of-stream ...)
       (identifier? (syntax f-name))
       (let-values ([(defs*+expr body-rest)
                     (extract-definition*+expression (syntax (rest-of-stream ...)))])
         (with-syntax ([(body ...) defs*+expr])
           (values (syntax (define (f-name . arg-descriptor) body ...))
                   body-rest)))]
      [_
       (raise-syntax-error 'deffun: "not proper syntax")]))
  
  (define (process-defvar: sexp-stream)
    (syntax-case sexp-stream () ;; _ is defvar:
      [(_ v-name rest-of-stream ...)
       (identifier? (syntax v-name))
       (let-values ([(body-expr body-rest)
                     (extract-one-expression (syntax (rest-of-stream ...)))])
         (with-syntax ([body body-expr])
           (values (syntax (define v-name body))
                   body-rest)))]
      [(_ v-name body rest-of-stream ...)
       (raise-syntax-error 'defvar: "not a variable" #'v-name)]
      [_
       (raise-syntax-error 'defvar: "not proper syntax")]))
  
  (define (process-defstruct: sexp-stream)
    (syntax-case sexp-stream () ;; _ is defstruct:
      [(_ struct-name (field ...) rest-of-stream ...)
       (and (identifier? (syntax struct-name))
            (andmap identifier? (syntax->list (syntax (field ...)))))
       (values (syntax (define-struct struct-name (field ...) #:transparent))
               (syntax (rest-of-stream ...)))]
      [_
       (raise-syntax-error 'defstruct: "not proper syntax")]))

  ;; can return false if it isn't finding a definition at the head of the stream
  (define (extract-one-definition sexp-stream)
    (if (stx-null? sexp-stream)
        (raise-syntax-error sexp-stream "program ended prematurely while expecting a definition or expression")
        (syntax-case (stx-car sexp-stream) (deffun: defvar: defstruct:)
          [deffun:
            (process-deffun: sexp-stream)]
          [defvar:
            (process-defvar: sexp-stream)]
          [defstruct:
            (process-defstruct: sexp-stream)]
          [_
           (values false sexp-stream)])))
  
  (define (extract-one-definition/expression sexp-stream)
    (if (stx-null? sexp-stream)
        (raise-syntax-error sexp-stream "program ended prematurely while expecting a definition or expression")
        (let-values ([(def def-rest)
                      (extract-one-definition sexp-stream)])
          (if def
              (values def def-rest)
              (extract-one-expression sexp-stream))))) ;; expect sexp-stream = def-rest
  
  (define (extract-definition*+expression sexp-stream)
    (define (defs-helper sexp-stream defs-so-far)
      (let-values ([(def def-rest)
                    (extract-one-definition sexp-stream)])
        (if def
            (defs-helper def-rest (cons def defs-so-far))
            (let-values ([(expr expr-rest)
                          (extract-one-expression def-rest)])
              ;; cons'ing the expr onto the defs rather than returning it separately
              (values (reverse (cons expr defs-so-far)) expr-rest)))))
    (defs-helper sexp-stream empty))

  (define (handle-stream sexp-stream past-objects)
    (if (stx-null? sexp-stream)
        (reverse past-objects)
        (let-values ([(first-thing rest-of-sexp-stream)
                      (extract-one-definition/expression sexp-stream)])
          (handle-stream rest-of-sexp-stream (cons first-thing past-objects)))))
  
  (handle-stream sexp-stream empty))

(define-syntax (top-level body-exprs)
  (syntax-case body-exprs () ;; _ is %module-begin
    [(_ bodies ...)
     (with-syntax
         ([(processed-bodies ...)
           (process-sexp-stream (syntax->list #'(bodies ...)))])
       (syntax (#%module-begin processed-bodies ...)))]))
