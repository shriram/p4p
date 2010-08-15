#lang racket
;; Add AND and OR
;; Does not handle '(a b c) correctly
;; defstruct: memo (a b) works, because commas are not enforced after the first id;
;;   this will affect function headers too; check for function calls
;; One source of syntactic confusion:
;;   f(x)(y)(z)
;;   is that f(x) followed by (y)(z), or f followed by (x)(y)
;;   The solution to this is to change the syntax for function-position-is-an-expression
;;   This happens uncommonly (function as a parameter doesn't pose a problem; let-binding also works around it)
;;     that the syntax doesn't need to be convenient
;; This produces an internal error:
;;   3(4)
;;   syntax-e: expects argument of type <syntax>; given '()
;; This produces an internal error:
;;   if: false 3
;;   ?: bad syntax in: ()

(require (for-syntax racket syntax/stx))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [top-level #%module-begin]))

(define-for-syntax (process-sexp-stream sexp-stream)
  
  ;; NOTE: This function doesn't return the rest of the stream.  
  ;; See explanation for process-expr-sequence.
  (define (process-comma-separated-identifiers sexp-stream)
    (define (comma-helper sexp-stream)
      (if (stx-null? sexp-stream)
          empty
          (syntax-case sexp-stream (unquote)
            [((unquote id) other-things ...)
             (identifier? (syntax id))
             (cons (syntax id) (comma-helper (syntax (other-things ...))))]
            [((unquote something) other-things ...)
             (raise-syntax-error 'comma-separated-list "expected to find an identifier" (syntax something))]
            [(v other-things ...)
             (identifier? (syntax v))
             (cons (syntax v) (comma-helper (syntax (other-things ...))))]
            [(v other-things ...)
             (raise-syntax-error 'comma-separated-list "expected to find an identifier" (syntax v))])))
    (comma-helper sexp-stream))

  (define (process-fun: sexp-stream)
    (syntax-case sexp-stream (in)
      [(fun:-kwd argdesc in rest-of-stream ...)
       (let-values ([(defs*+body body-rest) 
                     (extract-definition*+expression (syntax (rest-of-stream ...)))])
         (with-syntax ([(body ...) defs*+body])
           ;; NOTE: this conditional is repeated in process-deffun:, but with different RHSes
           (cond
             [(identifier? (syntax argdesc))
              (values (syntax (lambda argdesc body ...))
                      body-rest)]
             [(stx-list? (syntax argdesc))
              (with-syntax ([(args ...) (process-comma-separated-identifiers (syntax argdesc))])
                (values (syntax (lambda (args ...) body ...))
                        body-rest))]
             [else
              (raise-syntax-error 'argument-list "not valid syntax" (syntax argdesc))])))]))

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
  
  (define (process-do: sexp-stream)
    (syntax-case sexp-stream () ;; _ is do:
      [(_ (body ...) rest-of-stream ...)
       (let ([body-exprs (process-expr-sequence (syntax (body ...)))])
         (with-syntax ([(body ...) body-exprs])
           (values (syntax (begin body ...))
                   (syntax (rest-of-stream ...)))))]))
  
  (define (process-app fun-expr arg-descr rest-stream)
    ;; NOTE: arg-helper doesn't return the rest of the stream.  
    ;; See explanation for process-expr-sequence.
    ;; NOTE 2: Assumes first arg has been taken care of.
    (define (arg-helper arg-stream)
      (if (stx-null? arg-stream)
          empty
          (syntax-case arg-stream (unquote)
            [((unquote something) other-things ...)
             (let-values ([(an-arg arg-rest) (extract-one-expression
                                              (syntax (something other-things ...)))])
               (cons an-arg (arg-helper arg-rest)))]
            [_
             (raise-syntax-error 'process-app "argument not preceded by comma" arg-stream)]])))
    (define (process-args arg-stream)
      (if (stx-list? arg-stream)
          (if (stx-null? arg-stream)
              empty
              (let-values ([(first-arg arg-rest) (extract-one-expression arg-stream)])
                (pretty-print first-arg)
                (cons first-arg (reverse (arg-helper arg-rest)))))
          (raise-syntax-error 'application "not a proper argument list" arg-stream)))
    (with-syntax ([fun fun-expr]
                  [(args ...) (process-args arg-descr)])
      (values (syntax (#%app fun args ...))
              rest-stream)))

  (define (process-const const-expr const-rest)
    ;; Do some error-checking to make sure it's a datum
    (values const-expr const-rest))

  ;; NOTE: process-expr-sequence doesn't return the rest of the stream
  ;; because it terminates precisely when its input stream is empty.
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
        (syntax-case (stx-car sexp-stream) (fun: if: do: quote)
          [fun:
           (process-fun: sexp-stream)]
          [if:
           (process-if: sexp-stream)]
          [do:
            (process-do: sexp-stream)]
          [(quote e)
           (process-const (syntax e) (stx-cdr sexp-stream))]
          [(e ...)
           (let-values ([(fun-expr better-be-empty)
                         (extract-one-expression (syntax (e ...)))])
             (if (not (stx-null? better-be-empty))
                 (raise-syntax-error 'application "too many terms" better-be-empty)
                 (if (and (stx-pair? (stx-cdr sexp-stream))
                          (stx-list? (stx-car (stx-cdr sexp-stream))))
                     (process-app fun-expr (stx-car (stx-cdr sexp-stream)) (stx-cdr (stx-cdr sexp-stream)))
                     (raise-syntax-error 'application "not beginning with arguments" (stx-car (stx-cdr sexp-stream))))))]
          [app-or-val
           (let ([v (syntax->datum (syntax app-or-val))])
             (if (or (number? v) (string? v) (boolean? v)) ;; missing a few!
                 (process-const (stx-car sexp-stream) (stx-cdr sexp-stream))
                 (if (symbol? v)
                     (syntax-case (stx-cdr sexp-stream) (unquote)
                       ;; This line cost me from 2:30am to 1:30pm.
                       ;; The problem is that f(x, y) -> f(x (unquote y)), which is indistinguishable
                       ;; from f applied to x applied to something -- unless you have the below rule
                       ;; to distinguish unquotes.
                       [((unquote anything) anything-else ...)
                        ;; identifier case!
                        (process-const (stx-car sexp-stream) (stx-cdr sexp-stream))]
                       [((anything ...) anything-else ...)
                        (process-app (stx-car sexp-stream) (stx-car (stx-cdr sexp-stream)) (stx-cdr (stx-cdr sexp-stream)))]
                       [_
                        ;; identifier case!
                        (process-const (stx-car sexp-stream) (stx-cdr sexp-stream))])
                     (raise-syntax-error 'application "not beginning with arguments" (stx-car (stx-cdr sexp-stream))))))])))

  (define (process-deffun: sexp-stream)
    (syntax-case sexp-stream (=) ;; _ is deffun:
      [(_ f-name argdesc = rest-of-stream ...)
       (identifier? (syntax f-name))
       (let-values ([(defs*+expr body-rest)
                     (extract-definition*+expression (syntax (rest-of-stream ...)))])
         (with-syntax ([(body ...) defs*+expr])
           (cond
             [(identifier? (syntax argdesc))
              (values (syntax (define (f-name . argdesc) body ...))
                      body-rest)]
             [(stx-list? (syntax argdesc))
              (with-syntax ([(args ...) (process-comma-separated-identifiers (syntax argdesc))])
                (values (syntax (define (f-name args ...) body ...))
                        body-rest))]
             [else
              (raise-syntax-error 'argument-list "not valid syntax" (syntax argdesc))])))]
      [_
       (raise-syntax-error 'deffun: "not proper syntax")]))
  
  (define (process-defstruct: sexp-stream)
    (syntax-case sexp-stream () ;; _ is defstruct:
      [(_ struct-name (field-descr ...) rest-of-stream ...)
       (identifier? (syntax struct-name))
       (values (with-syntax ([(fields ...)
                              (process-comma-separated-identifiers (syntax (field-descr ...)))])
                 (syntax (define-struct struct-name (fields ...) #:transparent)))
               (syntax (rest-of-stream ...)))]
      [_
       (raise-syntax-error 'defstruct: "not proper syntax")]))

  (define (process-defvar: sexp-stream)
    (syntax-case sexp-stream (=) ;; _ is defvar:
      [(_ v-name = rest-of-stream ...)
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
  
  (define (extract-definition*+expression sexp-stream . checker)
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
