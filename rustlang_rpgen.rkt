#lang racket/base

;; Requirements
(require xsmith
         racr
         racket/string
         racket/port
         pprint
         )


;; Functional Helpers

;; Algebra Helper
(define (bin-expr op pn)
  (hs-append
    (text "(")
    (render-node (ast-child 'l pn))
    (text op)
    (render-node (ast-child 'r pn))
    (text ")")
    ))

(define (combine-prog pn)
  (hs-append
    (render-node (ast-child 'fd pn))
    (render-node (ast-child 'main pn))
    ))

(define (main-func pn)
  (hs-append
    (text "fn main() {")
    (render-node (ast-child 'b pn))
    (text "}")
    ))

(define (return-stmt pn)
  (hs-append
    (text "return")
    (render-node (ast-child 'e pn))
    (text ";")
    ))


;; General Rules
(define-spec-component arith)

(add-to-grammar
  arith
  [Node #f ()]
  [Program Node ([fd : FunctionDeclaration]
                 [main : FunctionDeclaration])]
  ;; Declaration 
  ;; [Declaration Node (name type)]
  ;; [FunctionDeclaration] (name Block)
  [FunctionDeclaration Node ([b : Block])]
  ;; [Block]
  [Block Node ([return : ReturnStatement])]
  ;; [Statement] (... )
  [Statement Node ()] 
  ;; [ReturnStatement] (return Expression)
  [ReturnStatement Statement ([e : Expression])]
  [Expression #f ()
              #:prop may-be-generated #f]
  [LiteralInt Expression ([v = (random 100)])]
  [Addition Expression ([l : Expression] [r : Expression])
            ;; The default weight is 10.
            #:prop choice-weight 20]
  [Subtraction Expression ([l : Expression] [r : Expression])
               ;; The default weight is 10.
               #:prop choice-weight 20]
  [Multiplication Expression ([l : Expression] [r : Expression])
                  ;; The default weight is 10.
                  #:prop choice-weight 20]
  [Division Expression ([l : Expression] [r : Expression])
            ;; The default weight is 10.
            #:prop choice-weight 20]
  )

(define int (base-type 'int))
(add-prop arith type-info
          [Program [int (λ (n t) (hash 'fd int 'main int))]]
          [FunctionDeclaration [int (λ (n t) (hash 'b int))]]
          [Block [int (λ (n t) (hash 'return int))]]
          [ReturnStatement [int (λ (n t) (hash 'e int))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]]
          [Subtraction [int (λ (n t) (hash 'l int 'r int))]]
          [Multiplication [int (λ (n t) (hash 'l int 'r int))]]
          [Division [int (λ (n t) (hash 'l int 'r int))]]
          )

(add-prop arith render-node-info
          [Program (λ (n) (combine-prog n))]
          [FunctionDeclaration (λ (n) (main-func n))]
          [Block (λ (n) (render-node (ast-child 'return n)))]
          [ReturnStatement (λ (n) (return-stmt n))]
          [LiteralInt (λ (n) (text (number->string (ast-child 'v n))))]
          [Addition (λ (n) (bin-expr "+" n))]
          [Subtraction (λ (n) (bin-expr "-" n))]
          [Multiplication (λ (n) (bin-expr "*" n))]
          [Division (λ (n) (bin-expr "/" n))]
          )

;; This line defines `rustlang-generate-ast`.
(assemble-spec-components rustlang arith)

;; Program generate starts from 'Program
(define (rustlang-generate)
  (rustlang-generate-ast 'Program))


;; Info Helpers
;; Use pprint, helps print a format text
(define (rustlang-format-render form)
  (with-output-to-string
    (λ ()
       (define (pp x)
         (pretty-print x (current-output-port) 1))
       (pp form))))

;; Info
(xsmith-command-line
  rustlang-generate
  #:fuzzer-name "rustlang"
  #:format-render rustlang-format-render
  #:comment-wrap (λ (lines)
                    (string-join
                      (map (λ (x) (format "// ~a" x)) lines)
                      "\n")))
