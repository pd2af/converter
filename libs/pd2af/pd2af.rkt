#lang racket

(require "../lib/load/all.rkt")
; (require "../../../../odysseus/lib/load/all.rkt")
(require racket/syntax)
; (require "../lib/projects/pd2af/common.rkt")
; (require "../lib/projects/pd2af/types.rkt")
; (require "../lib/projects/pd2af/context.rkt")
; (require "../lib/projects/pd2af/sexp.rkt")
; (require "../lib/projects/pd2af/geometry.rkt")
(require "../../../../odysseus/lib/projects/pd2af/common.rkt")
(require "../../../../odysseus/lib/projects/pd2af/types.rkt")
(require "../../../../odysseus/lib/projects/pd2af/context.rkt")
(require "../../../../odysseus/lib/projects/pd2af/sexp.rkt")
(require "../../../../odysseus/lib/projects/pd2af/geometry.rkt")
(require sxml)

(provide pd2af)

; 4 possible translation templates:
; * Enzyme activities template
; * Metabolite activities template
; * Combined activities template +
; * Simple enzyme activities template

(define (translate sexp-0 af-context-0 pd-context)
  (let loop ((sexp sexp-0) (af-context af-context-0))
    (match sexp
      ; base signal pathway regulation
      ((list-no-order
          `((,regulator-id) ,control-id (,x-id))
          `((,substrate-id) ,process-id (,product-id))
          _ ...)
        #:when (and (equal? x-id process-id)) ; (macromolecule? (car substrate-id) pd-context) (macromolecule? (car product-id) pd-context))
        (let* (
              (substrate (&& substrate-id pd-context))
              (product (&& product-id pd-context))
              (control (&& control-id pd-context))
              (regulator (&& regulator-id pd-context))

              (substrate-af (hash-union (hash 'id substrate-id 'class "biological activity" 'name (get-af-name substrate) 'type ($ class substrate)) substrate))
              (product-af (hash-union (hash 'id product-id 'class "biological activity" 'name (get-af-name product) 'type ($ class product)) product))
              (regulator-af (hash-union (hash 'id regulator-id 'class "biological activity" 'type ($ class control)) regulator))

              (regulator-substrate-influence (hash 'id (set-id) 'class (inverse-class (to-af-class ($ class control))) 'sources (list regulator-id) 'targets (list substrate-id)))
              (regulator-product-influence (hash 'id (set-id) 'class (to-af-class ($ class control)) 'sources (list regulator-id) 'targets (list product-id)))

              ; TODO: recode following up-to-date terms and rules of translation
              (substrate-active? (active? substrate-id pd-context))
              (substrate-inactive? (not substrate-active?))
              (product-active? (active? product-id pd-context))
              (product-inactive? (not product-active?))
              ; END
              )
          (cond
            ; a-a
            ((and substrate-active? product-active?)
                (loop
                  (@@--- substrate-id product-id control-id process-id sexp)
                  (&&>> regulator-af regulator-substrate-influence substrate-af regulator-product-influence product-af af-context)))
            ; a-i
            ((and substrate-active? product-inactive?)
                (loop
                  (@@--- substrate-id product-id control-id process-id sexp)
                  (&&>> regulator-af regulator-substrate-influence substrate-af af-context)))
            ; i-a
            ((and substrate-inactive? product-active?)
                (loop
                  (@@--- substrate-id product-id control-id process-id sexp)
                  (&&>> regulator-af regulator-product-influence product-af af-context)))
            ; i-i
            ((and substrate-inactive? product-inactive?)
                (loop
                  (@@--- substrate-id product-id control-id process-id sexp)
                  (&&>> regulator-af regulator-product-influence product-af af-context))))))
      ; metabolic pathway
      ; if no more matches - exit
      (else
        (values sexp af-context)))))

; start translation process
(define (pd2af pd-sexp-0 pd-context)
  (let*-values (
        ; take namespace and other non-glyph things from pd-context
        ((af-context-0) (append
                          (filter-not ProcessGlyph? pd-context)
                          (filter Container? pd-context)))
        ((sexp af-context) (translate pd-sexp-0 af-context-0 pd-context))
        )
    af-context))
