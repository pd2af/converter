#lang racket

(require racket/syntax)
(require sxml)
(require "../odysseus/lib/load/all.rkt")
(require "../sbgn/common.rkt")
(require "../sbgn/types.rkt")
(require "../sbgn/context.rkt")
(require "../sbgn/sexp.rkt")
(require "../sbgn/geometry.rkt")

(provide pd2af)

; 4 possible translation templates:
; * Enzyme activities template
; * Metabolite activities template
; * Combined activities template +
; * Simple enzyme activities template

(define (pd2af-1 sexp-0 af-context-0 pd-context)
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
              (and-gate (hash 'id (set-id) 'class "and" 'sources (list substrate-id regulator-id) 'targets (list product-id)))
              (logic-arc-substrate (hash 'id (set-id) 'class "logic arc" 'sources (list substrate-id) 'targets (list ($ id and-gate))))
              (logic-arc-regulator (hash 'id (set-id) 'class "logic arc" 'sources (list regulator-id) 'targets (list ($ id and-gate))))
              (and-gate-influence (hash 'id (set-id) 'class (to-af-class ($ class control)) 'sources (list ($ id and-gate)) 'targets (list product-id)))

              ; TODO: recode following up-to-date terms and rules of translation
              (substrate-active? (active? substrate-id pd-context))
              (substrate-inactive? (not substrate-active?))
              (product-active? (active? product-id pd-context))
              (product-inactive? (not product-active?))
              ; END
              )
          (cond
            ; metabolic pathway
            ((and (Metabolite? substrate) (Metabolite? product))
              (case ($ class control)
                ; ((stimulation catalysis)
                (else
                  (loop
                    (@@--- substrate-id product-id control-id process-id sexp)
                    (&&>> substrate-af logic-arc-substrate
                          regulator-af logic-arc-regulator
                          and-gate
                          and-gate-influence product-af
                          af-context)))
              ))
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
        ((sexp af-context) (pd2af-1 pd-sexp-0 af-context-0 pd-context))
        )
    af-context))
