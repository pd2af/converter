#lang racket

(require "../lib/load/all.rkt")
(require "xml2pd.rkt" "pd2pd.rkt" "pd2af.rkt" "af2af.rkt" "af2xml.rkt")
; (require "../lib/projects/pd2af/common.rkt")
; (require "../lib/projects/pd2af/types.rkt")
; (require "../lib/projects/pd2af/context.rkt")
; (require "../lib/projects/pd2af/sexp.rkt")
(require "../../../../odysseus/lib/projects/pd2af/common.rkt")
(require "../../../../odysseus/lib/projects/pd2af/types.rkt")
(require "../../../../odysseus/lib/projects/pd2af/context.rkt")
(require "../../../../odysseus/lib/projects/pd2af/sexp.rkt")
(require "../../../sbgn-lisp/sbgn-form.rkt")

(provide (all-defined-out))

(define (translate-pd pd-str)
	(let* ((pd-form (read (open-input-string pd-str)))
				(pd-context (form->context pd-form))
				(pd-sexp (context->sexp pd-context))
				(pd (hash 'pd-context pd-context 'pd-sexp pd-sexp))
        (pd (->>
              ; fold-same-signatures
							expand-names
              process-nodes-2-multiarcs
              logical-nodes-2-multiarcs
              ; remove-simple-chemicals

              pd))
        (af-context
            (->>
              collapse-same-name-chains
              remove-self-loops
              remove-source-and-sinks
							strip-off-state-prefix-if-no-duplication

              (pd2af ($ pd-sexp pd) ($ pd-context pd)))))
		; (--- af-context)
    af-context))

(define-catch (get-af-context af-str)
	(form->context (read (open-input-string af-str))))

(define (sort-by-names a b)
	(cond
		; if no compartment (compatment = #f):
		((or (not (first a)) (not (first b)))
			(string<? (second a) (second b)))
		; if the same compartment:
		((string=? (first a) (first b))
		 	(string<? (second a) (second b)))
		; order by compartments otherwise:
		(else
			(string<? (first a) (first b)))))

(define (get-directed-graph context)
	(let ((result
	(for/fold
		((res (hash)))
		((arc (filter ActivityArc? context)))
		(let* ((source-ids (or ($ sources arc) ($ source arc)))
					(target-ids (or ($ targets arc) ($ target arc)))
					)
			(hash-union
				res
				(hash (list
								(cond
									; logical arcs has 'source instead of 'target and scalar value in it
									((scalar? source-ids) (get-signature (&& source-ids context) context))
									((one-element? source-ids)
										(get-signature (&& (car source-ids) context) context))
									(else
										(sort
											(map (λ (x) (get-signature (&& x context) context)) source-ids)
											sort-by-names)))
								(cond
									((scalar? target-ids) (get-signature (&& target-ids context) context))
									((one-element? target-ids)
										(get-signature (&& (car target-ids) context) context))
									(else
										(sort
											(map (λ (x) (get-signature (&& x context) context)) target-ids)
											sort-by-names))))
							($ class arc)))))))
			result))
