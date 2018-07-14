#lang racket

(require "xml2pd.rkt" "pd2pd.rkt" "pd2af.rkt" "af2af.rkt" "af2xml.rkt")
(require "../odysseus/lib/load/all.rkt")
(require "../sbgn/common.rkt")
(require "../sbgn/types.rkt")
(require "../sbgn/context.rkt")
(require "../sbgn/sexp.rkt")
(require "../sbgn/geometry.rkt")
(require "../sbgn-lisp/sbgn-form.rkt")

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
              ; remove-source-and-sinks
							strip-off-state-prefix-if-no-duplication

              (pd2af ($ pd-sexp pd) ($ pd-context pd)))))
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
