#lang racket

(require "../odysseus/lib/load/all.rkt")
(require "xml2pd.rkt" "pd2pd.rkt" "pd2af.rkt" "af2af.rkt" "af2xml.rkt")
(require "../sbgn/common.rkt")
(require "../sbgn/types.rkt")
(require "../sbgn/context.rkt")
(require "../sbgn/sexp.rkt")
(require "translate.rkt")
(require rackunit)

(provide check-translation-equal? contexts-equal?)

(define (----id ids context)
	(----
		(filter (λ (el) (indexof? ids ($ id el))) context)))

(define (get-directed-graph context)
	(let ((result
	(for/fold
		((res (hash)))
		((arc (filter ActivityArc? context)))
		(let* ((source-ids ($ sources arc))
					(source-id (car source-ids))
					(target-ids ($ targets arc))
					(target-id (car target-ids))
					)
			(hash-union
				res
				(hash (list
								(cond
									((one-element? source-ids)
										(get-signature (&& (car source-ids) context) context))
									(else
										(sort
											(map (λ (x) (get-signature (&& x context) context)) source-ids)
											sort-by-names)))
								(cond
									((one-element? target-ids)
										(get-signature (&& (car target-ids) context) context))
									(else
										(sort
											(map (λ (x) (get-signature (&& x context) context)) target-ids)
											sort-by-names))))
							($ class arc)))))))
			result))

(define-catch (contexts-equal? context1 context2)
	(let* ((dg1 (get-directed-graph context1))
				(dg2 (get-directed-graph context2))
				(equal-hashes? (check-hash dg1 dg2)))
		(when
			(not equal-hashes?)
			(--- "\n") (---- dg1) (--- "---") (---- dg2) (--- "\n"))
		equal-hashes?))

(define-catch (check-translation-equal? pd-file directed-graph-2)
	(let* (
				(sbgn_ml (read-file pd-file))

        (pd (parse-pd-sbgn-ml sbgn_ml))
				; (_ (---- ($ pd-context pd)))

				(pd (remove-simple-chemicals pd))

        (pd (logical-nodes-2-multiarcs pd))
				(pd (process-nodes-2-multiarcs pd))

        (pd (fold-same-signatures pd))
				; (pd (clean-noncontrolled-elementary-ends pd))

        (pd-sexp ($ pd-sexp pd))
        (pd-context ($ pd-context pd))

        (af-context (pd2af pd-sexp pd-context))
				(af-context (collapse-same-name-chains af-context))
				(af-context (remove-self-loops af-context))
				(af-context (remove-source-and-sinks af-context))
				; (_ (----id '(glyph-51) af-context))
				; (_ (---- af-context))

				; here add AND gates and all these extra arcs, that we don't compare in the test
				(af-xml (af2xml af-context))

				(_ (write-file "last-test.af.sbgn" af-xml))

				(directed-graph-1 (get-directed-graph af-context))
				)
			(check-hash-difference directed-graph-1 directed-graph-2 "graph after translation" "should-be graph")))
