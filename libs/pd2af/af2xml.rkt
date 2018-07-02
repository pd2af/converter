#lang racket

(require "../lib/load/all.rkt")
; (require "../../../../odysseus/lib/load/all.rkt")
(require racket/syntax)
(require sxml)
(require "../lib/projects/pd2af/common.rkt")
(require "../lib/projects/pd2af/types.rkt")
(require "../lib/projects/pd2af/geometry.rkt")
(require "../lib/projects/pd2af/context.rkt")
(require "../lib/projects/pd2af/sexp.rkt")

(provide af2xml)

(define-catch (gate-el source-ids target-ids af-context)
	(let* (
				(neighbour-ids (append source-ids target-ids))
				(neighbours (map (λ (node-id) (&& node-id af-context)) neighbour-ids))
				; (_ (--- (&& 'glyph48 af-context)))
				; (_ (--- neighbour-ids "\n" neighbours "\n\n"))
				(gate-coors (get-node-xy-in-the-middle neighbours))
				(gate-x ($ x gate-coors)) (gate-y ($ y gate-coors)) (gate-w gateW) (gate-h gateH)
				(gate-id (set-id #:prefix "and-"))
				(gate-in-id (set-id #:prefix "and-in-"))
				(gate-out-id (set-id #:prefix "out-in-"))
				(sources (map (λ (id) (&& id af-context)) source-ids))
				(gate-ports-coors (calculate-port-coors sources gate-x gate-y)) ; calculate in relation only to sources, targets port will be on the opposite side anyway
				)
		(hash 'id gate-id 'class "and"
					'x gate-x 'y gate-y 'w gate-w 'h gate-h
					'in-id gate-in-id 'in-x ($ port-in-x gate-ports-coors) 'in-y ($ port-in-y gate-ports-coors)
					'out-id gate-out-id 'out-x ($ port-out-x gate-ports-coors) 'out-y ($ port-out-y gate-ports-coors))))

(define-catch (build-sxml-el el af-context)
	(let* (
				(tagname (if (ActivityArc? el) 'arc 'glyph))
				(id ($ id el))
				(class ($ class el))
				(x ($ x el))
				(y ($ y el))
				(w ($ w el))
				(h ($ h el))
				(compartment (if (and (ActivityNode? el) ($ compartment el))
												(format-list `(compartmentRef ~s) ($ compartment el))
												'$f))
				(source (if (ActivityArc? el)
										(format-list `(source ~s) ($ source el))
										'$f))
				(target (if (ActivityArc? el)
										(format-list `(target ~s) ($ target el))
										'$f))
				(label (if (and (or* Container? ActivityNode? el) ($ name el))
										(format-list `(label (@ (text ~s))) (get-node-name el))
										'$f))
				(bbox (if (or* Container? ActivityNode? ActivityLogicalOperator? el)
										(format-list
											`(bbox (@ (x ~s) (y ~s) (w ~s) (h ~s)))
											 x y w h)
										'$f))
				(uoi (if (ActivityNode? el)
								(let* ((uoi-entity (or ($ type el) ($ class el)))
											(uoi-entity (if (indexof? ActivityDefaultTypes uoi-entity)
																		'$f
																		(format-list '(entity (@ (name ~s))) uoi-entity)))
											(uoi-id (set-id #:prefix "uoi-"))
											; (uoi-text (if (another-same-name+class-element? el af-context) ($ uoi el) ""))
											(uoi-text (or ($ uoi el) ""))
											(x-uoi (+ x (/ w 2.0) (/ (- uoiW) 2.0)))
											(y-uoi (- y (/ uoiH 2.0)))
											)
									(if (and (not-equal? uoi-entity '$f))
										(format-list
											`(glyph (@ (id ~s) (class "unit of information"))
													(label (@ (text ~s)))
													~a
													(bbox (@ (x ~s) (y ~s) (w ~s) (h ~s))))
											uoi-id
											uoi-text
											uoi-entity
											x-uoi y-uoi uoiW uoiH)
										'$f))
								'$f))
				; (_ (--- el))
				; (coors (if (ActivityArc? el)
				; 					(let ((source-el (&& ($ source el) af-context))
				; 								(target-el (&& ($ target el) af-context)))
				; 						(get-line-between source-el target-el))
				; 					#f))
				(x1 ($ x1 el))
				(y1 ($ y1 el))
				(x2 ($ x2 el))
				(y2 ($ y2 el))
				(start (if (ActivityArc? el)
									(format-list
										`(start (@ (x ~s) (y ~s)))
										x1 y1)
									'$f))
				(end (if (ActivityArc? el)
									(format-list
										`(end (@ (x ~s) (y ~s)))
										x2 y2)
									'$f))
				(in-port (if (ActivityLogicalOperator? el)
									(format-list
										`(port (@ (id ~s) (x ~s) (y ~s)))
										($ in-id el) ($ in-x el) ($ in-y el))
									'$f))
				(out-port (if (ActivityLogicalOperator? el)
									(format-list
										`(port (@ (id ~s) (x ~s) (y ~s)))
										($ out-id el) ($ out-x el) ($ out-y el))
									'$f))
				)
		(format-list
			`(~a (@ (id ~s) (class ~s)
							~a ; compartmentRef
							~a ; source
							~a ; target
						)
					~a ; label
					~a ; bbox
					~a ; uoi
					~a ; start
					~a ; end
					~a ; in-port
					~a ; out-port
				)
				tagname
				id class
				compartment
				source
				target

				label
				bbox
				uoi
				start
				end
				in-port
				out-port
				)))

(define-catch (af2sxml af-context)
	(let ((translated-ids empty))
		(for/fold
			((res empty))
			((el af-context))
			(cond
				((ActivityArc? el)
					; unfold multiarc to the combination of simple arcs and gates:
					(let* (
								(source-ids ($ sources el))
								(target-ids ($ targets el))
								(gate (gate-el source-ids target-ids af-context))
								(class ($ class el))
								(source-arcs (for/list
																((source-id source-ids))
																(let ((coors (get-line-between (&& source-id af-context) gate)))
																	(hash 'id (set-id #:prefix "arc-") 'class "logic arc"
																				'source source-id 'target ($ in-id gate)
																				'x1 ($ x1 coors) 'y1 ($ y1 coors) 'x2 ($ x2 coors) 'y2 ($ y2 coors)))))
								(target-arcs 	(for/list
																((target-id target-ids))
																(let ((coors (get-line-between gate (&& target-id af-context))))
																	(hash 'id (set-id #:prefix "arc-") 'class class
																				'source ($ out-id gate) 'target target-id
																				'x1 ($ x1 coors) 'y1 ($ y1 coors) 'x2 ($ x2 coors) 'y2 ($ y2 coors)))))
								(single-arc (let ((coors (get-line-between (&& (car source-ids) af-context) (&& (car target-ids) af-context))))
																	(hash-union
																		(hash
																			'source (car source-ids) 'target (car target-ids)
																			'x1 ($ x1 coors) 'y1 ($ y1 coors) 'x2 ($ x2 coors) 'y2 ($ y2 coors))
																		el)))
								(actual-arcs
										(cond
											((not (and (list? source-ids) (list? target-ids))) (error "sources or targets have value other than list in ActivityArc element"))
											((or (more-than-one-element? source-ids) (more-than-one-element? target-ids))
												(pushr (append source-arcs target-arcs) gate))
											(else (list single-arc)))))
						(append res (map (λ (arc) (build-sxml-el arc af-context)) actual-arcs))))
				((or* Container? ActivityNode? el)
					(if (indexof? translated-ids ($ id el))
						res
						(begin
							(set! translated-ids (pushr translated-ids ($ id el)))
							(pushr res (build-sxml-el el af-context)))))
				(else
					res)))))

(define-catch (sxml2xml af-sxml)
	(let ((namespace "http://sbgn.org/libsbgn/0.2"))
		; (---- af-sxml)
		(format "~a~n~a"
			"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
			(srl:sxml->xml
				(format-list
					`(sbgn (@ (xmlns ,namespace)) (map (@ (language "activity flow")) ,@af-sxml)))))))

(define-catch (af2xml af-context)
	; (---- (af2sxml af-sexp af-context))
	(sxml2xml
		(order-by-tag
			'(glyph arc)
			(tree-clean not-empty-list? empty? (af2sxml af-context)))))
