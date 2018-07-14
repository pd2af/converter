#lang racket

(require racket/syntax)
(require "../odysseus/lib/load/all.rkt")
(require "../sbgn/common.rkt")
(require "../sbgn/types.rkt")
(require "../sbgn/context.rkt")
(require "../sbgn/sexp.rkt")
(require "../sbgn/geometry.rkt")

(provide (all-defined-out))

; (...) -> ((...) (...))
(define-catch (get-grouped-elements f elements (grouped-elements (list)) (results (list)))
	(cond
		((null? elements) grouped-elements)
		(else
			(let* ((element (car elements))
						(element-i (indexof results (f element)))
						(element-group (nth grouped-elements element-i))
						(element-group (pushr element-group ($ id element)))
						(results (if (= element-i 0) (pushr results (f element)) results))
						(grouped-elements (if (= element-i 0)
																(pushr grouped-elements element-group)
																(setn grouped-elements element-i element-group))))
				(get-grouped-elements f (cdr elements) grouped-elements results)))))

;; other
(define (same-group id1 id2 nodes-grouped)
  (ormap
    (λ (group) (and (indexof? group id1) (indexof? group id2)))
    nodes-grouped))

(define (prime-id id nodes-grouped)
  (for/fold
    ((res #f))
    ((group nodes-grouped))
    (or
      res
      (cond
        ((indexof? group id) (car group))
        (else #f)))))

(define f-same (λ (x) (list ($ compartment x) ($ name x) ($ class x) ($ uoi x))))

; list contains both simple chemicals and macromolecules?
(define (mixed-list? ids context)
	(let ((elements (if (list? ids)
										(map
											(λ (id) (get-element-by-id id context))
											ids)
										#f)))
		(and
			(list? ids)
			(> (length ids) 1)
			(not
				(empty?
					(filter
						ElementaryEPN?
						elements)))
			(not
				(empty?
					(filter-not
						ElementaryEPN?
						elements))))))

(define-catch (remove-simple-chemicals pd)
	(let* (
		    (context0 ($ pd-context pd))
				(sexp0 ($ pd-sexp pd))
				(elementary-nodes (filter ElementaryEPN? context0)))
		(let-values (((context sexp)
					(for/fold
						((context context0) (sexp sexp0))
						((el elementary-nodes))
						(let ((id ($ id el)))
							(cond
								((deletable? el context)
									(values
										(&&-- id context)
										(tree-exclude sexp id)))
								; ((SourceSink? el) ; remove all source and sinks from the diagram
								; 	(values
								; 		(&&-- id context)
								; 		; (@@--- id sexp)))
								; 		(for/fold
								; 			((res empty))
								; 			((triplet sexp))
								; 			(match triplet
								; 				(`((? list? ,id1) ,id2 (? list? ,id3))
								; 					#:when (or
								; 										(indexof? id1 id)
								; 										(indexof? id3 id))
								; 						(if (or (one-element? id1) (one-element? id3))
								; 							res
								; 							(pushr res (list (exclude id1 id) id2 (exclude id3 id)))))
								; 				(else (pushr res triplet))))))
								(else
									(values
										context
										sexp)))))))
			(hash 'pd-context context 'pd-sexp sexp))))

(define-catch (logical-nodes-2-multiarcs pd)
  (let* (
		    (context ($ pd-context pd))
				(arcs (filter LogicArc? context))
		    (sexp ($ pd-sexp pd))
		    (nodes (filter LogicalOperator? context)))
		(let-values (((context sexp)
										(for/fold
											((res-context context) (res-sexp sexp))
									    ((node nodes))
									    (let* (
														(id ($ id node))
														(in-id ($ in-id node))
									          (out-id ($ out-id node))
														(in-nodes-id (flatten
																						(cleanmap
																							(map
																								(λ (arc) (if (or (indexof? ($ targets arc) in-id) (indexof? ($ targets arc) out-id))
																														($ sources arc)
																														#f))
																								arcs))))
														(out-nodes-id (flatten
																						(cleanmap
																							(map
																								(λ (arc) (if (or (indexof? ($ sources arc) out-id) (indexof? ($ sources arc) in-id))
																														($ targets arc)
																														#f))
																								context))))
														; (out-nodes-id (car out-nodes-id)) ; the third element for a control triplet should be scalar, not list
														(out-arcs-id (cleanmap
																					(map
																						(λ (el) (if (or (indexof? ($ sources el) out-id) (indexof? ($ sources el) in-id))
																												($ id el)
																												#f))
																						context)))
														(out-arc-id (car out-arcs-id)) ; outcoming arcs for logical node must be the same type, so we take the first
														(out-arc (get-element-by-id out-arc-id context))
														(new-id (set-id #:prefix (id-prefix ($ class out-arc))))
														(multiarc (hash-union (hash 'id new-id 'name new-id 'sources in-nodes-id 'targets out-nodes-id 'class ($ class out-arc)) node))
														(multiarc (hash-delete-all multiarc '(out-id in-id x y w h)))
														(res-context (exclude res-context node)) ; exclude logical node from context
														(res-context (for/fold ((res res-context)) ((out-arc-id out-arcs-id)) (exclude res (&& out-arc-id context)))) ; exclude old control arcs from context
														(res-context (context-redirect (list id in-id out-id) new-id res-context)) ; redirect all references in the context onto the 'new-id'
														; (res-sexp (subst-ids (list id out-id in-id) new-id res-sexp)) ; substitute all references to old process node on references on the new multiarc
														(res-context (pushr res-context multiarc)) ; add new multiarc to context
														(res-sexp (filter-not
																				(λ (triplet)
																					(match triplet
																							(`((,i1 ...) ,i2 (,i3 ...))
																									(or (equal? id i2) (indexof? i1 out-id) (indexof? i1 in-id)))
																							(else #f)))
																				res-sexp)) ; exclude triplet with logical node and with outcoming control from sexp
														(res-sexp (pushr res-sexp `(,in-nodes-id ,new-id ,out-nodes-id))) ; add new multiarc to sexp
														)
												(values
													res-context
													res-sexp)))))
					(let* ((context (filter-not LogicArc? context))) ; remove all flux arcs from the context
						(hash
							'pd-context context
							'pd-sexp sexp)))))

(define-catch (process-nodes-2-multiarcs pd)
  (let* (
		    (context ($ pd-context pd))
				(arcs (filter FluxArc? context))
		    (sexp ($ pd-sexp pd))
		    (nodes (filter ProcessNode? context)))
		(let-values (((context sexp)
										(for/fold
											((res-context context) (res-sexp sexp))
									    ((node nodes))
									    (let* (
														(id ($ id node))
														(in-nodes-id (flatten
																						(cleanmap
																							(map
																								(λ (arc) (if (indexof? ($ targets arc) id)
																														($ sources arc)
																														#f))
																								arcs))))
														(out-nodes-id (flatten
																						(cleanmap
																							(map
																			 					(λ (arc) (if (indexof? ($ sources arc) id)
																														($ targets arc)
																														#f))
																								arcs))))
														(new-id (set-id #:prefix (id-prefix ($ class node))))
														(multiarc (hash-union (hash 'id new-id 'name new-id 'sources in-nodes-id 'targets out-nodes-id 'class ($ class node)) node))
														(multiarc (hash-delete-all multiarc '(out-id in-id x y w h)))
														(res-context (exclude res-context node)) ; exclude process node from context
														(res-context (context-redirect (list id) new-id res-context)) ; redirect all references in the context onto the 'new-id'
														(res-sexp (subst-ids (list id) new-id res-sexp)) ; substitute all references to old process node on references on the new multiarc
														(res-context (pushr res-context multiarc)) ; add new multiarc to context
														(res-sexp (filter-not
																				(λ (triplet)
																					(match triplet
																							(`((,i1 ...) ,i2 (,i3 ...))
																									; (or (equal? i1 id) (equal? i3 id)))
																									(equal? i2 id)) ; exclude process node from sexp
																							(else #f)))
																				res-sexp))
														(res-sexp (pushr res-sexp `(,in-nodes-id ,new-id ,out-nodes-id))) ; add new multiarc to sexp
														)
												(values
													res-context
													res-sexp)))))
					(let* ((context (filter-not FluxArc? context))) ; remove all flux arcs from the context
						(hash
							'pd-context context
							'pd-sexp sexp)))))

(define-catch (deletable? id context)
	(let* ((el (&& id context))
				(el-info (what-kind-of-element el context))
				(mixed-end? ($ mixed-end? el-info))
				(source-end? ($ source-end? el-info))
				(target-end? ($ target-end? el-info))
				(single-end? ($ single-end? el-info)))
		(and
			mixed-end?
			(not single-end?))))

; fold nodes with same names and no controls between them
(define-catch (fold-same-signatures pd)
	(let* (
        (context0 ($ pd-context pd))
        (sexp0 ($ pd-sexp pd))
				(nodes (filter Node? context0)) ; take only nodes, without arcs
				(nodes-grouped (get-grouped-elements f-same nodes))) ; merge with equal names (sameness defined by f-same)
		(let loop ((context context0) (sexp sexp0) (nodes-left nodes-grouped))
			(cond
				((empty? nodes-left) (hash 'pd-context context 'pd-sexp sexp))
				(else
					(let* ((node-group (car nodes-left))
								; 1 redirect references sources and targets in context and substitutes ids in sexp
								(pd (if (one-element? node-group)
											(%% context sexp)
											(%%
												(context-redirect node-group (car node-group) context)
												(subst-ids node-group (car node-group) sexp))))
								; 2 remove processes without controls between equal ids
								;; after ids substitutions some of process multiarcs now can have nodes of same names on each end, in the case these hononyms are single or the same on both ends and process doesn't have incoming controls - we can remove this process multiarc from the context completely
								(pd (%%
											; remove multiarcs that point to the same source and target
											(filter-not
							 					(λ (el)
													; (--- 	($ id el) ($ name el)
													; 			(and
													; 				(not (incoming-arcs? ($ id el) context))
													; 				(hash-ref el 'sources #f)
													; 				(hash-ref el 'targets #f)
													; 				(equal? ($ sources el) ($ targets el))))
													(and
														(not (incoming-arcs? ($ id el) context))
														(hash-ref el 'sources #f)
														(hash-ref el 'targets #f)
														(equal? ($ sources el) ($ targets el))))
												(%%-context pd))
											(filter-not
												(λ (triplet)
													(match triplet
														(`(,id1 ,id2 ,id3)
																(and
																		(not (incoming-arcs? id2 context0))
																		(equal? id1 id3)))
														(else #f)))
												(%%-sexp pd)))))
					(loop (%%-context pd) (%%-sexp pd) (cdr nodes-left))))))))

(define-catch (clean-noncontrolled-elementary-ends pd)
	(let* (
        (context ($ pd-context pd))
        (sexp ($ pd-sexp pd))
				(elementary-epns (filter ElementaryEPN? context)))
		(let loop ((new-sexp sexp) (new-context context) (epns elementary-epns))
				(cond
					((empty? epns) (hash 'pd-context new-context 'pd-sexp new-sexp))
					(else
						(let ((epn-id ($ id (car epns))))
							(match new-sexp
								(`(,_ ... (,id1 ,id2 (,(== epn-id))) ,_ ...)
									(if (and
												(ProcessNode? (&& id2 context)) ; yet already multiarc, not process node
												(not (incoming-arcs? id2 context))
												(deletable? id2 context))
										(loop
											(@@-- `(,id1 ,id2 (,epn-id)) epn-id new-sexp)
											(&&-- id2 (&&-- epn-id new-context))
											(cdr epns))
										(loop new-sexp new-context (cdr epns))))
								(else
									(loop new-sexp new-context (cdr epns))))))))))

(define-catch (expand-names pd)
	(let* ((context ($ pd-context pd))
				(sexp ($ pd-sexp pd))
				(pd-res
					(for/fold
						((res empty))
						((el context))
						(cond
							((not ($ name el)) (pushr res el))
							(else
								(let* ((current-name ($ name el))
											(states (filter (λ (x) (indexof? ($ default-state-variables defaults) x)) (hash-keys el)))
											(state-values (map (λ (x) (hash-ref el x)) states))
											(same-name-elements (filter (λ (x) (equal? ($ name x) current-name)) context)))
									(cond
										((one-element? same-name-elements) (pushr res el))
										((empty? state-values) (error "same names and no states for two or more elements"))
										(else
											(pushr res (hash-union (hash 'name (str ($ name el) "-" (implode state-values "-"))) el))))))))))
		(hash 'pd-context pd-res 'pd-sexp sexp)))
