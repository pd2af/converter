#lang racket

(require "../odysseus/lib/load/all.rkt")
(require "../sbgn/common.rkt")
(require "../sbgn/types.rkt")
(require "../sbgn/context.rkt")
(require "../sbgn/sexp.rkt")
(require "../sbgn/geometry.rkt")
(require racket/syntax)

(provide (all-defined-out))

; element names consider equal if equal are their af-signatures
(define (get-af-signature el)
  (let* ((res
          (list ($ compartment el) ($ class el) ($ name el)))
        (res (if (ontology-uoi? ($ uoi el))
                  (append res (list ($ uoi el)))
                  res)))
    res))

(define (except-current-element el-id af-context)
  (filter-not (λ (e) (equal? ($ id e) el-id)) af-context))

(define-catch (collapse-same-name-chains af-context)
  (for/fold
    ((res-context af-context))
    ((el af-context))
    (cond
      ((ActivityNode? el)
        ;; elements with same 'name' as 'el'
        (let* ((same-names (filter
                              (λ (e) (equal? (get-af-signature e) (get-af-signature el)))
                              (except-current-element ($ id el) res-context)))
              (same-names-id (map (λ (e) ($ id e)) same-names)))
          (if (not-empty? same-names)
            (let* (
                  (el-id ($ id el))
                  ;; choose nodes that have incoming arcs and this arcs are not from the same-names:
                  (starter-nodes (filter (λ (s) (not-same-name-incoming-arcs? s res-context)) (pushr same-names ($ name el))))
                  ;; if any nodes with incoming, choose the first in the list, other same-names will be collapsed into this node. If no any nodes with incoming arcs - pick the current element:
                  (only-el-id (if (not-empty? starter-nodes) (car starter-nodes) el-id))
                  (only-el (&& only-el-id res-context))
                  ;;; make a collapse:
                  ; ATTENTION: by removing adjacent arcs to the deleted elements, some other nodes also can be removed (current work of &&--)
                  (collapsed-context
                              (for/fold
                                ((res2 res-context))
                                ; iterate over all el same-names, except only-el element
                                ((s-id (exclude (pushr same-names-id el-id) only-el-id)))
                                  (&&<> s-id only-el-id res2)))

                  )
              collapsed-context)
            res-context))) ; else
      (else res-context))))

(define-catch (remove-self-loops context)
  (for/fold
    ((res empty))
    ((e context))
    (let* ((sources ($ sources e))
          (targets ($ targets e))
          ; Self-looping: if element presents both in sources and targets - remove it from them
          (sources-and-targets (if (and sources targets)
                                    (intersect sources targets)
                                    #f))
          (new-sources (if sources-and-targets (minus sources targets) #f))
          (new-targets (if sources-and-targets (minus targets sources) #f)))
      (cond
        ; remove whole element, if both new sources and targets empty
        ((and (empty? new-sources) (empty? new-targets))
          res)
        ; remove rudimentary left-hander
        ((and new-sources new-targets (empty? new-targets))
          (pushr res (hash-union (hash 'sources new-sources 'targets targets) e)))
        ; remove rudimentary right-hander (is this situation possible?)
        ((and new-sources new-targets (empty? new-sources))
          (pushr res (hash-union (hash 'sources sources 'targets new-targets) e)))
        ; if no other elements to connect from sources or to targets - remove this arc:
        ((and new-sources new-targets (and (empty? new-sources) (empty? new-targets)))
          res)
        ; otherwise modify its sources and targets atrributes:
        ((and sources targets)
          (pushr res (hash-union (hash 'sources new-sources 'targets new-targets) e)))
        ; simply append the next non-arc element
        (else (pushr res e))))))

(define-catch (remove-source-and-sinks context)
  (for/fold
    ((res-context context))
    ((e context))
    (cond
      ((equal? "source and sink" ($ type e)) (&&-- ($ id e) res-context))
      (else res-context))))

(define-catch (strip-off-state-prefix-if-no-duplication context)
  (define-catch (name-root aname)
    (cond
      ((not aname) aname)
      (else
        (first (string-split (->string aname) "-")))))
  (define-catch (same-name-root? name1 name2)
      (equal? (name-root name1) (name-root name2)))
  (define-catch (get-same-namer el context)
    (let* ((current-id ($ id el))
          (current-name ($ name el))
          (same-namers
            (filter-not
              (λ (x)
                (or (equal? current-id ($ id x))
                    (not (same-name-root? current-name ($ name x)))))
            context)))
      same-namers))
  (for/fold
    ((res empty))
    ((el context))
    (begin
      ; (when ($ name el) (--- "\n>>>>>" ($ name el) (get-same-namer el context) "\n"))
      (cond
        ((and ($ name el) (empty? (get-same-namer el context)))
          (pushr res (hash-union (hash 'name (name-root ($ name el))) el)))
        (else (pushr res el))))))
