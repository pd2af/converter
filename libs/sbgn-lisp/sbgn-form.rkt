#lang racket

(require "../../odysseus/lib/load/all.rkt")
(require racket/syntax)
(require sxml)
(require "../../odysseus/lib/projects/pd2af/common.rkt")
(require "../../odysseus/lib/projects/pd2af/types.rkt")
(require "../../odysseus/lib/projects/pd2af/context.rkt")
(require "../../odysseus/lib/projects/pd2af/geometry.rkt")

(provide (all-defined-out))

(define reserved-keys '(id old-id in-id out-id
                        name compartment class
                        sources targets
                        cardinality
                        uoi
                        q x y w h
                        in-x in-y out-x out-y
                        complex
                        def))

; custom geometry parameters for simple diagrams
(define simple-generation (hash 'el-w 80 'el-h 40 'process-w 24 'process-h 24 'sink-w 50 'sink-h 50))
(define defaults-1 (hash-union simple-generation defaults))

(define default-compartment-name "default")

(define (NotDefaultCompartment? element)
  (let ((class ($ class element)))
    (and
      (equal? class "compartment")
      (not-equal? ($ id element) default-compartment-name))))

(define context (make-parameter empty))
(define context-compartment (make-parameter empty))
(define map-type (make-parameter #f))

(void (set-id #:prefix "glyph-" #:reset #t)) ; clear count in the very begin

(define purify-string (change-text (list
                                      (cons "\"" "")
                                      (cons "&quote;" ""))))

;; macros section
;; perhaps better to implement bricks through match forms together with other elements of pd-form
(define-syntax (pd stx)
	(syntax-case stx ()
		((_ body ...)
      #'(begin
          (map-type "process description")
          (process-lisp-sbgn '(body ...))))))

(define-syntax (af stx)
	(syntax-case stx ()
		((_ body ...)
      #'(begin
          (map-type "activity flow")
          (process-lisp-sbgn '(body ...))))))

; (define-syntax (brick-pd-transcription stx)
;   (syntax-case stx ()
;     ((_ ...)

;; helpers
(define-catch (get-cardinality cardinality-string (source-or-target #f))
  (let* (
        ; original ',' cannot be read by READ function, hence we substitute in sbgn-lisp ',' for '&' somewhere before parsing
        (cardinality (if cardinality-string (split cardinality-string "&&") #f))
        (sources-cardinality (if cardinality (first cardinality) #f))
        (sources-cardinality (if sources-cardinality (split sources-cardinality "&") #f))
        (targets-cardinality (if (or (not cardinality) (empty? (cdr cardinality))) #f (second cardinality)))
        (targets-cardinality (if targets-cardinality (split targets-cardinality "&") #f)))
    (case source-or-target
      ((sources) sources-cardinality)
      ((targets) targets-cardinality)
      (else (append sources-cardinality targets-cardinality)))))

;;;; pd-lisp -> pd-lisp
(define-catch (add-ids-to-unnamed-forms pd-form)
  (transform-list-recur
    pd-form
    (λ (x) (match x
              (`(source-and-sink ,parameters ...)
                `(,(set-id #:prefix "source-and-sink-") source-and-sink ,@parameters))
              (else x)))))

;;;; pd-lisp -> context
(define (list>=2? lst)
  (and
    (list? lst)
    (ormap (λ (x) (list? x)) lst)))

(define-catch (add-environment-parameter context parameter-hash)
  (let* ((env-element (filter (λ (e) (hash-ref e 'env #f)) context))
        (env-element-parameters (if (empty? env-element) (hash) (hash-ref env-element 'env (hash)))))
      (if (empty? env-element)
        (pushr context (hash 'env parameter-hash))
        (pushr
          (exclude-all context env-element)
          (hash 'env (hash-union parameter-hash env-element-parameters))))))

(define-catch (get-environment-parameter context parameter)
  (let* ((env-element (filter (λ (e) (hash-ref e 'env #f)) context)))
    (if (empty? env-element)
        #f
        (let* ((env-element (first env-element))
              (env-parameters (hash-ref env-element 'env))
              (parameter-value (hash-ref env-parameters parameter #f)))
          parameter-value))))

; (define (get-unique-id id compartment)
;   (format-symbol "~a-~a" compartment id))
;
; (define (extend-id context)
;   (for/list
;     ((el context))
;     (if (and ($ id el) ($ compartment el))
;       (hash-union
;         (hash 'id (get-unique-id ($ id el) ($ compartment el)))
;         el)
;       el)))

(define-catch (new-id? context element-list-form)
  (and
    (list? element-list-form)
    (let ((ids (map (λ (x) ($ id x)) context))
          (id (car element-list-form)))
      (if (indexof? ids id)
        #f
        id))))

(define (list-form-of-process-glyph? list-form)
  (match list-form
    (`(,_ ,class ,_ ...)
      (indexof? ProcessGlyph (->string class)))
    (else #f)))

(define (list-form-of-complex? list-form)
  (match list-form
    (`(,_ complex ,_ ...) #t)
    (else #f)))

(define-catch (get-node-id element-form)
  (cond
    ((list? element-form) (car element-form))
    ((scalar? element-form) element-form)
    (else #f)))

(define-catch (read-class cls)
  (let ((cls (string-replace (->string cls) "-" " ")))
    (cond
      ((indexof? ProcessGlyph cls) cls) ;  for PD
      ((indexof? ActivityGlyph cls) cls) ; for AF
      (else #f))))

(define-catch (read-parameters pars)
  (local ((define (split-parameter-form p)
          (let ((couple (split (->string p) ":")))
            (hash (->symbol (first couple)) (implode (rest couple) ":")))))
    (let loop ((new-pars (hash)) (old-pars pars))
      (cond
        ((empty? old-pars) new-pars)
        ((equal? (length old-pars) 1) (hash-union new-pars (split-parameter-form (car old-pars))))
        (else
          (let* ((next-couple (lshift old-pars 2))
                (rest-pars (ltrim old-pars 2))
                (a (first next-couple))
                (b (second next-couple)))
            (if (string? b)
              (loop
                (hash-union new-pars (hash (rtrim-symbol a) b))
                rest-pars)
              (loop
                (hash-union new-pars (hash-union (split-parameter-form a) (split-parameter-form b)))
                rest-pars))))))))

(define-catch (make-hash-nodes element-list-form context)
  (cond
    ((list? element-list-form)
      (match element-list-form
        (`(,id complex (,inner-elements ...) ,parameters ...)
          (cleanmap
            (append
              (list
                (hash-union
                  (hash 'id id 'class "complex")
                  (hash 'components (map get-node-id inner-elements))
                  (read-parameters parameters)))
              (map (λ (x)
                    (cond
                      ((scalar? x) #f)
                      (else (make-hash-nodes x context))))
                    inner-elements))))
        (`(,id ,class ,parameters ...)
          (hash-union
            (hash 'id id 'class (read-class class))
            (read-parameters parameters)))
        (else #f)))
    ((scalar? element-list-form) #f)
    (else #f)))

(define-catch (make-hash-multiarc-element element-list-form (source-ids #f) (target-ids #f))
  (let (
        (base-hash
          (match element-list-form
            (`(,class ,id ,parameters ...)
              (hash-union
                (hash 'id id 'class (read-class class))
                (read-parameters parameters)))
            (class
                (hash 'id (set-id #:prefix "multiarc-") 'class (read-class class)))
            (else #f))))
    (cond
      ((and base-hash source-ids target-ids)
          (hash-union base-hash (hash 'sources source-ids 'targets target-ids)))
      (base-hash base-hash)
      (else #f))))

(define (add-parameters-to-context id parameters-list-form context)
  (&&-> id (read-parameters parameters-list-form) context))

(define (add-node-to-context element context (compartment #f))
  (let ((new-elements (make-hash-nodes element context)))
    (cond
      ((list? new-elements) ; works just in the case when parsed list-form was a complex
        (let* ((complex (car new-elements))
              (complex-id ($ id complex))
              (components (cdr new-elements))
              (components (map (λ (x) (hash-union (hash-delete x 'compartment) (hash 'complex complex-id))) components))
              (context (pushr-unique context (cond
                                        (compartment (hash-union complex (hash 'compartment compartment)))
                                        (else complex)))))
          (append-unique context components)))
      ; single element
      (compartment
        (pushr-unique context (hash-union new-elements (hash 'compartment compartment))))
      (else
        (pushr-unique context new-elements)))))

(define-catch (add-multiarc-element-to-context element sources targets context)
  (let* (
        (sources (cond
                    ((scalar? sources) (list sources))
                    ((list-form-of-process-glyph? sources) (list (get-node-id sources)))
                    ((list? sources) (map get-node-id sources))
                    (else (get-node-id sources))))
        (targets (cond
                    ((scalar? targets) (list targets))
                    ((list-form-of-process-glyph? targets) (list (get-node-id targets)))
                    ((list? targets) (map get-node-id targets))
                    (else (get-node-id targets))))
        (source-ids (map get-node-id sources))
        (target-ids (map get-node-id targets)))
    (pushr-unique context (make-hash-multiarc-element element source-ids target-ids))))

(define-catch (append-list-form-parameter par-name par-value)
  (λ (list-form)
    (let ((frmt
            (cond
              ((string? par-value) "~a:\"~a\"")
              (else "~a:~a"))))
      (pushr list-form (format-symbol frmt par-name par-value)))))

(define (instantiate-element element-id context (compartment #f) #:id (id #f)) ; #:set-coors (set-coors #f))
  (let* (
        (element (&& element-id context))
        (new-element (hash-delete-all element '(id def)))
        (new-id (if id id (set-id #:prefix (str element-id "-"))))
        (new-element (hash-union (hash 'id new-id) new-element))
        (new-element (if compartment (hash-union (hash 'compartment compartment) new-element) new-element)))
        ; (new-element (cond
        ;                 (set-coors
        ;                   (let ((coors (calculate-element-geometry new-element)))
        ;                     (hash-union (hash 'x ($ x coors) 'y ($ y coors) 'w ($ w coors) 'h ($ h coors)) new-element)))
        ;                 (else new-element))))
    (pushr-unique context new-element)))

(define-catch (instantiate-element-form element-form context compartment)
  (let* ((element-id (get-node-id element-form))
        (element (&& element-id context))
        (instantiated-id (set-id #:prefix (str element-id "-"))))
    (cond
      ((Def? element)
        (add-parameters-to-context instantiated-id (cdr element-form) (instantiate-element element-id context compartment #:id instantiated-id )))
      (else
        context))))

(define-catch (add-nodes-to-context element-forms context (compartment #f))
  (cond
    ;; [a1 ...]
    ((scalar? element-forms) (instantiate-element-form element-forms context compartment)) ; if reference on the node, make sure it no longer contains 'def' parameter, so it can be drawn later
    ; A complex contains list of ids inside its list form. To avoid mixing this list with list form of an element, we make this special case, and check it before a general case
    ;; [(c1 complex (...) ...) ...]
    ((list-form-of-complex? element-forms)
      (add-node-to-context element-forms context compartment))
    ;; [((e1 ...) e2 ...) ...]
    ((list>=2? element-forms)
      (for/fold
        ((res-context context))
        ((element-form element-forms))
        (let* ((element-id (get-node-id element-form))
              (element-in-context (&& element-id res-context)))
          (cond
            ((new-id? context element-form)
              (add-node-to-context element-form res-context compartment))
            ((list? element-form)
              (cond
                ((Def? element-in-context)
                  (let ((instantiated-id (set-id #:prefix (str (car element-form) "-"))))
                    (add-parameters-to-context instantiated-id (cdr element-form) (instantiate-element element-id res-context compartment #:id instantiated-id )))) ; first, make copy of defed element and then add parameters to it
                (else
                  (add-parameters-to-context (car element-form) (cdr element-form) res-context)))) ; not new id but new parameters for already existed instantiated element
            ((scalar? element-form) (instantiate-element-form element-form context compartment))
            (else
              res-context)))))
    ;; [(e1 ...) ...]
    ((new-id? context element-forms)
      (add-node-to-context element-forms context compartment))
    ;; [(e1 e2 e3) ...]
    ((list? element-forms)
      (for/fold
        ((res-context context))
        ((element-id element-forms))
        (let* ((element (&& element-id context)))
          (if (and element (Def? element))
            (instantiate-element element-id res-context compartment) ; if references on the nodes, make sure they no longer contain 'def' parameter, so they can be drawn later; and also add coordinates
            res-context))))
    ;; ?
    (else context)))

(define-catch (read-result-file sbgn-form)
  (match sbgn-form
    ((list-no-order `(->file self.sbgn) _ ...)
      (let ((new-filename (format "~a.sbgn" (path->string (find-system-path 'run-file)))))
              (context (add-environment-parameter (context) (hash 'filepath new-filename)))))
    ((list-no-order `(->file ,filepath) _ ...)
      (context (add-environment-parameter (context) (hash 'filepath filepath))))
    (else (context))))
      ; (error "Don't know where to output the result. Add (->file <filepath>) form?"))))

(define-catch (read-defs sbgn-form)
  (let loop ((next-sbgn-form sbgn-form))
    (cond
      ((empty? next-sbgn-form) void)
      ((equal? (and (list? (car next-sbgn-form)) (caar next-sbgn-form)) 'defs)
        (set-id #:prefix "glyph-")
        (for
          ((d (cdar next-sbgn-form)))
          (match d
            (`(sample ,id ,class ,parameters ...)
                (let ((parameters (append parameters '(def:#t))))
                  (context (append-unique
                              (context)
                              (add-nodes-to-context `(,id ,class ,@parameters) (context) #f)))))
            (else
              void))))
      (else
        (loop (cdr next-sbgn-form))))))

(define-catch (read-compartments sbgn-form)
  (let loop ((next-sbgn-form sbgn-form))
    (let* (
          (section-name (and (not (empty? next-sbgn-form)) (list? (car next-sbgn-form)) (caar next-sbgn-form)))
          (compartment-prefix (->string section-name))
          (section-parsed (split (->string section-name) ":"))
          (compartment (and (list? section-parsed) (> (length section-parsed) 1) (equal? (second section-parsed) "compartment")))
          (compartment-name (if compartment (first section-parsed) #f))
          (compartment-id (if (equal? compartment-name default-compartment-name)
                            default-compartment-name
                            (set-id #:prefix (str compartment-name "-"))))
          )
      (cond
        ((empty? next-sbgn-form) void)
        (compartment
          (context
              (pushr
                (context)
                (hash 'id compartment-id 'class "compartment" 'name compartment-name)))
          (for
            ((triplet (cdar next-sbgn-form)))
            (let-values (((sources op targets)
                            (match triplet
                              (`(,sources
                                      ,op
                                          ,targets)
                                  #:when (or (ArcForm? op) (ActivityArcForm? op))
                                  (values sources op targets))
                              (`(,element ...)
                                  (values element #f #f))
                              (else
                                  (values #f #f #f)))))
                  (when sources
                            (context
                              (append-unique
                                (context)
                                (add-nodes-to-context sources (context) compartment-id))))
                  (when (and op sources targets)
                            (context
                              (append-unique
                                (context)
                                (add-multiarc-element-to-context op sources targets (context)))))
                  (when targets
                            (context
                              (append-unique
                                (context)
                                (add-nodes-to-context targets (context) compartment-id))))))
          (loop (cdr next-sbgn-form)))
        (else
          (loop (cdr next-sbgn-form)))))))

(define-catch (sbgn-lisp->context sbgn-form)
  (set-id #:prefix "glyph-")
  (parameterize ((context empty))
        (read-defs sbgn-form)
        (read-compartments sbgn-form)
        (read-result-file sbgn-form)
        (context)))

;;;; context -> context
(define-catch (make-single-arcs-from-multiarc el)
  (let* (
        (in-sources ($ sources el))
        (in-target ($ in-id el))
        (out-source ($ out-id el))
        (out-targets ($ targets el))
        (sources ($ out-id el))
        (in-class (cond
                    ((LogicalOperator? el) "logic arc")
                    ((ProcessNode? el) "consumption")
                    (else "equivalence arc")))
        (out-class (cond
                    ((LogicalOperator? el) ($ modulation el))
                    ((ProcessNode? el) "production")
                    (else ($ class el))))
        (cardinality ($ cardinality el))
        (sources-cardinality (get-cardinality cardinality 'sources))
        (targets-cardinality (get-cardinality cardinality 'targets))
        (source-arcs
            (for/list
              ((source in-sources))
              (let* (
                    (in-arc-hash (hash 'class in-class 'id (set-id #:prefix (str (string-replace in-class " " "-") "-")) 'source source 'target in-target))
                    (in-arc-hash (if (and sources-cardinality (not (empty? sources-cardinality)))
                                      (hash-union
                                        (hash 'cardinality (nth sources-cardinality (indexof in-sources source)))
                                        in-arc-hash)
                                      in-arc-hash)))
                  in-arc-hash)))
        (target-arcs
            (for/list
              ((target out-targets))
              (let* (
                    (out-arc-hash (hash 'class out-class 'id (set-id #:prefix (str (string-replace out-class " " "-")"-")) 'source out-source 'target target))
                    (out-arc-hash (if (and targets-cardinality (not (empty? targets-cardinality)))
                                      (hash-union
                                        (hash 'cardinality (nth targets-cardinality (indexof out-targets target)))
                                        out-arc-hash)
                                      out-arc-hash)))
                  out-arc-hash))))
    (append source-arcs target-arcs)))

(define-catch (expand-context-edges context)
  (for/fold
    ((res empty))
    ((el context))
    (cond
      ((ProcessNode? el)
        (let* (
              (id ($ id el))
              (in-id (format-symbol "~a-in" id))
              (out-id (format-symbol "~a-out" id))
              (process-node (hash-union el (hash 'in-id in-id 'out-id out-id)))
              (arcs (make-single-arcs-from-multiarc process-node)))
            (append-unique res (list process-node) arcs)))
      ((and
        (or* ModulationArc? ActivityArc? el)
        (or
          (not (one-element? ($ sources el)))
          (not (one-element? ($ targets el)))))
        ; (error (format "Should not be several targets for a Control Arc. Check <~a> Arc" ($ id el))))
        (let* (
              (id (set-id #:prefix "modulation-"))
              (in-id (format-symbol "~a-in" id))
              (out-id (format-symbol "~a-out" id))
              (and-gate (hash 'class "and" 'modulation ($ class el) 'id id 'in-id in-id 'out-id out-id 'sources ($ sources el) 'targets ($ targets el)))
              (arcs (make-single-arcs-from-multiarc and-gate))
              )
            (append-unique res (list and-gate) arcs)))
      ((or* ModulationArc? ActivityArc? el)
        (let ((id (set-id #:prefix "modulation-")))
          (pushr res (hash-union
                        (hash 'id id 'source (car ($ sources el)) 'target (car ($ targets el)))
                        el))))
      (else (pushr res el)))))

(define (get-random-coor limit)
  (random limit))

(define-catch (get-coors-from-q q (x-w 0) (x-h 0))
  (let* ((q-w ($ square-w defaults-1))
        (q-h ($ square-h defaults-1)))
    (if q
      (let* ((qs (split q "-"))
            (qx (->number (first qs)))
            (qy (->number (second qs))))
        (centrify x-w x-h (* qx (* 0.5 q-w)) (* qy q-h) q-w q-h))
      (hash 'x ($ default-x defaults-1) 'y ($ default-y defaults-1)))))

(define-catch (get-compartment-coors compartment-id context)
    (let*-values
    (
    ((elements-of-compartment) (filter (λ (x) (equal? ($ compartment x) compartment-id)) context))
    ((x-min y-min x-max y-max)
      (for/fold
        ((x-min #f) (y-min #f) (x-max #f) (y-max #f))
        ((el elements-of-compartment))
        (let (
              (el-x (->number ($ x el)))
              (el-y (->number ($ y el)))
              (el-w (->number ($ w el)))
              (el-h (->number ($ h el))))
          (values
            (if (or (not x-min) (< el-x x-min)) el-x x-min)
            (if (or (not y-min) (< el-y y-min)) el-y y-min)
            (if (or (not x-max) (> (+ el-x el-w) x-max)) (+ el-x el-w) x-max)
            (if (or (not y-max) (> (+ el-y el-h) y-max)) (+ el-y el-h) y-max)))))
    ((x y w h) (values
              (- x-min ($ compartment-margin-x defaults-1))
              (- y-min ($ compartment-margin-y defaults-1))
              (+ (- x-max x-min) (* 2 ($ compartment-margin-x defaults-1)))
              (+ (- y-max y-min) (* 2 ($ compartment-margin-y defaults-1)))))
    ((x y) (values
              (if (< x 0) 0 x)
              (if (< y 0) 0 y))))
    (hash 'x x 'y y 'w w 'h h)))

(define-catch (calculate-complex-components-xy complex)
  (let* (
        (component-ids ($ components complex))
        (complex-x ($ x complex))
        (complex-y ($ y complex))
        (n (length component-ids))
        (el-w ($ el-w defaults-1))
        (el-h ($ el-h defaults-1))
        ; (complex-w (case n
        ;               ((1 2 3) (+ el-w 20))
        ;               (else (* 2 (+ el-w 20)))))
        (complex-w (+ el-w 20))
        ; (complex-h (case n
        ;               ((1 2 3) (* n (+ el-h 10)))
        ;               (else (* (/c n 2) (+ el-h 10)))))
        (complex-h (* n (+ el-h 10)))
        )
        (let loop ((res-hash (hash)) (ids component-ids))
          (if (empty? ids)
            res-hash
            (let* ((count (- n (length ids)))
                  ; (column (if (odd? count) 1 0))
                  (column 0)
                  ; (row (/f count 2))
                  (row count)
                  (x (+ (+ complex-x 10) (* el-w column)))
                  (y (+ (+ complex-y 5) (* (+ el-h ($ component-vertical-gap defaults-1)) row))))
              (loop
                (hash-union res-hash (hash (car ids) (hash 'x x 'y y 'w el-w 'h el-h)))
                (cdr ids)))))))

(define-catch (calculate-element-geometry el)
  (let* (
        (w (cond
            ((or* ActivityNode? NamedEPN? el) ($ el-w defaults-1))
            ((SourceSink? el) ($ sink-w defaults-1))
            (else ($ el-w defaults-1))))
        (h (cond
            ((or* ActivityNode? NamedEPN? el) ($ el-h defaults-1))
            ((SourceSink? el) ($ sink-h defaults-1))
            (else ($ el-h defaults-1))))
        (coors (get-coors-from-q ($ q el) w h))
        (x (or ($ x coors) (get-random-coor W)))
        (y (or ($ y coors) (get-random-coor H)))
        )
    (hash 'x x 'y y 'w w 'h h)))

(define-catch (calculate-port-element-geometry multiarc context)
  (let* (
        (el multiarc)
        (id ($ id el))
        (type ($ type multiarc))
        (source-ids ($ sources el))
        (sources (map (λ (source-id) (&& source-id context)) source-ids))
        (target-ids ($ targets el))
        (targets (map (λ (target-id) (&& target-id context)) target-ids))
        (a-mass-center (mass-center (append sources targets)))
        (xc ($ x a-mass-center))
        (yc ($ y a-mass-center))
        (w (if (LogicalOperator? multiarc) ($ gate-w defaults-1) ($ process-w defaults-1)))
        (h (if (LogicalOperator? multiarc) ($ gate-h defaults-1) ($ process-h defaults-1)))
        (x (- xc (/ w 2.0)))
        (y (- yc (/ h 2.0)))
        (port-coors (calculate-port-coors sources x y ($ gate-margin defaults-1) #:w w #:h h))
        )
    (hash-union el
      (hash 'x x 'y y 'w w 'h h
            'in-x ($ port-in-x port-coors) 'in-y ($ port-in-y port-coors)
            'out-x ($ port-out-x port-coors) 'out-y ($ port-out-y port-coors)))))

(define-catch (expand-context-xy context)
  (let* (
        (context
            ;; calculate geometry for epn nodes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((or* EPN? ActivityNode? el)
                  (let ((coors (calculate-element-geometry el)))
                    (pushr res (hash-union el (hash 'x ($ x coors) 'y ($ y coors) 'w ($ w coors) 'h ($ h coors))))))
                (else (pushr res el)))))
        (context
            ;; calculate xy for subelements in the complexes
            (map
              (λ (el)
                (cond
                  ((Complex? el) (hash-union
                                    (hash 'components (calculate-complex-components-xy el))
                                    el))
                  (else el)))
              context))
        (context
            ;; calculate sizes of complexes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((Complex? el)
                  (let* (
                        (components (hash-values ($ components el))))
                    (let-values (((xmin ymin xmax ymax)
                                    (for/fold
                                      ((xmin ($ x el)) (ymin ($ y el)) (xmax (+ ($ x el) ($ w el))) (ymax (+ ($ y el) ($ h el))))
                                      ((component components))
                                      (let* ((x ($ x component))
                                            (y ($ y component))
                                            (w ($ w component))
                                            (h ($ h component)))
                                        (values
                                          (if (< x xmin) x xmin)
                                          (if (< y ymin) y ymin)
                                          (if (> (+ x w) xmax) (+ x w) xmax)
                                          (if (> (+ y h) ymax) (+ y h) ymax))))))
                    (pushr res (hash-union (hash 'x xmin 'y ymin 'w (+ 10 (- xmax xmin)) 'h (+ 10 (- ymax ymin))) el)))))
                (else (pushr res el)))))
        (context
          ;; calculate location for process nodes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((ProcessNode? el) (pushr res (calculate-port-element-geometry el context)))
                (else (pushr res el)))))
        (context
          ;; calculate location for logical nodes, right after we know coordinates of process nodes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((LogicalOperator? el) (pushr res (calculate-port-element-geometry el context)))
                (else (pushr res el)))))
        (context
            ;; calculate start and end points for arcs
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((or* FluxArc? LogicArc? el)
                  (let* (
                        (source (&& ($ source el) context '(id in-id out-id)))
                        (target (&& ($ target el) context '(id in-id out-id)))
                        (coors (calculate-arc-coors ($ x source) ($ y source) ($ w source) ($ h source) ($ x target) ($ y target) ($ w target) ($ h target) (list ($ id source) ($ id target))))
                        (x1 (if ($ out-id source) ($ out-x source) ($ x1 coors)))
                        (y1 (if ($ out-id source) ($ out-y source) ($ y1 coors)))
                        (x2 (if ($ in-id target) ($ in-x target) ($ x2 coors)))
                        (y2 (if ($ in-id target) ($ in-y target) ($ y2 coors)))
                        )
                  (pushr res (hash-union el (hash 'x1 x1 'y1 y1 'x2 x2 'y2 y2)))))
                ((or* ModulationArc? ActivityArc? el)
                  (let* (
                        (source (&& ($ source el) context '(id in-id out-id)))
                        (target (&& ($ target el) context '(id)))
                        (coors (calculate-arc-coors ($ x source) ($ y source) ($ w source) ($ h source) ($ x target) ($ y target) ($ w target) ($ h target) (list ($ id source) ($ id target))))
                        (x1 ($ x1 coors))
                        (y1 ($ y1 coors))
                        (x2 ($ x2 coors))
                        (y2 ($ y2 coors))
                        )
                  (pushr res (hash-union el (hash 'x1 x1 'y1 y1 'x2 x2 'y2 y2)))))
                (else (pushr res el)))))
        (context
            ;; calculate sizes of compartments
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((Container? el)
                  (let* (
                        (coors (get-compartment-coors ($ id el) context))
                        )
                    (pushr res (hash-union el (hash 'x ($ x coors) 'y ($ y coors) 'w ($ w coors) 'h ($ h coors))))))
                (else (pushr res el)))))
        )
    context))

;;;; context -> sxml
(define-catch (build-complex element context)
  (let* (
        (complex-id (->string ($ id element)))
        (compartment ($ compartment element))
        (compartment (if (equal? compartment default-compartment-name) #f compartment))
        (compartment (if compartment `(compartmentRef ,(->string compartment)) '$f))
        (components ($ components element))
        (name ($ name element))
        (label (if name
                      `(label (@ (text ,(purify-string (->string name)))))
                      '$f))
        (bbox `(bbox (@ (x ,($ x element)) (y ,($ y element)) (w ,($ w element)) (h ,($ h element)))))
        (internal-elements (filter (λ (x) (indexof? (hash-keys components) ($ id x))) context))
        (internal-elements (map (λ (x) (hash-substitute
                                          (hash-delete x 'compartment) ; remove compartment info from the element as it is inside a complex
                                          (list
                                            (cons 'id (format-symbol "~a-~a" ($ id x) complex-id)) ; mofify id of the element, to make it unique (in the case element is defined by def for use in the several complexes)
                                            (cons 'old-id ($ id x)))))
                                internal-elements))
        (context (append-unique context internal-elements))
        (content
          (for/fold
            ((sxml empty))
            ((c internal-elements))
            (let* (
                  (component-id ($ id c))
                  (coors (hash-ref components ($ old-id c)))
                  (c (hash-union coors c))
                  (subglyph (cond
                              ((Complex? c) (build-complex c context))
                              ((Node? c) (build-node c))
                              (else '$f)))
                  )
              (pushr sxml subglyph)))))
    (format-list
      `(glyph (@ (class "complex") (id ~a) ~a)

        ~a ; label
        ~a ; bbox
        ~@a) ; content
      complex-id
      compartment

      label
      bbox
      content)))

(define-catch (build-node element #:node-with-ports? (node-with-ports? #f))
  (let* (
        (class ($ class element))
        (id (->string ($ id element)))
        (compartment ($ compartment element))
        (complex ($ complex element))
        (compartment (if (and compartment (not complex) (not (equal? compartment default-compartment-name))) `(compartmentRef ,(->string compartment)) '$f))
        (name (purify-string (->string (or ($ name element) ($ id element)))))
        (label (if (or* NamedEPN? Container? ActivityNode? element) `(label (@ (text ,name))) '$f))
        (el-w ($ el-w defaults-1)) (el-h ($ el-h defaults-1))
        (x (or ($ x element) 0)) (y (or ($ y element) 0)) (w (or ($ w element) el-w)) (h (or ($ h element) el-h))
        (bbox (if (or* Node? Container? ActivityNode? element) `(bbox (@ (x ,x) (y ,y) (w ,w) (h ,h))) '$f))
        (in-port (if node-with-ports? `(port (@ (id ,(->string ($ in-id element))) (x ,($ in-x element)) (y ,($ in-y element)))) '$f))
        (out-port (if node-with-ports? `(port (@ (id ,(->string ($ out-id element))) (x ,($ out-x element)) (y ,($ out-y element)))) '$f))
        (uoi ($ uoi element))
        (uoi (and uoi (purify-string ($ uoi element)) uoi))
        (uoi-w ($ uoi-w defaults-1)) (uoi-h ($ uoi-h defaults-1))
        (uoi-x (+ x (/ (- w uoi-w) 2.0)))
        (uoi-y (- y (/ uoi-h 2.0)))
        (entity ($ type element))
        (entity (and entity (string-replace (str entity) "-" " ")))
        (entity-glyph (if entity
                          `(entity (@ (name ,entity)))
                          '$f))
        (entity (if (equal? entity "macromolecule") #f entity))
        (uoi-glyph (if (and (or* NamedEPN? ActivityNode? element) (or uoi entity))
                          (format-list
                            `(glyph (@ (id ,(->string (set-id #:prefix (str id "-uoi-")))) (class "unit of information"))
                              (label (@ (text ,(if uoi uoi ""))))
                              ~a
                              (bbox (@ (x ,uoi-x) (y ,uoi-y) (w ,uoi-w) (h ,uoi-h))))
                            entity-glyph)
                        '$f))
        (state-variables (minus (hash-keys element) reserved-keys))
        (state-glyphs (if (and (NamedEPN? element) (not (empty? state-variables)))
                        (for/list
                          ((state-variable state-variables) (i (in-naturals)))
                          (let* (
                                (sv-x (+ x (list-ref ($ state-variable-positions defaults-1) i)))
                                (hh (if (equal? ($ state-position defaults) 'top) 0 h)) ; state on top or on bottom
                                (sv-y (- (+ y hh) (/ ($ state-variable-d defaults-1) 2.0)))
                                (default-variable? (indexof? ($ default-state-variables defaults-1) state-variable))
                                (sv-w (if default-variable?
                                          ($ state-variable-d defaults-1)
                                          ($ state-variable-w defaults-1)))
                                (sv-h (if default-variable?
                                          ($ state-variable-d defaults-1)
                                          ($ state-variable-h defaults-1)))
                                (state-value (purify-string (->string (hash-ref element state-variable))))
                                (state-value (if (equal? state-value "empty") "" state-value))
                                (state (if default-variable?
                                          `(state (@ (value ,state-value) (variable "")))
                                          `(state (@ (value ,state-value) (variable ,(->string state-variable)))))))
                          `(glyph (@ (id ,(->string (set-id #:prefix (str id "-state-variable-")))) (class "state variable"))
                              ,state
                              (bbox (@ (x ,sv-x) (y ,sv-y) (w ,sv-w) (h ,sv-h))))))
                        '$f))
        )
    (format-list
      `(glyph (@ (class ~a) (id ~a) ~a)
        ~a ; label
        ~a ; bbox
        ~a ; in-port
        ~a ; out-port
        ~a ; uoi-glyph
        ~@a ; state-glyphs
        )
      class
      id
      compartment

      label
      bbox
      in-port
      out-port
      uoi-glyph
      state-glyphs)))

(define-catch (build-arc element)
  (let* (
        (class ($ class element))
        (id (->string ($ id element)))
        (source (->string ($ source element)))
        (target (->string ($ target element)))
        (cardinality ($ cardinality element))
        (cardinality-glyph (if cardinality
                                    `(glyph (@ (id ,(->string (set-id #:prefix (str id "-cardinality-")))) (class "cardinality"))
                                        (label (@ (text ,cardinality)))
                                        (bbox (@ (x "0") (y "0") (w "0") (h "0"))))
                                    '$f))
        (start `(start (@ (x ,($ x1 element)) (y ,($ y1 element)))))
        (end `(end (@ (x ,($ x2 element)) (y ,($ y2 element)))))
        )
    (format-list
      `(arc (@ (class ~a) (id ~a) (source ~a) (target ~a))
        ~a ; cardinality
        ~a ; start
        ~a ; end
        )
        class
        id
        source
        target

        cardinality-glyph

        start
        end)))

(define-catch (form->context sbgn-form)
  (expand-context-xy
    (expand-context-edges
      (sbgn-lisp->context sbgn-form))))

(define-catch (sbgn-lisp->sxml context)
  (let* (
        (map-type-now (or (map-type) "process description"))
        (xmlns "http://sbgn.org/libsbgn/0.2")
        (W ($ W defaults-1))
        (H ($ H defaults-1))
        (el-w ($ el-w defaults-1))
        (el-h ($ el-h defaults-1))
        (process-w ($ process-w defaults-1))
        (process-h ($ process-h defaults-1))
        (sxml-wrap `(sbgn (@ (xmlns ,xmlns)) (map (@ (language ,map-type-now)) ~@a)))
        (first-level-elements (clean Env? (clean Def? (clean InternalEPN? context))))
        (body
          (for/fold
            ((sxml empty))
            ((element first-level-elements) (i (in-naturals)))
            (let ((sxml-element
                    (cond
                      ((Complex? element) (build-complex element context))
                      ((or* LogicalOperator? ProcessNode? element) (build-node element #:node-with-ports? #t))
                      ((Arc? element) (build-arc element))
                      ((ActivityArc? element) (build-arc element))
                      ((or* Node? ActivityNode? (λ (x) (and* Container? NotDefaultCompartment? x)) element) (build-node element))
                      (else #f))))
              (if sxml-element
                (pushr sxml sxml-element)
                sxml)))))
    (format-list
      sxml-wrap
  		(order-by-tag '(glyph arc) body))))

; form -> xml-string
; (define-catch (get-lisp-sbgn sbgn-form)
;   (let* (
;         (sbgn-form (add-ids-to-unnamed-forms sbgn-form))
;         (context (form->context sbgn-form))
;         (sxml (sbgn-lisp->sxml context))
;         (xml
;         	(format "~a~n~a"
;         		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
;         		(srl:sxml->xml sxml))))
;     xml))

; writes to file
(define-catch (process-lisp-sbgn sbgn-form)
  (let* ((sbgn-form (add-ids-to-unnamed-forms sbgn-form))
        (context (form->context sbgn-form))
        (filepath (get-environment-parameter context 'filepath))
        (sxml (sbgn-lisp->sxml context))
        (xml
        	(format "~a~n~a"
        		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        		(srl:sxml->xml sxml))))
    xml))
    ; (write-file filepath xml)))
