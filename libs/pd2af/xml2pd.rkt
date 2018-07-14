#lang racket

(require racket/syntax)
(require sxml)
(require "../odysseus/lib/load/all.rkt")
(require "../sbgn/common.rkt")
(require "../sbgn/types.rkt")
(require "../sbgn/context.rkt")
(require "../sbgn/sexp.rkt")
(require "../sbgn/geometry.rkt")

(provide parse-pd-sbgn-ml)

; if we have class - this is a 'glyph'
(define (get-glyphs sexp)
  ; ((txpath "//v:*[@class]" '((v . "v"))) sexp))
  ((txpath "/v:sbgn/v:map/v:*[@class]" '((v . "v"))) sexp))

; for debug
(define (print-id-if id el (f (λ (x) (--- x))))
  (let ((cur-id (second (second (second el)))))
    (when (equal? cur-id id)
      (f el))))

(define (record-glyph el)
  (match el
    ; source and sink
    (`(v:glyph (@ (id ,id) (compartmentRef ,compartment) ... (class "source and sink")) ; id is the first as sxml puts it on the first place after parsing, no matter which place id occupied in xml
        (v:bbox ,(list-no-order @ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (hash
            'id (->id id)
            'name id
            'compartment (if (empty? compartment) #f (car compartment))
            'class "source and sink"
            'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))

    ; tag
    (`(v:glyph ,(list-no-order '@ `(id ,id) '(class "tag") _ ...)
        (v:label (@ (text ,name)))
        (v:bbox ,(list-no-order @ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (hash
            'id (->id id)
            'name name
            'class "tag"
            'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))

    ; complex:
    (`(v:glyph (@ (id ,id) (compartmentRef ,compartment) ... (class "complex"))
        (v:bbox ,(list-no-order @ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        (v:glyph ,(list-no-order '@ `(id ,ids-of-parts) `(class ,classes-of-parts) _ ...)
          (v:label (@ (text ,names-of-parts)))
          (v:bbox ,(list-no-order '@ `(y ,_) `(x ,_) `(w ,_) `(h ,_)))
          ,_ ...)
        ...)
        (let* (
              (x (->number x)) (y (->number y)) (w (->number w))  (h (->number h))
              (x (+ x (/ w 2.0) (/ (- W) 2.0)))
              (y (+ y (/ h 2.0) (/ (- H) 2.0)))
              (w W)
              (h H))
            (hash
              'id (->id id)
              'name (implode (sort names-of-parts string<?) "-")
              'compartment (if (empty? compartment) #f (->id (car compartment)))
              'class "complex"
              'x x 'y y 'w w 'h h))
    )

    ; compartment:
    (`(v:glyph ,(list-no-order '@ `(id ,id) '(class "compartment") _ ...)
        (v:label (@ (text ,name)) ,_ ...)
        (v:bbox ,(list-no-order @ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (hash
            'id (->id id)
            'name name
            'class "compartment"
            'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))

    ; process node:
    (`(v:glyph ,(list-no-order '@ `(id ,id) `(class ,class ...) _ ...)
        (v:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        (v:port (@ ,_ ... (id ,in-id)))
        (v:port (@ ,_ ... (id ,out-id))))
          (hash
            'id (->id id)
            'class (apply str class)
            'in-id (->id in-id) 'out-id (->id out-id)
            'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))

    ; epn with state:
    (`(v:glyph (@ (id ,id) (compartmentRef ,compartment) ... (class ,class ...))
        (v:label (@ (text ,name)))
        clone ...
        (v:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        ;; sometimes glyphs comewithout states at all:
        (v:glyph (@ (id ,state-id-extra-1) (class "state variable"))
          (v:bbox ,(list-no-order '@ `(y ,y1_s_extra_1) `(x ,x1_s_extra_1) `(w ,w1_s_extra_1) `(h ,h1_s_extra_1))))
        ...
        ; normal state glyph:
        (v:glyph (@ (id ,state-id1) (class "state variable"))
          ,state-sexp1
          (v:bbox ,(list-no-order '@ `(y ,y1_s) `(x ,x1_s) `(w ,w1_s) `(h ,h1_s))))
        ...
        ; unit of information has floating position among state variables, so include patterns for state variables above and below
        (v:glyph (@ (id ,uoi-id) (class "unit of information"))
          (v:label (@ (text ,uoi)))
          (v:bbox ,(list-no-order '@ `(y ,y_uoi) `(x ,x_uoi) `(w ,w_uoi) `(h ,h_uoi))))
        ...
        ;; sometimes glyphs come without states at all:
        (v:glyph (@ (id ,state-id-extra-2) (class "state variable"))
          (v:bbox ,(list-no-order '@ `(y ,y1_s_extra_2) `(x ,x1_s_extra_2) `(w ,w1_s_extra_2) `(h ,h1_s_extra_2))))
        ...
        (v:glyph (@ (id ,state-id2) (class "state variable"))
          ,state-sexp2
          (v:bbox ,(list-no-order '@ `(y ,y2_s) `(x ,x2_s) `(w ,w2_s) `(h ,h2_s))))
        ...
        )
          (let* ((class (apply str class))
                (uoi (if (empty? uoi) #f (car uoi))))
            (hash
              'id (->id id)
              'uoi uoi
              'class class
              'name (if (and (equal? class "macromolecule multimer") uoi)
                      (build-name name uoi)
                      name)
              'compartment (if (empty? compartment) #f (->id (car compartment)))
              'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)
              'states
              (for/fold
                ((res (hash)))
                ((i (append state-sexp1 state-sexp2)))
                (match i
                  (`(v:state (@ (value ,value) (variable ,variable))) (hash-union res (hash variable value)))
                  (else res))))))

    ; epn no state:
    (`(v:glyph (@ (id ,id) (compartmentRef ,compartment) ... (class ,class ...))
        (v:label (@ (text ,name)))
        ,clone ...
        (v:bbox ,(list-no-order @ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (hash
            'id (->id id)
            'name name
            'compartment (if (empty? compartment) #f (->id (car compartment)))
            'class (apply str class)
            'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))

    ; arc:
    ((list-no-order 'v:arc (list-no-order @ `(target ,target) `(source ,source) `(class ,class ...) _ ...)
        `(v:start (@ (y ,y1) (x ,x1)))
        `(v:end (@ (y ,y2) (x ,x2)))
        _ ...)
          (hash
            'id (set-id #:prefix "a")
            'class (apply str class)
            'sources (list (->id source))
            'targets (list (->id target))
            'x1 (->number x1) 'y1 (->number y1) 'x2 (->number x2) 'y2 (->number y2)))
    (else #f)))

(define (name-by-id id glyphs)
  ($ name (get-element-by-id id glyphs)))

(define (parse-pd-sbgn-ml xml)
  (let* (
        (namespace (first (third (ssax:xml->sxml (open-input-string xml) '()))))
        (namespace (string-replace (symbol->string namespace) ":sbgn" ""))
        (sxml (ssax:xml->sxml (open-input-string xml) `((v . ,namespace))))
        (glyphs (get-glyphs sxml))
        (glyphs (filter ProcessGlyph? (filter true? (map record-glyph glyphs))))
        (context (pushr glyphs (hash 'class "parameters" 'namespace namespace)))
        (pd-sexp `(sbgn-pd))
        (pd-sexp (add-interactions pd-sexp context))
        )
    (hash 'pd-sexp pd-sexp 'pd-context context)))
