#lang racket

(require "../../load/all.rkt")
(require "common.rkt")
(require "types.rkt")

(provide (all-defined-out))

(define (subst-ids old-ids new-id sexp)
  ; (--- old-ids new-id)
  (for/fold
    ((res sexp))
    ((old-id old-ids))
    (for/list
      ((triplet res))
      (match triplet
        (`(,(list-no-order (== old-id) rest ...) ,id2 ,id3)
              `((,new-id ,@rest) ,id2 ,id3))
        (`(,(== old-id) ,id2 ,id3)
              `(,new-id ,id2 ,id3))

        ; commented to avoid overwriting processes, after which doubles appears in the sexp
        ; (`(,id1 ,(list-no-order (== old-id) rest ...) ,id3)
        ;       `(,id1 (,new-id ,@rest) ,id3))
        ; (`(,id1 ,(== old-id) ,id3)
        ;       `(,id1 ,new-id ,id3))

        (`(,id1 ,id2 ,(list-no-order (== old-id) rest ...))
              `(,id1 ,id2 (,new-id ,@rest)))
        (`(,id1 ,id2 ,(== old-id))
              `(,id1 ,id2 ,new-id))

        (else triplet)))))

;; sexp modification
(define (replace-ids sexp ids id)
  (let loop ((sexp sexp) (ids ids))
    (cond
      ((empty? ids) sexp)
      (else
        (loop (replace-by-part sexp (car ids) id) (cdr ids))))))

(define (exclude-id sexp id)
  (tree-clean
    not-empty-list?
    empty?
    (tree-exclude sexp id)))

(define (replace-triplet sexp sample-triplet target-triplet)
  (for/fold
    ((sexp-res (list)))
    ((triplet sexp))
    (cond
      ((equal? triplet sample-triplet) (pushr sexp-res target-triplet))
      (else (pushr sexp-res triplet)))))

(define (@@<> sample-triplet target-triplet sexp )
  (replace-triplet sexp sample-triplet target-triplet))

(define (remove-triplet sexp sample-triplet)
  (let ((id-to-delete (second sample-triplet)))
    (tree-exclude
      (exclude-all sexp sample-triplet)
      id-to-delete)))

(define (@@-- sample-triplet sexp)
  (remove-triplet sexp sample-triplet))

(define (@@--- . args)
  (cond
    ((empty? args) (error "too few arguments in @@---"))
    ((= (length args) 1) (first args))
    ((= (length args) 2) (@@---1 (first args) (second args)))
    (else
      (@@--- (append (but-last (cdr args)) (list (@@---1 (car args) (last args))))))))

(define (@@---1 id sexp)
  (for/fold
    ((res-sexp empty))
    ((triplet sexp))
    (match triplet
      ; probably analyze deeper what else should be deleted- id2, id3 from res-sexp?
      (`(,id1 ,id2 ,id3) #:when (indexof? id1 id)
                            (if (one-element? id1)
                              res-sexp ; remove current triplet from the sexp
                              (pushr res-sexp (list (exclude id1 id) id2 id3)))) ; reduce
      (`(,id1 ,id2 ,id3) #:when (equal? id2 id) res-sexp) ; remove
      (`(,id1 ,id2 ,id3) #:when (indexof? id3 id)
                            (if (one-element? id3)
                              res-sexp ; remove
                              (pushr res-sexp (list id1 id2 (exclude id3 id))))) ; reduce
      (else (pushr res-sexp triplet)))))

(define (@@++ triplet sexp)
  (pushr sexp triplet))

;;;;;; PD sexp qualifiers
(define (multiarc-triplet? triplet)
  (match triplet
    (`(,id1 ,id2 ,id3)
				(and
          (list? id1)
  				(list? id3)))
    (else #f)))

(define (triplet-single-ends? triplet)
  (match triplet
    (`(,id1 ,id2 ,id3)
        (and
  				(or
            (scalar? id1)
            (and (list? id1) (= (length id1) 1)))
          (or
            (scalar? id3)
            (and (list? id3) (= (length id3) 1)))))
    (else #f)))

(define (->names sexp ctx)
	(for/fold
		((res (list)))
		((triplet sexp))
		(match triplet
			(`(,id1 ,id2 ,id3)
        ; (--- id1 id2 id3 ($ name (&& (car id1) ctx)))
				(pushr
					res
					(list
						(if (list? id1) (map (λ (x) ($ name (&& x ctx))) id1)
						 								($ name (&& id1 ctx)))
					 	id2
						(if (list? id3) (map (λ (x) ($ name (&& x ctx))) id3)
						 								($ name (&& id3 ctx))))))
			(else res))))

; checks for incoming controls for a given process <id>
(define (incoming-controls? id sexp)
  (for/or
    ((triplet sexp))
    (match triplet
      (`(,id1 ,id2 ,id3) #:when (indexof? id3 id) #t)
      (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select-glyphs subset)
  (λ (glyphs)
    (filter (λ (x) (indexof? subset ($ class x))) glyphs)))

(define (add-controls buf glyphs)
  (let ((c-arcs ((select-glyphs ModulationArc) glyphs)))
    (for/fold
      ((res buf))
      ((c c-arcs))
      (begin
      ; (--- ($ sources c))
      (pushr
        res
        (list
          ($ sources c)
          ($ id c)
          ($ targets c)))))))

(define (add-processes buf glyphs)
  (let ((p-nodes ((select-glyphs ProcessNode) glyphs))
        (p-arcs ((select-glyphs FluxArc) glyphs)))
    (for/fold
      ((res buf))
      ((p p-nodes))
      (let* ((p-id ($ id p))
            (p-in-id ($ in-id p))
            (p-out-id ($ out-id p))
            ; (_ (---  (filter
            ;             (λ (a) (--- p-in-id a)
            ;                     (or
            ;                       (equal? ($ target a) p-in-id)
            ;                       (equal? ($ target a) p-out-id)))
            ;             p-arcs)))
            (sources
                    (map
                      (λ (el) ($ source el))
                      (filter
                        (λ (a) (or
                                  (equal? ($ target a) p-in-id)
                                  (equal?  ($ target a) p-out-id)))
                        p-arcs)))
            (targets
                    (map
                      (λ (el) ($ target el))
                      (filter
                        (λ (a) (or
                                  (equal?  ($ source a) p-in-id)
                                  (equal?  ($ source a) p-out-id)))
                        p-arcs)))
            )
        (pushr res (list sources p-id targets))))))

(define (add-interactions buf glyphs)
  (add-controls
    (add-processes buf glyphs) glyphs))

(define-catch (context->sexp context)
  (let* ((sexp `(sbgn-pd))
        (sexp (add-interactions sexp context)))
    sexp))
