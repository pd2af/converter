#lang racket

(require "../../libs/odysseus/lib/load/all.rkt")
(require "../../libs/odysseus/graphics/svg.rkt")
(require "../../libs/odysseus/graphics/color.rkt")
(require "../../libs/odysseus/knowledge-base/tab-tree.rkt")
(require "../../libs/odysseus/knowledge-base/html.rkt")
(require "../../libs/sbgn-lisp/sbgn-form.rkt")
(require "../../libs/pd2af/translate.rkt")
(require "../../libs/pd2af/test.rkt")
;
(provide (all-defined-out))
(provide (all-from-out "../../libs/pd2af/translate.rkt" "../../libs/pd2af/test.rkt"))

(define ns (module->namespace (string->path "c:/denis/denis_core/projects/pd2af/libs/sbgn-lisp/sbgn-form.rkt")))

; (define newt-prefix-url "http://web.newteditor.org/?URL=https://denis-shirshov.ru/for/alexander-mazein/rules")
(define newt-prefix-url "http://web.newteditor.org/?URL=http://pd2af.rusvegia.com/rules-auto")

(define (exists? var)
  (and var (non-empty-string? var)))

(define (preprocess-lisp-sbgn astr)
  (let* ((symbols-to-change
          (list
            (cons "`" "\"")
            (cons "\"" "\\\"")
            (cons "," "&")))
        (res (for/fold
                ((res astr))
                ((s symbols-to-change))
                (string-replace res (car s) (cdr s)))))
    (read (open-input-string res))))

(define-catch (get-pd-str basepath item)
  (let* (
        (folder ($ folder item))
        (folder-path (str basepath "/" folder))
        (pd-coors ($ pd-coors item))
        (pd-str ($ pd item))
        (compartments ($ compartments item))
        (pd-full ($ pd-full item))
        (pd-file ($ pd-file item))
        (diagram-files-in-folder ($ diagram-files-in-folder item))
        (pd-full-str (or
                        (and pd-full (format "(pd ~a)" pd-full))
                        (and pd-file (read-file pd-file))
                        (and diagram-files-in-folder (read-file (str folder-path "/diagram.pd")))
                        (if compartments
                          (format "(pd ~a ~a)" pd-coors pd-str)
                          (format "(pd (default:compartment ~a ~a))" pd-coors pd-str)))))
      pd-full-str))

(define-catch (get-af-str basepath item)
  (let* (
        (folder ($ folder item))
        (folder-path (str basepath "/" folder))
        (af-coors ($ af-coors item))
        (af-str ($ af item))
        (compartments ($ compartments item))
        (af-full ($ af-full item))
        (af-file ($ af-file item))
        (diagram-files-in-folder ($ diagram-files-in-folder item))
        (af-full-str (or
                        (and af-full (format "(af ~a)" af-full))
                        (and af-file (read-file af-file))
                        (and diagram-files-in-folder (read-file (str folder-path "/diagram.af")))
                        (if compartments
                          (format "(af ~a)" af-str)
                          (format "(af (default:compartment ~a ~a))" af-coors af-str)))))
      af-full-str))


(define-catch (create-sbgn-ml-files basepath item)
  (let* (
        (folder ($ folder item))
        (folder-path (str basepath "/" folder))
        (pd-full-str (get-pd-str basepath item))
        (pd-xml (eval (preprocess-lisp-sbgn pd-full-str) ns))

        (af-full-str (get-af-str basepath item))
        (af-xml (eval (preprocess-lisp-sbgn af-full-str) ns))

        (translated-af-context (try
                                  (translate-pd pd-full-str)
                                  (catch empty)))
        (canonical-af-context (get-af-context af-full-str))
        ; (translatable? (contexts-equal? translated-af-context canonical-af-context))
        )
    (write-file-to-dir #:dir folder-path #:file "pd.sbgn" pd-xml)
    (write-file-to-dir #:dir folder-path #:file "af.sbgn" af-xml)
    ))

(define (calculate-img-width img-path)
  (if (file-exists? img-path)
    (let* ((image-geometry (get-image-geometry img-path))
          (w0 ($ width image-geometry))
          (w (* 0.8 w0)))
      (~a w))
    (~a 0)))

;; Gantt diagrams

(define (get-start-time element jobs)
  (let* (
        (start ($ start element))
        (after ($ after element))
        (after-ids (if after (string-split after ",") empty))
        (afters (map (λ (id) (get-item-by-id-from-the-list jobs id)) after-ids))
        (start
          (if (and afters (not-empty? afters))
            (car (sort (map (λ (x) (get-end-time x jobs)) afters) d>))
            start)))
    start))

(define (get-end-time element jobs)
  (let* (
        (start (get-start-time element jobs))
        (len ($ length element))
        (len (if len (days-count len) 0))
        (end (or ($ end element) (d+ start len))))
    end))
;
(define-catch (get-first-after element jobs)
  (let* (
        (after-ids ($ after element))
        (after-ids (if after-ids
                      (string-split ($ after element) ",")
                      #f))
        (afters (if after-ids
                  (map (λ (id) (get-item-by-id-from-the-list jobs id)) after-ids)
                  #f))
        (result
          (if afters
              (apply min (map (λ (x) ($ _order x)) afters))
              #f)))
    result))

(define-catch (draw-gantt tabtree-file path svg-width svg-height)
  ; constants
  (define y0 50)
  (define track-h 30) ; height per one job bar
  (define colors (string-split "black #0096B2 #CC46C7 #89B836 #F29A4C #B22E2A #4C57DB #74D95F" " "))
  ; (define colors (string-split "black #3EA6B2 #5656C2 #559139 #618AB8 #984EB8 #53AB4D" " "))
  (let* ((tabtree (parse-tab-tree tabtree-file))
        (path (split path "."))
        (jobs (get-$3 path tabtree))
        (start-times (map (λ (x) (get-start-time x jobs)) jobs))
        (end-times (map (λ (x) (get-end-time x jobs)) jobs))
        (min-start-time (apply min (map date->days start-times)))
        (max-end-time (apply max (map date->days end-times)))
        (days-length (- max-end-time min-start-time))
        (step (/f svg-width days-length))
        (users-list empty)
        )
    (str
        ; ticks
        (for/fold
          ((res ""))
          ((tick (range 0 (inc days-length))))
          (let* ((aday (+ min-start-time tick))
                (adate (days->date aday))
                (full-adate (days->date aday #:year #t))
                (is-leap-year? (leap-year? (->number (third (split full-adate ".")))))
                (adate-day (->number (first (split adate "."))))
                (adate-month (second (split adate ".")))
                (adate-month-abbr (nth months (->number adate-month)))
                (first-day? (equal? 1 adate-day))
                (adate (->string adate-day))
                (canvas-height (- svg-height 50))
                (y1 20))
            (str
              res "\n"
              (line 'stroke "grey" 'stroke-width "1" 'x1 (* step tick) 'y1 y1 'x2 (* step tick) 'y2 (+ y1 canvas-height)) "\n"
              (when (holiday? (days->date aday #:year #t))
                (rect 'x (* step tick) 'y y1 'width step 'height canvas-height 'class "holiday-bg"))
              "\n"
              ; today
              (when (equal? (current-date) full-adate)
                (rect 'x (* step tick) 'y y1 'width step 'height canvas-height 'class "today-bg"))
              (when first-day? (text (@ 'x (+ (* 0.5 step) (* step tick)) 'y 12 'class "labels") adate-month-abbr))
              (text
                (@ 'x (+ (* 0.5 step) (* step tick)) 'y 33 'class "labels")
                adate)
              "\n"
            )))
        ; bars
        (for/fold
          ((res ""))
          ((job jobs) (i (in-naturals)))
          (let* (
                (user ($ user job))
                (user (and user (car (split user ","))))
                (_ (set! users-list (if (indexof? users-list user) users-list (pushr users-list user))))
                (color (if user (list-ref colors (indexof users-list user)) (list-ref colors 0)))
                (color (if (equal? ($ status job) "done") "#bbb" color)) ; (color/shadow (color/desaturate color -55) 15)
                (start-date (get-start-time job jobs))
                (end-date (get-end-time job jobs))
                (start (- (date->days start-date) min-start-time))
                (end (- (date->days end-date) min-start-time))
                (x (* step start))
                (dx (* step end))
                (width (- dx x))
                (y (* track-h i))
                (min_order (apply min (map (λ (x) ($ _order x)) jobs)))
                (first-after (get-first-after job jobs))
                (y-first-after (if first-after (* track-h (- first-after min_order)) #f))
                (h 25)
                )
            (str
              res "\n"
              (g (@ 'transform (format "translate(0,~a)" y0))
                (rect 'x x 'y y 'width width 'height h 'fill color) "\n"
                (text (@ 'x (+ x 5) 'y (+ y 17) 'class "job_title") ($ title job)) "\n"
                ; convergence line
                (when y-first-after
                  (str (line 'x1 x 'y1 (+ y h) 'x2 x 'y2 y-first-after 'class "convergence_line") "\n"))
              )
            )
          ))
        ; legend
        (for/fold
          ((res ""))
          ((user users-list) (i (in-naturals)))
          (str
            res "\n"
            (rect 'x (* i 100) 'y (- svg-height 20) 'width 20 'height 10 'fill (list-ref (rest colors) i))
            (text (@ 'x (+ 25 (* i 100)) 'y (- svg-height 10)) user))))))
