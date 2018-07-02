#lang racket
(require web-server/servlet web-server/servlet-env web-server/formlets)
(require "../lib/load/all.rkt")
(require "../lib/projects/pd2af/odysseus/system.rkt")
(require "../settings.rkt")
; (require "../../../../odysseus/lib/load/all.rkt")
(require "../sbgn/xml2pd.rkt")
(require "../sbgn/pd2pd.rkt")
(require "../sbgn/pd2af.rkt")
(require "../sbgn/af2af.rkt")
(require "../sbgn/af2xml.rkt")

(provide (all-defined-out))

(define sample '(sbgn-pd (S1 x P1) (enzyme catalysis x)))

(define (make-json-string val)
  (string->bytes/utf-8
    (format "~a"
      (cond
        ((not val) "Invalid format")
        (else (alist->json val))))))

; sudo -b nohup racket pd2af/server/start-server.rkt

; TODO: rewrite through translate-pd
(define (translator req)
  (set-id #:reset #t) ; reset counter, otherwise it will accumulate values up to thousands and godzillions
  (let* (
        (req (request-bindings req))
        (sbgn_ml (cdr (assoc 'file req)))
        (filename (cdr (assoc 'filename req)))
        (filename (first (split filename ".")))
        (sbgn_ml (bytes->string/utf-8 sbgn_ml))

        (pd (parse-pd-sbgn-ml sbgn_ml))

				(pd (remove-simple-chemicals pd))

        (pd (logical-nodes-2-multiarcs pd))
				(pd (process-nodes-2-multiarcs pd))

        (pd (fold-same-signatures pd))

        (pd-sexp ($ pd-sexp pd))
        (pd-context ($ pd-context pd))

        (af-context (pd2af pd-sexp pd-context))
				(af-context (collapse-same-name-chains af-context))
				(af-context (remove-self-loops af-context))
				(af-context (remove-source-and-sinks af-context))

				(af-xml (af2xml af-context))

        (generated-filename (format "~a.af.sbgn" filename))
        (file-url (str (get-settings 'root-url) "/generated/" generated-filename))
        (file-path (case (where-am-i)
                      ((xpolaris) (get-settings 'paths 'pd2af.me))
                      ((ab-24) (get-settings 'paths 'local-win-ab-24))
                      (else (get-settings 'paths 'pd2af.org))))
        (_ (write-file (format "~a/~a" file-path generated-filename) af-xml))
        (resp (make-json-string `((af_filename ,generated-filename) (af_fileurl ,file-url)))))
  	(response/full
      200
      #"Sends data back"
      (current-seconds)
      #"application/json; charset=utf-8"
      (list)
      (list
        resp
      ))))
