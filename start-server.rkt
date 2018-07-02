#lang racket

(require web-server/servlet web-server/servlet-env)
(require "servlets/translator.rkt")
;
; make background process: 'sudo -b nohup racket start-server.rkt'

(serve/servlet
	translator
	#:listen-ip #f ; #f - open to external http requests
	#:port 80  ; standard port
	; #:servlet-regexp #rx"" ; capture all top-level requests
	#:servlet-regexp #rx"/translator"
	#:extra-files-paths (list (build-path "static"))
	#:launch-browser? #f
)
