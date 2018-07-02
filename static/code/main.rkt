#lang racket

(require "../../../../../odysseus/lib/load/all.rkt")
(require "../../../../../odysseus/knowledge-base/tab-tree.rkt")
(require "../../../../../odysseus/knowledge-base/utils.rkt")
(require "../../../../../odysseus/knowledge-base/html.rkt")
(require "snippet-functions.rkt")
(require "template-functions.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

; (write-file "../index.html" (process-html-template "../templates/index.thtml" #:tabtree-root "../knowledge" #:namespace ns))
; (write-file "../rules.html" (process-html-template "../templates/rules.thtml" #:tabtree-root "../knowledge" #:namespace ns))
; (write-file "../examples.html" (process-html-template "../templates/examples.thtml" #:tabtree-root "../knowledge" #:namespace ns))
(write-file "../specification.html" (process-html-template "../templates/specification.thtml" #:tabtree-root "../knowledge" #:namespace ns))
; (write-file "../todo.html" (process-html-template "../templates/todo.thtml" #:tabtree-root "../knowledge" #:namespace ns))
