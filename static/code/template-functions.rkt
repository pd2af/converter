#lang racket

(require "../../../../../odysseus/lib/load/all.rkt")

(provide (all-defined-out))

(define (yandex-counter)
  (read-file "../templates/yandex-counter.phtml"))
