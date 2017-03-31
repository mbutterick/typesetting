#lang racket/base
(provide (all-defined-out))

(struct co-dict (dict) #:transparent)
(struct co-array (items) #:transparent)
(struct co-stream (dict data) #:transparent)
(struct co-version (num) #:transparent)
(struct co-io (obj gen thing) #:transparent)
(struct co-io-ref (obj gen) #:transparent)