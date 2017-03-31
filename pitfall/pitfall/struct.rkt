#lang racket/base
(provide (all-defined-out))

(struct co-dict (dict) #:transparent)
(struct co-array (items) #:transparent)
(struct co-stream (dict data) #:transparent)
(struct co-version (num) #:transparent)
(struct co-header (string) #:transparent)
(struct co-io (idx rev thing) #:transparent)
(struct co-io-ref (idx rev) #:transparent)
(struct co-comment (text) #:transparent)