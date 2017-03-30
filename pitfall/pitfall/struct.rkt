#lang racket/base
(provide (struct-out $stream))
(struct $stream (dict data) #:transparent)