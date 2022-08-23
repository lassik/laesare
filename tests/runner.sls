;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2022 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;;; Simple runner for top-level programs that use SRFI 64

(library (laesare tests runner)
  (export
    test-exit)
  (import
    (rnrs)
    (srfi :64 testing))

(define wrongs 0)

(define (global-test-runner)
  (let ((runner (test-runner-simple)))
    (let ((on-final (test-runner-on-final runner)))
      (test-runner-on-final! runner
                             (lambda (runner)
                               (set! wrongs (+ wrongs
                                               (test-runner-fail-count runner)
                                               (test-runner-xpass-count runner)))
                               (on-final runner)
                               (test-runner-reset runner))))
    runner))

(define (test-exit)
  (exit (if (zero? wrongs) 0 1)))

(test-runner-factory
 (lambda () (global-test-runner))))
