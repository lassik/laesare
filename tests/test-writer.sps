#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>

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

(import
  (rnrs (6))
  (srfi :64 testing)
  (laesare reader)
  (laesare writer))

(define (get-all mode input)
  (let ((reader (make-reader (open-string-input-port input) "<test>")))
    (reader-mode-set! reader mode)
    (let lp ((ret '()))
      (let-values ([(type token) (get-token reader)])
        (if (eof-object? token)
            (reverse ret)
            (lp (cons (cons type token) ret)))))))

(define (tokens->string mode tokens)
  (call-with-string-output-port
    (lambda (p)
      (let ((writer (make-writer p "<test>")))
        (writer-mode-set! writer mode)
        (for-each (lambda (type+token)
                    (put-token writer (car type+token) (cdr type+token)))
                  tokens)))))

(define (->r7rs tokens)
  (map (lambda (tok)
         (cond ((equal? tok '(directive . r6rs)) '(directive . r7rs))
               (else tok)))
       tokens))

(define (->r6rs tokens)
  (map (lambda (tok)
         (cond ((equal? tok '(directive . r7rs)) '(directive . r6rs))
               (else tok)))
       tokens))

(test-begin "write-simple")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "#t"))
            "#t")
(test-equal (tokens->string 'r6rs (get-all 'r7rs "#true"))
            "#t")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "(x . y)"))
            "(x . y)")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "([ ])"))
            "([ ])")
(test-equal (tokens->string 'r7rs (get-all 'r7rs "([ ])"))
            "(( ))")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "#()"))
            "#()")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "'a `a ,a ,@a"))
            "'a `a ,a ,@a")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "#'a #`a #,a #,@a"))
            "#'a #`a #,a #,@a")
(test-equal (tokens->string 'r7rs (get-all 'r6rs "#\\linefeed"))
            "#\\newline")
(test-equal (tokens->string 'r6rs (get-all 'r7rs "#\\newline"))
            "#\\linefeed")
(test-equal (tokens->string 'r7rs (get-all 'r6rs "#\\vtab"))
            "#\\xb")
(test-end)

(test-begin "write-shebang")
(test-equal (tokens->string 'rnrs (get-all 'rnrs "#!/usr/bin/env scheme-script"))
            "#!/usr/bin/env scheme-script\n")
(test-end)

(test-begin "write-bytevector")
(test-equal (tokens->string 'r6rs (get-all 'r7rs "#u8(0 255)"))
            "#vu8(0 255)")
(test-equal (tokens->string 'r7rs (get-all 'r6rs "#vu8(0 255)"))
            "#u8(0 255)")
(test-equal (tokens->string 'rnrs (get-all 'rnrs "#!r7rs #u8(0 255)"))
            "#!r7rs #u8(0 255)")
(test-equal (tokens->string 'rnrs (get-all 'rnrs "#!r6rs #vu8(0 255)"))
            "#!r6rs #vu8(0 255)")
(test-equal (tokens->string 'rnrs (->r7rs (get-all 'rnrs "#!r6rs #vu8(0 255)")))
            "#!r7rs #u8(0 255)")
(test-equal (tokens->string 'rnrs (->r6rs (get-all 'rnrs "#!r7rs #u8(0 255)")))
            "#!r6rs #vu8(0 255)")
(test-end)

(test-begin "write-comments")
(test-equal (tokens->string 'rnrs (get-all 'rnrs "  ;; foo\n"))
            "  ;; foo\n")
(test-equal (tokens->string 'r6rs (get-all 'r6rs "#; #(x)"))
            "#; #(x)")
(test-equal (tokens->string 'rnrs (get-all 'rnrs " a #| b #| c |# d |# e "))
            " a #| b #| c |# d |# e ")
;; TODO: There is a limitation here. Whitespace is lost in the commented datum.
;; (test-equal (tokens->string 'r6rs (get-all 'r6rs "#; #( x )"))
;;             "#; #( x )")
(test-end)

(test-begin "write-ref")
(test-equal (tokens->string 'r7rs (get-all 'r7rs "(#1=foo #1#)"))
            "(#1=foo #1#)")
(test-end)

(test-begin "write-file")
(define (tokenize-file filename)
  (call-with-input-file filename
    (lambda (p)
      (let ((reader (make-reader p filename)))
        (let lp ((ret '()))
          (let-values ([(type token) (get-token reader)])
            (if (eof-object? token)
                (reverse ret)
                (lp (cons (cons type token) ret)))))))))

(define (get-file filename)
  (call-with-input-file filename get-string-all))

(define (read-string string)
  (call-with-port (open-string-input-port string) read))

;; (display (tokens->string 'r6rs (tokenize-file "reader.sls")))

(test-equal (read-string
             (call-with-string-output-port
               (lambda (p)
                 (display (tokens->string 'r6rs (tokenize-file "reader.sls")) p))))
            (read-string
             (get-file "reader.sls")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
