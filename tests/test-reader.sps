#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2017, 2018, 2019 Göran Weinholt <goran@weinholt.se>

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
  (laesare tests runner))

(define-syntax check
  (lambda (x)
    (syntax-case x (=>)
      ((_ expr => expect)
       #'(test-equal expect expr)))))

;; Lexing
(test-begin "lexing")
(letrec ((get-all
          (lambda (input . arg*)
            (guard (con
                    ((lexical-violation? con)
                     ;; (display (condition-message con)) (newline)
                     ;; (write (condition-irritants con)) (newline)
                     '&lexical))
              (let ((reader (make-reader (open-string-input-port input) "<test>")))
                (reader-mode-set! reader (if (null? arg*) 'r6rs (car arg*)))
                (let lp ((ret '()))
                  (let-values (((type token) (get-token reader)))
                    (if (eof-object? token)
                        (reverse ret)
                        (lp (cons (cons type token) ret))))))))))

  (check (get-all "") => '())
  (check (get-all "#!/usr/bin/env scheme-script\n#f") => '((shebang 1 0 "/usr/bin/env scheme-script") (value . #f)))
  (check (get-all " #!/usr/bin/env scheme-script\n#f") => '((whitespace . " ") (shebang 1 1 "/usr/bin/env scheme-script") (value . #f)))
  (check (get-all " #f ") => '((whitespace . " ") (value . #f) (whitespace . " ")))
  (check (get-all "#!r6rs #f") => '((directive . r6rs) (whitespace . " ") (value . #f)))

  ;; Comment to line ending or paragraph separator.
  (check (get-all ";comment\x000A;(") => '((comment . "comment\x000A;") (openp . #f)))
  (check (get-all ";comment\x000D;(") => '((comment . "comment\x000D;") (openp . #f)))
  (check (get-all ";comment\x000D;\x000A;(") => '((comment . "comment\x000D;\x000A;") (openp . #f)))
  (check (get-all ";comment\x0085;(") => '((comment . "comment\x0085;") (openp . #f)))
  (check (get-all ";comment\x000D;\x0085;(") => '((comment . "comment\x000D;\x0085;") (openp . #f)))
  (check (get-all ";comment\x2028;(") => '((comment . "comment\x2028;") (openp . #f)))
  (check (get-all ";comment\x2029;(") => '((comment . "comment\x2029;") (openp . #f)))

  ;; Inline comments.
  (check (get-all "#;1") => '((inline-comment () . 1)))
  (check (get-all "#; 1") => '((inline-comment ((whitespace . " ")) . 1)))
  (check (get-all "#;;\n1") => '((inline-comment ((comment . "\n")) . 1)))
  (check (get-all "#;#||#1") => '((inline-comment ((nested-comment . "")) . 1)))
  (check (get-all "#;#;1 2") => '((inline-comment ((inline-comment () . 1) (whitespace . " ")) . 2)))

  ;; Nested comments.
  (check (get-all "#| |#") => '((nested-comment . " ")))
  (check (get-all "#| x |# y") => '((nested-comment . " x ") (whitespace . " ") (identifier . y)))
  (check (get-all "#| x #| y |# z |# w") => '((nested-comment . " x #| y |# z ") (whitespace . " ") (identifier . w)))
  (check (get-all "#|##||#|#") => '((nested-comment . "##||#")))
  (check (get-all "#|#|#||#|#|#") => '((nested-comment . "#|#||#|#")))
  (check (get-all "#|#|#| ## |#|#|#") => '((nested-comment . "#|#| ## |#|#")))

  ;; Examples from R6RS.
  (check (get-all "#\\a") => '((value . #\a)))
  (check (get-all "#\\A") => '((value . #\A)))
  (check (get-all "#\\(") => '((value . #\()))
  (check (get-all "#\\ ") => '((value . #\x0020)))
  (check (get-all "#\\nul") => '((value . #\x0000)))
  (check (get-all "#\\alarm") => '((value . #\x0007)))
  (check (get-all "#\\backspace") => '((value . #\x0008)))
  (check (get-all "#\\tab") => '((value . #\x0009)))
  (check (get-all "#\\linefeed") => '((value . #\x000A)))
  (check (get-all "#\\newline") => '((value . #\x000A)))
  (check (get-all "#\\vtab") => '((value . #\x000B)))
  (check (get-all "#\\page") => '((value . #\x000C)))
  (check (get-all "#\\return") => '((value . #\x000D)))
  (check (get-all "#\\esc") => '((value . #\x001B)))
  (check (get-all "#\\space") => '((value . #\x0020)))
  (check (get-all "#\\delete") => '((value . #\x007F)))
  (check (get-all "#\\xFF") => '((value . #\x00FF)))
  (check (get-all "#\\x03BB") => '((value . #\x03BB)))
  (check (get-all "#\\x00006587") => '((value . #\x6587)))
  (check (get-all "#\\λ") => '((value . #\x03BB)))
  (check (get-all "#\\x0001z") => '&lexical)
  (check (get-all "#\\λx") => '&lexical)
  (check (get-all "#\\alarmx") => '&lexical)
  (check (get-all "#\\alarm x") => '((value . #\x0007) (whitespace . " ") (identifier . x)))
  (check (get-all "#\\Alarm") => '&lexical)
  (check (get-all "#\\alert") => '&lexical)
  (check (get-all "#\\xA") => '((value . #\x000A)))
  (check (get-all "#\\xFF") => '((value . #\x00FF)))
  (check (get-all "#\\xff") => '((value . #\x00FF)))
  (check (get-all "#\\x ff") => '((value . #\x0078) (whitespace . " ") (identifier . ff)))
  (check (get-all "#\\x(ff)") => '((value . #\x0078) (openp . #f) (identifier . ff) (closep . #f)))
  (check (get-all "#\\(x)") => '&lexical)
  (check (get-all "#\\(x") => '&lexical)
  (check (get-all "#\\((x)") => '((value . #\x0028) (openp . #f) (identifier . x) (closep . #f)))
  (check (get-all "#\\x00110000") => '&lexical)
  (check (get-all "#\\x000000001") => '((value . #\x0001)))
  (check (get-all "#\\xD800") => '&lexical)

  ;; String examples from R6RS.
  (check (get-all "\"abc\"") => '((value . "\x0061;\x0062;\x0063;")))
  (check (get-all "\"\\x41;bc\"") => '((value . "\x0041;\x0062;\x0063;")))
  (check (get-all "\"\\x41; bc\"") => '((value . "\x0041;\x0020;\x0062;\x0063;")))
  (check (get-all "\"\\x41bc;\"") => '((value . "\x41BC;")))
  (check (get-all "\"\\x41\"") => '&lexical)
  (check (get-all "\"\\x;\"") => '&lexical)
  (check (get-all "\"\\x41bx;\"") => '&lexical)
  (check (get-all "\"\\x00000041;\"") => '((value . "\x0041;")))
  (check (get-all "\"\\x0010FFFF;\"") => '((value . "\x10FFFF;")))
  (check (get-all "\"\\x00110000;\"") => '&lexical)
  (check (get-all "\"\\x000000001;\"") => '((value . "\x0001;")))
  (check (get-all "\"\\xD800;\"") => '&lexical)
  (check (get-all "\"A\nbc\"") => '((value . "\x0041;\x000A;\x0062;\x0063;")))

  ;; Circular data
  (check (get-all "#0=(a b c . #0#)" 'r7rs)
         => '((label . 0) (openp . #f) (identifier . a) (whitespace . " ")
              (identifier . b) (whitespace . " ") (identifier . c)
              (whitespace . " ") (dot . #f) (whitespace . " ") (reference . 0)
              (closep . #f)))

  ;; Mantissa width (tests commented out for now, they depend on
  ;; string->number, which is not working in Guile 3.0.8)
  ;; (check (get-all "(1.0|11)" 'r6rs)
  ;;        => '((openp . #f) (value . 1.0) (closep . #f)))
  ;; (check (get-all "(1.0|24)" 'r6rs)
  ;;        => '((openp . #f) (value . 1.0) (closep . #f)))
  ;; (check (get-all "(1.0|53)" 'r6rs)
  ;;        => '((openp . #f) (value . 1.0) (closep . #f)))

  ;; Found with fuzzing
  (check (get-all "#\\xF000000000000000") => '&lexical)

  )
(test-end)

(test-begin "tolerant-lexing")
(letrec ((get-all
          (lambda (input . arg*)
            (with-exception-handler
              (lambda (con)
                (unless (warning? con)
                  (raise con)))
              (lambda ()
                (let ((reader (make-reader (open-string-input-port input) "<test>")))
                  (reader-tolerant?-set! reader #t)
                  (reader-mode-set! reader (if (null? arg*) 'r6rs (car arg*)))
                  (let lp ((ret '()))
                    (let-values (((type token) (get-token reader)))
                      (if (eof-object? token)
                          (reverse ret)
                          (lp (cons (cons type token) ret)))))))))))
  (check (get-all "\n#!/usr/bin/env scheme-script\n#f") =>
         '((whitespace . "\n")
           (identifier . /usr/bin/env)
           (whitespace . " ")
           (identifier . scheme-script)
           (whitespace . "\n")
           (value . #f)))
  (check (get-all ";;\n#! comment !#\n foo" 'rnrs) =>
         '((comment . ";\n")
           (comment . "comment ")
           (whitespace . "\n ")
           (identifier . foo))))
(test-end)

;; Detect file type
(test-begin "detect")
(letrec ((detect (lambda (input)
                   (call-with-port (open-string-input-port input)
                     detect-scheme-file-type))))
  (check (detect "") => 'empty)
  (check (detect "#!r6rs") => 'empty)
  (check (detect "#!/usr/bin/env scheme-script\n#f") => 'r6rs-program)
  (check (detect "#! /usr/bin/env scheme-script\n#f") => 'r6rs-program)
  (check (detect "(import (rnrs))") => 'r6rs-program)
  (check (detect "#!r6rs (import ") => 'r6rs-program)
  (check (detect "#!r6rs (library ") => 'r6rs-library)
  ;; Looks weird but it's allowed.
  (check (detect "#!r6rs [library ") => 'r6rs-library)
  (check (detect "[#!r6rs library ") => 'r6rs-library)
  (check (detect "#!r6rs [import ") => 'r6rs-program)
  (check (detect "[#!r6rs import ") => 'r6rs-program)
  ;; R7RS stuff.
  (check (detect "(define-library ") => 'r7rs-library))
(test-end)

;; Reading
(test-begin "reading")
(letrec ((stripped-read
          (lambda (input)
            (let ((reader (make-reader (open-string-input-port input) "<test>")))
              (reader-mode-set! reader 'r6rs)
              (guard (con
                      (else
                       ;; (display (condition-message con)) (newline)
                       ;; (write (condition-irritants con)) (newline)
                       'error))
                (annotation-stripped (read-annotated reader)))))))
  (check (stripped-read "#!/usr/bin/env scheme-script\n#f") => '#f)
  (check (stripped-read " #!/usr/bin/env scheme-script\n#f") => 'error)
  (check (stripped-read "#f") => #f)
  (check (stripped-read "()") => '())
  (check (stripped-read "(a . b)") => '(a . b))
  (check (stripped-read "( . b)") => 'error)
  (check (stripped-read "#!r6rs ()") => '())
  (check (stripped-read "(#!r6rs)") => '())
  (check (stripped-read "#!\tr6rs ()") => 'error)
  (check (stripped-read "#\\NEWLINE") => 'error)
  (check (stripped-read "#!fold-case #\\NEWLINE") => #\newline)
  (check (stripped-read "#!no-fold-case #\\NEWLINE") => 'error)
  (check (stripped-read "#!fold-case X") => 'x)
  ;; Disabled for GNU Guile 2.2.0.
  ;; (check (stripped-read "#!fold-case STRAßE") => 'strasse)
  (check (stripped-read "\"\\xf6;\"") => "\xf6;")

  ;; There is no obfuscated Scheme contest.
  (check (stripped-read "(let((* .(`'((unquote .(+ .[])).()))).())((caadr .(* .())).(#o-7 .[#E#x93/3])).[])") =>
         '(let ([* `'(,+)]) ((caadr *) -7 49)))

  ;; Comment to line ending or paragraph separator.
  (check (stripped-read "; comment\x000A;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x000D;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x000D;\x000A;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x0085;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x000D;\x0085;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x2028;(display 'hello)") => '(display 'hello))
  (check (stripped-read "; comment\x2029;(display 'hello)") => '(display 'hello))

  ;; Inline comments
  (check (stripped-read "#; ;foo\n(x) (y)") => '(y))
  (check (stripped-read "#; #| |# (x) (y)") => '(y))
  (check (stripped-read "#; #; (x) (y) (z)") => '(z))

  ;; Nested comments
  (check (stripped-read "#| x |# y") => 'y)
  (check (stripped-read "#| x #| y |# z |# w") => 'w)
  (check (stripped-read "#|##||#|# x") => 'x)
  (check (stripped-read "#|#|#||#|#|# x") => 'x)
  (check (stripped-read "#|#|#| ## |#|#|# x") => 'x))
(test-end)

;; RnRS modes and the incompatibilities added in R7RS.
(test-begin "rnrs")
(letrec ((stripped-read
          (lambda (mode input)
            (let ((reader (make-reader (open-string-input-port input) "<test>")))
              (reader-mode-set! reader mode)
              (guard (con
                      (else
                       ;; (display (condition-message con)) (newline)
                       ;; (write (condition-irritants con)) (newline)
                       'error))
                (read-datum reader))))))
  ;; Booleans
  (test-equal #f (stripped-read 'rnrs "#!false"))
  (test-equal #t (stripped-read 'rnrs "#!true"))
  (test-equal #f (stripped-read 'r7rs "#false"))
  (test-equal #t (stripped-read 'r7rs "#true"))
  (test-equal #f (stripped-read 'r7rs "#faLse"))
  (test-equal #t (stripped-read 'r7rs "#trUE"))
  (test-equal #f (stripped-read 'r7rs "#f"))
  (test-equal #t (stripped-read 'r7rs "#t"))
  (test-equal 'error (stripped-read 'r6rs "#!true"))
  (test-equal 'error (stripped-read 'r5rs "#!true"))
  ;; Characters
  (test-equal #\nul (stripped-read 'r6rs "#\\nul"))
  (test-equal #\nul (stripped-read 'r7rs "#\\null"))
  (test-equal 'error (stripped-read 'r7rs "#\\nul"))
  (test-equal 'error (stripped-read 'r6rs "#\\null"))
  ;; Bytevectors
  (test-equal #vu8() (stripped-read 'r6rs "#vu8()"))
  (test-equal #vu8() (stripped-read 'r7rs "#u8()"))
  (test-equal #vu8() (stripped-read 'r7rs "#U8()"))
  (test-equal 'error (stripped-read 'r6rs "#u8()"))
  (test-equal 'error (stripped-read 'r7rs "#vu8()"))
  ;; Strings
  (test-equal "|" (stripped-read 'r7rs " \"\\|\" "))
  (test-equal 'error (stripped-read 'r7rs " \"\\v\" "))
  (test-equal 'error (stripped-read 'r7rs " \"\\f\" "))
  (test-equal "\v" (stripped-read 'r6rs " \"\\v\" "))
  (test-equal "\f" (stripped-read 'r6rs " \"\\f\" "))
  ;; Symbols
  (test-equal 'foo (stripped-read 'r7rs "|foo|"))
  (test-equal (string->symbol "a|b") (stripped-read 'r7rs "|a\\|b|"))
  (test-equal (string->symbol "a b") (stripped-read 'r7rs "|a b|"))
  (test-equal 'Hello (stripped-read 'r7rs "|H\\x65;llo|"))
  (test-equal 'λ (stripped-read 'r7rs "|\\x3BB;|"))
  (test-equal (string->symbol "\x9;\x9;") (stripped-read 'r7rs "|\\t\\t|"))
  (test-equal (string->symbol "\x9;\x9;") (stripped-read 'r7rs "|\\x9;\\x9;|"))
  (test-equal (string->symbol "") (stripped-read 'r7rs "||"))
  (test-equal (string->symbol "+soup+") (stripped-read 'r7rs "+soup+"))
  (test-equal (string->symbol "@") (stripped-read 'r7rs "@"))
  ;; Delimiters
  (test-equal '(foo bar) (stripped-read 'r7rs "(foo|bar|)"))
  (test-equal 'error (stripped-read 'r6rs "(foo|bar|)"))
  (test-equal `(foo ,(string->symbol ".#") (bar)) (stripped-read 'r7rs "(foo .#(bar))"))
  (test-equal '(foo . #(bar)) (stripped-read 'r6rs "(foo .#(bar))"))
  ;; Numbers (not)
  (test-equal 'error (stripped-read 'r7rs "0x1"))
  (test-equal 0 (stripped-read 'r7rs "+0"))
  ;; (test-equal 'error (stripped-read 'r7rs "+."))
  (test-equal 0.0 (stripped-read 'r7rs "+.0"))
  )
(test-end)

;; Shared/circular data
(test-begin "shared")
(letrec ((stripped-read
          (lambda (input)
            (let ((reader (make-reader (open-string-input-port input) "<test>")))
              (reader-mode-set! reader 'r7rs)
              (guard (con
                      (else
                       ;; (display (condition-message con)) (newline)
                       ;; (write (condition-irritants con)) (newline)
                       'error))
                (read-datum reader))))))
  (test-assert (let ((x (stripped-read "#0=(a b c . #0#)")))
                 (equal? x (cdddr x))))
  (test-assert (let ((x (stripped-read "#0= #(a b c #0#)")))
                 (equal? x (vector-ref x 3))))
  (test-assert (let ((x (stripped-read "(#0=(a) #0#)")))
                 (equal? (car x) (cadr x))))
  (test-assert (let ((x (stripped-read "(#0=(a) . #0#)")))
                 (equal? (car x) (cdr x))))
  (test-assert (let ((x (stripped-read "(#0# . #0=(a))")))
                 (equal? (car x) (cdr x)))) ;not required by r7rs
  (test-equal '((a) (a) (a)) (stripped-read "(#0=(a) #0# #0#)"))
  (test-equal 'error (stripped-read "#0= #0#"))
  (test-equal 'error (stripped-read "#0= #1#"))
  (test-equal 'error (stripped-read "#0= (#1#)"))
  (test-equal 'error (stripped-read "(#0=a #0=a)")) ;duplicate label
  (test-equal 'error (stripped-read "#u8(#0=1 #0#)")) ;not expected to work
  (test-equal '(1 2 3) (stripped-read "#0=(1 2 3 #;#0#)"))
  (test-equal '#(1 2 3) (stripped-read "#0=#(1 2 3 #;#0#)"))
  (test-equal '#(1 2 3) (stripped-read "#0=#(1 2 3 #;#1#)"))
  (test-equal #vu8(0) (stripped-read "#0= #u8(0)"))
  (test-equal #f (stripped-read "#0= #false")))
(test-end)

;; Case folding
(test-begin "foldcase")
(letrec ((stripped-read
          (lambda (fold? input)
            (let ((reader (make-reader (open-string-input-port input) "<test>")))
              (reader-fold-case?-set! reader fold?)
              (read-datum reader)))))
  (test-equal 'foo (stripped-read #f "foo"))
  (test-equal 'foo (stripped-read #t "foo"))
  (test-equal 'FOO (stripped-read #f "FOO"))
  (test-equal 'foo (stripped-read #t "FOO"))
  (test-equal 'foo (stripped-read #f "#!fold-case foo"))
  (test-equal 'foo (stripped-read #t "#!fold-case foo"))
  (test-equal 'FOO (stripped-read #f "#!no-fold-case FOO"))
  (test-equal 'FOO (stripped-read #t "#!no-fold-case FOO"))
  (test-equal #t
              (let ((reader (make-reader (open-string-input-port "#!fold-case FOO")
                                         "<test>")))
                (read-datum reader)
                (reader-fold-case? reader)))
  (test-equal #f
              (let ((reader (make-reader (open-string-input-port "#!no-fold-case FOO")
                                         "<test>")))
                (reader-fold-case?-set! reader #t)
                (read-datum reader)
                (reader-fold-case? reader))))
(test-end)

;; Reading files
(test-begin "read-files")
(let ((p (open-input-file "reader.sls")))
  (cond ((and (port-has-set-port-position!? p)
              (port-has-port-position? p))
         (let ((pos0 (port-position p))
               (r (make-reader p "reader.sls")))
           (let ((datum (read-datum r)))
             (set-port-position! p pos0)
             (let ((expect (read p)))
               (test-equal expect datum)))))
        (else
         (display "Skipping test due to lack of port-position support\n"))))
(test-end)

(test-exit)
