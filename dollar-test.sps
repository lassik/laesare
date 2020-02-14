(import (rnrs) (laesare reader))

(define filename "dollar-test-data.scm")

(define (test-lexer)
  (call-with-port
   (open-input-file filename)
   (lambda (port)
     (let ((reader (make-reader port filename)))
       (let loop ((xs '()))
         (let-values (((type lexeme) (get-token reader)))
           (if (equal? 'eof type) (reverse xs)
               (loop (cons (cons type lexeme) xs)))))))))

(define (test-parser)
  (call-with-port
   (open-input-file filename)
   (lambda (port)
     (let ((reader (make-reader port filename)))
       (let loop ((xs '()))
         (let ((datum (read-datum reader)))
           (if (eof-object? datum) (reverse xs)
               (loop (cons datum xs)))))))))

(for-each (lambda (x) (write x) (newline))
          (test-parser))
