(define (write-some-c-code)
  (let ((my-code
         "int foo() {
  bar();
}
"))
    (display my-code)))

(write-some-c-code)


(define (write-some-c-code)
  (let ((my-code
         $ int foo() {
         $   bar();
         $ }
         ))
    (display my-code)))
