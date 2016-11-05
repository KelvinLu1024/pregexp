;last change 2005-04-24

(library (pregexp tester)
  (export test
          bottomline)
  (import [rnrs]
          [rnrs io simple])

  ;; do not rely on SLIB
  #;(require (lib "defmacro.ss"))


  (define foobar
    (let ([failed 0])
      (case-lambda
       [() failed]
       [(x) (set! failed (+ failed 1))])))
  #;
  (define *failed* 0)

  (define-syntax test
    (syntax-rules ()
      [(test expr expected-answer)
       (test-each expr expected-answer)]
      [(test expr expected-answer rest ...)
       (begin
         (test-each expr expected-answer)
         (test rest ...))]))

  (define-syntax test-each
    (syntax-rules ()
      [(test-each expr expected-answer)
       (begin
         (display "Trying ")
         (display 'expr)
         (newline)
         (display "    --> ")
         (let ([actual expr]
               [expected 'expected-answer])
           (display actual)
           (newline)
           (display "...")
           (if (equal? actual expected)
               (display "OK")
               (begin
                 #;(set! *failed* (+ *failed* 1))
                 (foobar #f)
                 (display "FAILED!!!")
                 (newline)
                 (display "   ;;; expected ")
                 (display 'expected)
                 (newline)))))]))
  (define bottomline
    (lambda ()
      (newline)
      (let ([*failed* (foobar)])
        (if (= *failed* 0)
            (display "All tests succeeded! :-) :-) :-)")
            (begin
             (display *failed*)
             (display " test")
             (if (> *failed* 1) (display "s"))
             (display " failed! :-( :-( :-("))))
      (newline)))
  
  ;; redefine with define-syntax
  #;  
  (define-macro test
    (lambda ee
      `(begin
         ,@(let loop ((ee ee) (r '()))
             (if (null? ee) (reverse r)
                 (loop (cddr ee)
                       (cons `(test-each ,(car ee) ,(cadr ee))
                             r)))))))

  ;; redefine with define-syntax
  #; 
  (define-macro test-each
    (lambda (expr expected-answer)
      `(begin
         (display "Trying ")
         (write ',expr)
         (newline)
         (display "   --> ")
         (let ((__actual-answer ,expr))
           (write __actual-answer)
           (display " ... ")
           (if  (equal? __actual-answer ',expected-answer)
                (display "OK")
                (begin
                  (set! *failed* (+ *failed* 1))
                  (display "FAILED!!!")
                  (newline)
                  (display "   ;;; expected ")
                  (write ',expected-answer))))
         (newline))))
  ;; rewrite because r6rs does not allow top-level variable assignment
  ;; in library
  #; 
  (define bottomline
    (lambda ()
      (newline)
      (if (= *failed* 0)
          (display "All tests succeeded! :-) :-) :-)")
          (begin (display *failed*)
                 (display " test")
                 (if (> *failed* 1) (display "s"))
                 (display " failed! :-( :-( :-(")))
      (newline)))

  )
