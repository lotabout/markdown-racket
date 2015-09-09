#lang racket

(require parsack)
(module+ test
  (require rackunit))

;;=============================================================================
;; Helper functions

;; peg style of por
(define-syntax por
  (syntax-rules ()
    [(_ x ...)
     (<or> (try x) ...)]))


;; line ending characters
(define $line-ending
  (<?> (por (string "\n")
            (parser-seq (char #\return) (~ (notFollowedBy (char #\newline))))
            (string "\r\n"))
       "expected: line ending."))

(module+ test
  (check-equal? (parse-result $line-ending "\n") '(#\newline))
  (check-equal? (parse-result $line-ending "\r") '(#\return))
  (check-equal? (parse-result $line-ending "\r\n") '(#\return #\newline))
  (check-equal? (parse-result $line-ending "\ra") '(#\return))
  (check-exn exn:fail? (lambda () (parse-result $line-ending "x"))))

;; one blank line
(define $blank-line
  (<?> (parser-compose
         (ret <- (many (oneOf " \t")))
         $line-ending
         (return (list->string ret)))
       "expected: blank line"))

(module+ test
  (check-equal? (parse-result $blank-line "       \t \n") "       \t ")
  (check-equal? (parse-result $blank-line "       \t \r") "       \t ")
  (check-exn exn:fail? (lambda () (parse-result $blank-line "a\n"))))

;; whitespace character
(define $whitespace-char
  (<?> (oneOf " \t\n\v\f\r")
       "expected: whitespace character"))

(module+ test
  (check-eq? (parse-result $whitespace-char "\t") #\tab)
  (check-eq? (parse-result $whitespace-char " ") #\space)
  (check-eq? (parse-result $whitespace-char "\n") #\newline)
  (check-eq? (parse-result $whitespace-char "\v") #\vtab)
  (check-eq? (parse-result $whitespace-char "\f") #\page)
  (check-eq? (parse-result $whitespace-char "\r") #\return)
  (check-exn exn:fail? (lambda () (parse-result $whitespace-char "a"))))

;; one or more whitespace
(define $whitespace
  (<?> (parser-compose
         (ret <- (many1 $whitespace-char))
         (return (list->string ret)))
       "expected: whitespace"))

(module+ test
  (check-equal? (parse-result $whitespace "   \t\f  ") "   \t\f  ")
  (check-equal? (parse-result $whitespace "\t\r\vab") "\t\r\v")
  (check-exn exn:fail? (lambda () (parse-result $whitespace "a"))))

;; space
(define $space-char
  (<?> (char #\space)
        "expected: space character"))

(define $none-whitespace-char
  (<?> (<!> $whitespace-char)
       "expected: non-whitespace character"))

(define $ascii-punctuation-char
  (<?> (oneOf "!\"#$%&'()*+,-./:;<=>?@]^_`{{|}~")
       "expected: ASCII punctuation character"))

(define $non-indent
  (<?> (oneOfStrings "   " "  " " " "")
       "expected: non indent space"))

;;=============================================================================
;; Leaf Blocks

;; horizontal rules
(define $horizontal-rule
  (<?> (parser-compose
         $non-indent
         (sep <- (oneOf "*-_"))
         (second-sep <- (parser-compose (many $space-char) (char sep)))
         (rest-sep <- (many1 (try (parser-compose (many $space-char) (char sep)))))
         (many $space-char)
         $line-ending
         (return (list->string (list* sep second-sep rest-sep))))
       "expected: horizontal rule"))

(module+ test
  (check-equal? (parse-result $horizontal-rule "***\n") "***")
  (check-equal? (parse-result $horizontal-rule "---\n") "---")
  (check-equal? (parse-result $horizontal-rule "___\n") "___")
  (check-equal? (parse-result $horizontal-rule " ***\n") "***")
  (check-equal? (parse-result $horizontal-rule "  ***\n") "***")
  (check-equal? (parse-result $horizontal-rule "   ***\n") "***")
  (check-equal? (parse-result $horizontal-rule "-------\n") "-------")
  (check-equal? (parse-result $horizontal-rule " - - -\n") "---")
  (check-equal? (parse-result $horizontal-rule " **  * ** * ** * **\n") "***********")
  (check-equal? (parse-result $horizontal-rule " -     -     --  \n") "----")
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "    ---\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "+++\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "===\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "--\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "**\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "__\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "*-*\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "a----\n")))
  (check-exn exn:fail? (lambda () (parse-result $horizontal-rule "---a---\n")))
  )
