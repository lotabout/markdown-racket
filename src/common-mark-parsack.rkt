#lang racket

(require parsack)
(module+ test
  (require rackunit))

;; TODO: change return type to (type . content)

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

(define $none-line-ending
  (<!> $line-ending))

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

(define $many-spaces
  (many1 $space-char))

(define $none-whitespace-char
  (<?> (<!> $whitespace-char)
       "expected: non-whitespace character"))

(define $ascii-punctuation-char
  (<?> (oneOf "!\"#$%&'()*+,-./:;<=>?@]^_`{{|}~")
       "expected: ASCII punctuation character"))

(define $non-indent
  (<?> (oneOfStrings "   " "  " " " "")
       "expected: non indent space"))

;;
(define (line-ending->space char)
  (case char
    [(#\newline #\return) #\space]
    [else char]))

;;=============================================================================
;; Inline

;; backslash escape
(define $escaped-char
  (<?> (parser-one
         (char #\\)
         (~> $none-line-ending))
       "expected: escaped chars"))

;; entities
;; skip for now

;; code spans
(define $code-span
  (<?> (parser-compose
         ($backtick-string <- (many1 (char #\`)))
         (code <- (manyUntil
                    (<or> (try (parser-compose
                                 (orig <- (string (list->string $backtick-string)))
                                 (addition <- (many1 (char #\`)))
                                 (return (append orig addition))))
                          $anyChar)
                    (try (parser-one
                           (~> (string (list->string $backtick-string)))
                           (lookAhead (<!> (char #\`)))))))
         (return (string-normalize-spaces (list->string (flatten code)))))
       "expected: code span"))

(module+ test
  (check-equal? (parse-result $code-span "`abc`x") "abc")
  (check-equal? (parse-result $code-span "``abc``x") "abc")
  (check-equal? (parse-result $code-span "``a`b``x") "a`b")
  (check-equal? (parse-result $code-span "`` foo ` bar  ``x") "foo ` bar")
  (check-equal? (parse-result $code-span "`` ``` ``x") "```")
  (check-equal? (parse-result $code-span "``\nfoo\n``x") "foo")
  (check-equal? (parse-result $code-span "`foo   bar\n  baz`x") "foo bar baz")
  (check-equal? (parse-result $code-span "`foo\\`bar`x") "foo\\"))

(define $inline
  (try (many1 (<!> (por $line-ending)))))

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

;; ATX header, like:
;; #.foo
;; ##.foo
(define $trailing-hashes-before-newline
  (try (parser-compose
         $many-spaces
         (many (char #\#))
         $many-spaces
         (lookAhead $line-ending)
         (return null))))

(define $atx-header
  (<?> (parser-compose
         $non-indent
         (head <- (oneOfStrings "######" "#####" "####" "###" "##" "#"))
         $many-spaces
         (xs <- (many1Till (<or> $trailing-hashes-before-newline
                                 $inline)
                           $line-ending))
         (return (cons (list->string head) (format "~a" xs))))
       "expected: ATX header"))

(module+ test
(check-equal? (parse-result $atx-header "# foo\n") '("#" . "foo"))
(check-equal? (parse-result $atx-header "## foo\n") '("##" . "foo"))
(check-equal? (parse-result $atx-header "### foo\n") '("###" . "foo"))
(check-equal? (parse-result $atx-header "#### foo\n") '("####" . "foo"))
(check-equal? (parse-result $atx-header "##### foo\n") '("#####" . "foo"))
(check-equal? (parse-result $atx-header "###### foo\n") '("######" . "foo"))
(check-equal? (parse-result $atx-header "   ###### foo\n") '("######" . "foo"))
(check-exn exn:fail? (lambda () (parse-result $atx-header "####### foo\n")))
(check-exn exn:fail? (lambda () (parse-result $atx-header "#5 foo\n")))
(check-exn exn:fail? (lambda () (parse-result $atx-header "#foobar\n")))
(check-exn exn:fail? (lambda () (parse-result $atx-header "\\# foo\n")))
(check-equal? (parse-result $atx-header "# foo *bar* \\*baz\\*\n") '("#" . "foo *bar* \\*baz\\*"))
;; TODO: add more test.
)
