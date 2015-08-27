#lang racket

(require racket/match)

(module+ test
  (require rackunit))

(define ((regex-parse regex type [string-op identity]) string)
  (let ((positions (regexp-match-positions* regex string))
        (upper (string-length string)))
    (let split ((lower 0)
                (positions positions)
                (result '()))
      (cond
        [(> lower upper) (reverse result)]
        [(empty? positions)
         (reverse (cons (cons 'literal (substring string lower upper))
                        result))]
        [else
          (let ((left (caar positions))
                (right (cdar positions)))
            (if (= lower left)
                (split right (cdr positions)
                       (cons (cons type (string-op (substring string left right)))
                             result))
                (split left positions
                       (cons (cons 'literal (substring string lower left))
                             result))))]))))

(define parse-link (regex-parse #px"\\[.*\\]\\(.*\\)" 'link))
(module+ test
  (check-equal? (parse-link "ab[desc](link)e")
                '((literal . "ab") (link . "[desc](link)") (literal . "e")))
  (check-equal? (parse-link "abdesc](link)e")
                '((literal . "abdesc](link)e"))))

(define parse-inline-code
  (regex-parse #px"`.*`" 'code (lambda (x) (substring x 1 (- (string-length x) 1)))))
(module+ test
  (check-equal? (parse-inline-code "ab`code`cd")
                '((literal . "ab") (code . "code") (literal . "cd")))
  (check-equal? (parse-inline-code "abcode`cd")
                '((literal . "abcode`cd"))))

(define parse-strong
  (regex-parse #px"(\\*\\*|__).*\\1" 'strong (lambda (x) (substring x 2 (- (string-length x) 2)))))
(module+ test
  (check-equal? (parse-strong "abc**def**g")
                '((literal . "abc") (strong . "def") (literal . "g")))
  (check-equal? (parse-strong "abc**def_*g")
                '((literal . "abc**def_*g")))
  (check-equal? (parse-strong "abc__def__g")
                '((literal . "abc") (strong . "def") (literal . "g")))
  (check-equal? (parse-strong "**abc__def__g**h")
                '((strong . "abc__def__g") (literal . "h")))
  (check-equal? (parse-strong "")
                '((literal . ""))))

(define parse-em
  (regex-parse #px"(\\*|_).*\\1" 'em (lambda (x) (substring x 1 (- (string-length x) 1)))))
(module+ test
  (check-equal? (parse-em "abc*def*g")
                '((literal . "abc") (em . "def") (literal . "g")))
  (check-equal? (parse-em "abc*def_g")
                '((literal . "abc*def_g")))
  (check-equal? (parse-em "abc_def_g")
                '((literal . "abc") (em . "def") (literal . "g")))
  (check-equal? (parse-em "*abc*_def_g")
                '((em . "abc") (em . "def") (literal . "g")))
  (check-equal? (parse-em "")
                '((literal . ""))))

(define parse-style (regex-parse #px"(\\*\\*|__|~~|\\*|_).*\\1" 'style))

(parse-style "**abc_de~~f~~_**")


#;(regexp-match-positions* #rx"(\\*\\*|__).*(\\*\\*|__)" "__abc***ab*")
#;(regexp-match-positions* #rx"ab+c" "aabbbbbbbbc   abc")

