#lang racket

(require parsack)

(module+ test
  (require rackunit))

;; peg style of por
(define-syntax por 
  (syntax-rules ()
    [(_ x ...)
     (<or> (try x) ...)]))

;; single space
(define space-char (por (char #\space) (char #\tab)))

(module+ test
  (check-eq? (parse-result space-char " ") #\space)
  (check-eq? (parse-result space-char "\t") #\tab))

;; spaces (zero or more)
(define sp (many space-char))

;; space (one or more)
(define space (many1 space-char))

(module+ test
  (check-exn exn:fail? (lambda () (parse-result space"")))
  (check-equal? (parse-result space " ") '(#\space))
  (check-equal? (parse-result space "  ") '(#\space #\space))
  (check-equal? (parse-result space " \t ") '(#\space #\tab #\space)))


(module+ test
  (check-equal? (parse-result sp "  ") '(#\space #\space))
  (check-equal? (parse-result sp "  \t") '(#\space #\space #\tab))
  (check-equal? (parse-result sp "  1\t") '(#\space #\space)))

;; newline characters
(define newline (por (char #\newline) (string "\r\n")))

(module+ test
  (check-equal? (parse-result newline "\n") #\newline)
  (check-equal? (parse-result newline "\r\n") '(#\return #\newline))
  (check-exn exn:fail? (lambda () (parse-result newline "\r"))))

;; none indent space
(define non-indent-space (por (string "   ") (string "  ") (string " ") (string "")))

(module+ test
  (check-equal? (parse-result non-indent-space "   ") '(#\space #\space #\space))
  (check-equal? (parse-result non-indent-space "  ") '(#\space #\space))
  (check-equal? (parse-result non-indent-space " ") '(#\space))
  (check-equal? (parse-result non-indent-space "") '()))

;; indent
(define indent (por (string "\t") (string "    ")))

(module+ test
  (check-equal? (parse-result indent "    ") '(#\space #\space #\space #\space))
  (check-equal? (parse-result indent "\t") '(#\tab))
  (check-exn exn:fail? (lambda () (parse-result indent "   "))))


(define str (string ""))



;; stubs
(define end-line  (string  ""))
(define ul-or-star-line  (string  ""))
(define strong  (string  ""))
(define emph  (string  ""))
(define strike  (string  ""))
(define image  (string  ""))
(define link  (string  ""))
(define code  (string  ""))

;; inline
(define inline (por str end-line ul-or-star-line space strong emph strike image link code))

