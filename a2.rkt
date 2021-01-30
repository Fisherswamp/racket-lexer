#lang racket

(define (lex str)
  (define str_trm (string-trim str))
  (if (not (non-empty-string? str_trm))
      '()
      (let ([res (identify-longest-valid-token str_trm)])
        (if (empty? (car res))
            (lex (string-trim (cdr res)))
            (cons (car res) (lex (string-trim (cdr res))))))))

;; Find the longest valid token in the string recursively. Returns
;; the full token at its longest length, and the remaining string
;; as a pair:
;; '((TOKEN-TYPE VALUE|#f) . REMAINING-STRING)
;; example: '((FLOAT 15.41) . "\fun :)")
(define (identify-longest-valid-token str)
  ;; Ignore all comments, then:
  ;; First check if input is a valid float
  ;; Then check if input is a valid int
  (define vblock (valid-block-comment? str))
  (define vline (valid-line-comment? str))
  (define vfloat (valid-float? str))
  (define vint (valid-int? str))
  (define vname (valid-name? str))
  (define punc? (valid-punctuation-and-create? str))
  (define starts-with-quote? (eq? (string-ref str 0) #\"))
  (cond
    [(string? vblock)
     (cons '() (string-replace str vblock "" #:all? #f))]
    [(string? vline)
     (cons '() (string-replace str vline "" #:all? #f))]
    [(string? vfloat)
     (create-token 'FLOAT vfloat str #t)]
    [(string? vint)
     (create-token 'INT vint str #t)]
    [(string? vname)
     (if (reserved-name? vname)
         (cons (list (reserved-name? vname) #f) (string-replace str vname "" #:all? #f))
         (cons (list 'NAME (string->symbol vname)) (string-replace str vname "" #:all? #f)))]
    [(pair? punc?)
     punc?]
    [starts-with-quote?
     (define res (parse-as-lexed-string (substring str 1)))
     (if (eq? res 'error)
         (cons (list 'INVALID str) "")
         (cons (list 'STRING (car res)) (cdr res)))]
    [else (cons (list 'INVALID str) "")])
 
  )

;; Turns a value into a token and removes it from the string
;; returns the token and its data, paired with the remaining string:
;; '((TOKEN-TYPE VALUE|#f) . REMAINING-STRING)
;; example: '((FLOAT 15.41) . "\fun :)")
(define (create-token token value full-string convert-to-num)
  (define true-val (if convert-to-num
      (string->number value)
      value))
  (define rem-str (string-replace full-string value "" #:all? #f))
  (cons (list token true-val) rem-str))

;; checks if a string starts with a valid float -
;; if so returns that float, else returns false
(define (valid-float? str)
  (validate-regex str #rx"^[+-]?[0-9]*[.][0-9]+(?=[\r\n\t (){},;.]|$)"))

;; Hash for puntuation char to atom
(define punk-hash (hash
                   #\( 'OPAREN
                   #\) 'CPAREN
                   #\{ 'OBRACE
                   #\} 'CBRACE
                   #\, 'COMMA
                   #\; 'SEMICOLON
                   #\. 'PERIOD))

;; takes in a string and returns a tokenized version of the first character
;; along with the rest of the string as a pair if the first character is a
;; punctuation ('(',')',...etc), else returning false
(define (valid-punctuation-and-create? str)
  (if (validate-regex str #rx"^[,(){};.]")
      (cons (list (hash-ref punk-hash (string-ref str 0)) #f) (substring str 1))
      #f))

;; checks if a string starts with a valid integer -
;; if so returns that float, else returns false
(define (valid-int? str)
  (validate-regex str #rx"^[+-]?[0-9]+(?=[\r\n\t (){},;.]|$)"))

;; checks to see if a string starts with a valid block comment -
;; if so returns that comment, else returns false
(define (valid-block-comment? str)
  (validate-regex str #rx"^\\/[*](.|\n)*?[*]\\/"))

;; checks to see if a string starts with a valid line comment -
;; if so returns that comment, else returns false
(define (valid-line-comment? str)
  (validate-regex str #rx"^\\/\\/[^\n]*"))

;; check to see if a string starts with a valid name -
;; if so returns that name, else returns false
(define (valid-name? str)
  (validate-regex str #px"^[^\\d\",(){};.\\s]([^,\"(){};.\\s]*)(?=[\r\n\t (){},;.]|$)"))
;; check if a string is a reserved name, and if so return that name
(define (reserved-name? str)
  (if (regexp-match #rx"^((def)|(if)|(fun)|(not)|(and)|(or))(?=[\r\n\t (){},;.]|$)" str)
      (string->symbol (string-upcase str))
      #f))


;; checks if the string contains the regex pattern, If so returns the pattern
; else returns false
(define (validate-regex str regex)
  (define res (regexp-match regex str))
  (if (list? res)
      (first res)
      #f))
;; This function takes an input string which itself contains a string
;; and parses the string out of it. This assumes that the starting quotation
;; of the string has already been remoted.
;; returns 'error if an error, else the parsed string paired with the remaining string:
;; Example:
;; (parse-as-lexed-string " hello \\\" 5.3\" hello 5.7")
;; > '(" hello \" 5.3" . " hello 5.7")
(define (parse-as-lexed-string str)
  (letrec ([parse-rec (λ (cur-str rem-str escaped?)
                        (if (non-empty-string? rem-str)
                            (let ()
                              (define char (string-ref rem-str 0))
                              (if escaped?
                                  (let ()
                                    (define parsed-char (if (eq? char #\n)
                                                   #\newline
                                                   char))
                                    (parse-rec (string-append-immutable cur-str (list->string (list parsed-char))) (substring rem-str 1) #f))
                                  (if (eq? char #\\)
                                      (parse-rec cur-str (substring rem-str 1) #t)
                                      (if (eq? char #\") ;; If found end of string next character must be whitespace, punctuation or end of string
                                          (if (or (= (string-length rem-str) 1) (regexp-match #rx"[\\r\\n\\t (){},;.]" (list->string (list (string-ref rem-str 1)))))
                                              (cons cur-str (substring rem-str 1))
                                              'error)
                                          (parse-rec (string-append-immutable cur-str (list->string (list char))) (substring rem-str 1) #f)))))
                            'error))])
    (parse-rec "" str #f)))
;; TESTS

(module+ test
  (require (only-in rackunit
                    check-equal?))
  (check-equal? (lex "") null)
  )
#|
(lex "
def factorial = fun (n) {
  // first check base case
  if(<(n, 0.9),
     1,
     factorial(-(n, 1)) /* recursive case */ )
};

print(+(\"5! is \", factorial(5)))
")
|#