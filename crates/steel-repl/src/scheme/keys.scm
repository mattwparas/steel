;; `steel keys`: shows the bytes each key sends, to see what a terminal
;; does before teaching the line editor about it. Bytes that arrive within
;; 30ms of each other are taken to be one key.

(require-builtin #%private/steel/terminal)

(define (hex byte)
  (define digits (number->string byte 16))
  (if (< byte 16) (string-append "0" digits) digits))

(define (describe byte)
  (cond
    [(= byte 27) "ESC"]
    [(= byte 127) "DEL"]
    [(= byte 13) "RET"]
    [(= byte 9) "TAB"]
    [(= byte 0) "C-@"]
    [(< byte 27) (string-append "C-" (string (integer->char (+ byte 96))))]
    [(< byte 32) (string-append "C-" (string (integer->char (+ byte 64))))]
    [(= byte 32) "SPC"]
    [else (string (integer->char byte))]))

;; Plain ASCII byte by byte, anything else decoded as UTF-8 if it is
(define (describe-key key)
  (if (null? (filter (lambda (byte) (>= byte 128)) key))
      (string-join (map describe key) " ")
      (with-handler (lambda (_) "(not UTF-8)")
                    (string-append "\"" (bytes->string/utf8 (list->bytes key)) "\""))))

(define (pad text width)
  (define missing (- width (string-display-width text)))
  (if (> missing 0)
      (string-append text (make-string missing #\space))
      text))

;; The first byte waits as long as it takes, the rest of the key arrives with it
(define (read-key)
  (define first (read-byte/timeout #f))
  (if (eof-object? first)
      first
      (let loop ([key (list first)])
        (define next (read-byte/timeout 30))
        (if (integer? next)
            (loop (cons next key))
            (reverse key)))))

(define (show-keys)
  (let loop ()
    (define key (read-key))
    (cond
      [(eof-object? key) (displayln "eof")]
      [(equal? key '(4)) (displayln "C-d, bye")]
      [else
       (displayln (string-append (pad (string-join (map hex key) " ") 24) (describe-key key)))
       (flush-output-port (current-output-port))
       (loop)])))

(define (main)
  (unless (terminal?)
    (error "steel keys needs a terminal on stdin and stdout"))
  (displayln "Press keys to see the bytes they send. C-d quits.")
  (define size (terminal-size))
  (when size
    (displayln (string-append "Terminal size: "
                              (number->string (car size))
                              " columns, "
                              (number->string (cadr size))
                              " rows")))
  (enable-raw-mode!)
  (with-handler (lambda (err)
                  (disable-raw-mode!)
                  (raise-error err))
                (show-keys))
  (disable-raw-mode!))

(main)
