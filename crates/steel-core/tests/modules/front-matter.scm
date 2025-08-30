(provide parse-file
         Page
         Page-front-matter-map
         Page-content
         Page-path)

(struct Page (path front-matter-map content))

(define (parse-file path)
  (let ([file (open-input-file path)]
        [front-matter (open-output-string)])
    (let loop ([port file]
               [close-front-matter #f])
      (let ([next-line (read-line port)])
        (cond
          ;; TODO: Fix 'eof
          [(equal? 'eof next-line)]
          [else
           ;; If it has front matter, lets see if it works
           (if (starts-with? next-line "+++")
               ;; If this is the second time we've seen it
               (if close-front-matter
                   ;; Setup the key-value map
                   (Page path
                         (transduce (read! (get-output-string front-matter)) (into-hashmap))
                         (read-port-to-string port))
                   (loop port #t))

               (begin
                 (display next-line front-matter)
                 (loop port close-front-matter)))])))))
