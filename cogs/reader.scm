(require "../self_hosted/contract.scm" (for-syntax "../self_hosted/contract.scm"))

(define/c (parse-cog module)
    (->c string? hash?)
    (if (is-dir? module)
        (let ((cog-path (string-append module "/cog.scm")))
            (if (is-file? cog-path)
                (parse-cog-file cog-path)
                (error! "Unable to locate the cog file for module: " module)))
        (error! "Unable to locate the module " module)))

;; Parses a cog file directly into a hashmap
(define/c (parse-cog-file path)
    (->c string? hash?)
    (define contents (let ((file (open-input-file path))) (read-port-to-string file)))
    (transduce (read! contents)
               (mapping cdr)
               (into-hashmap)))

;; Discover the cogs located at the path, return as a list of hash maps
(define/c (discover-cogs path)
    (->c string? list?)
    (transduce (read-dir path)
               (filtering is-dir?)
               (mapping parse-cog)
               (into-list)))


; (parse-cog-file ".")

(displayln (discover-cogs "."))


; (displayln (discover-cogs '()))