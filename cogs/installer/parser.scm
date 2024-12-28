(provide discover-cogs
         parse-cog
         parse-cog-file)

(struct DylibSpec (name git-url sha subdir workspace-root))

;; Discover the cogs located at the path, return as a list of hash maps
(define/contract (discover-cogs path)
  (->/c string? hash?)
  (when (not (path-exists? path))
    (displayln "cogs directory does not exist, creating now...")
    (create-directory! path))
  (transduce (read-dir path)
             (filtering is-dir?)
             (mapping parse-cog)
             (flattening)
             (mapping (lambda (package) (list (hash-get package 'package-name) package)))
             (into-hashmap)))

(define (convert-path path)
  (if (equal? (current-os!) "windows")
      (string-replace path "/" "\\")
      path))

(define (parse-cog module [search-from #f])
  ;; TODO: This needs to handle relative paths
  (displayln "searching for: " module)
  (if (is-dir? module)
      (let ([cog-path (convert-path (string-append module "/cog.scm"))])
        (if (is-file? cog-path)
            ;; Update the resulting map with the path to the module
            (list (hash-insert (parse-cog-file cog-path) 'path module))

            (hash-values->list (discover-cogs module))))

      (if search-from
          (begin
            ;; This is no good - need to do platform agnostic separator
            (define new-search-path
              (convert-path (string-append (trim-end-matches search-from "/") "/" module)))

            (displayln "Searching in: " new-search-path)

            (parse-cog new-search-path))

          (error! "Unable to locate the module " module))))

;; Parses a cog file directly into a hashmap
(define/contract (parse-cog-file path)
  (->/c string? hash?)
  (define contents (let ([file (open-input-file path)]) (read-port-to-string file)))
  (transduce (read! contents)
             (mapping cdr)
             (mapping (lambda (p)
                        ;; TODO: Move this out - also make sure
                        (if (member (car p) '(dylibs dependencies))
                            (list (car p)
                                  (map (lambda (spec)
                                         (if (list? spec)
                                             (apply hash spec)
                                             spec))
                                       (cadr p)))
                            p)))
             (into-hashmap)))
