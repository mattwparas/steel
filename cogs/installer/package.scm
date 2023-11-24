;; TODO:
;; Parse the arguments from the command line, in this case either grab the path to the directory, or a git url, or the local directory
;; Parse the cog file, and check the dependencies. If any need to be downloaded, fetch those
;; Install to $STEEL_HOME/cogs to make it available for anything to download
;; Version resolution... for now just assume everything is compatible with everything without versions
;; Storing versions in a manifest would be nice - a project has an associated manifest that pins versions.

;; Load in contracts for stress testing
(require "steel/result")

(provide package-installer-main)

(define (append-with-separator path)
  (if (ends-with? path "/") (string-append path "cogs") (string-append path "/cogs")))

;; Should make this lazy?
(define *STEEL_HOME* (~> "STEEL_HOME" (env-var) (append-with-separator)))

(define/contract (parse-cog module)
  (->/c string? list?)
  (if (is-dir? module)
      (let ([cog-path (string-append module "/cog.scm")])
        (if (is-file? cog-path)
            ;; Update the resulting map with the path to the module
            (list (hash-insert (parse-cog-file cog-path) 'path module))

            (hash-values->list (discover-cogs module))))
      (error! "Unable to locate the module " module)))

;; Parses a cog file directly into a hashmap
(define/contract (parse-cog-file path)
  (->/c string? hash?)
  (define contents (let ([file (open-input-file path)]) (read-port-to-string file)))
  (transduce (read! contents) (mapping cdr) (into-hashmap)))

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

;; Given a package spec, install that package directly to the file system
(define/contract (install-package package)
  (->/c hash? string?)
  (define destination
    (string-append *STEEL_HOME* "/" (symbol->string (hash-get package 'package-name))))
  (copy-directory-recursively! (hash-get package 'path) destination)
  destination)

;; Given a package pec, uninstall that package by deleting the contents of the installation
(define/contract (uninstall-package package)
  (->/c hash? string?)
  (define destination
    (string-append *STEEL_HOME* "/" (symbol->string (hash-get package 'package-name))))
  (displayln destination))

(define/contract (install-package-and-log cog-to-install)
  (->/c hash? void?)
  (let ([output-dir (install-package cog-to-install)])
    (display "✅ Installed package to: ")
    (displayln output-dir)
    (newline)))

(define (check-install-package installed-cogs cog-to-install)
  (define package-name (hash-get cog-to-install 'package-name))
  (if (hash-contains? installed-cogs package-name)
      (begin
        (displayln "Beginning installation for " package-name)
        (displayln "    Package already installed...")
        (displayln "    Overwriting existing package installation...")
        (install-package-and-log cog-to-install))

      (begin
        (displayln "Package is not currently installed.")
        (install-package-and-log cog-to-install))))

(define (parse-cogs-from-command-line)
  (if (empty? std::env::args) (list (current-directory)) std::env::args))

(define (package-installer-main)

  (define cogs-to-install (parse-cogs-from-command-line))

  ;; Grab the map of installed cogs on the file system.
  ;; We will check if the cog is already installed before patching over the directory
  (define installed-cogs (discover-cogs *STEEL_HOME*))

  (transduce cogs-to-install
             (flat-mapping parse-cog)
             (into-for-each (lambda (x) (check-install-package installed-cogs x)))))
