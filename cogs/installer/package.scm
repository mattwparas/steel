;; TODO:
;; Parse the arguments from the command line, in this case either grab the path to the directory, or a git url, or the local directory
;; Parse the cog file, and check the dependencies. If any need to be downloaded, fetch those
;; Install to $STEEL_HOME/cogs to make it available for anything to download
;; Version resolution... for now just assume everything is compatible with everything without versions
;; Storing versions in a manifest would be nice - a project has an associated manifest that pins versions.

;; Load in contracts for stress testing
(require "steel/result")

(require "parser.scm")
(require (only-in "download.scm"
                  download-and-install-library
                  download-cog-to-sources-and-parse-module
                  run-dylib-installation))

(provide package-installer-main
         parse-cog
         parse-cog-file
         create-dylib-index
         parse-cog-file
         install-package
         install-package-and-log)

(define (append-with-separator path dir)
  (if (ends-with? path "/") (string-append path dir) (string-append path "/" dir)))

;; Should make this lazy?
(define *STEEL_HOME* (~> "STEEL_HOME" (env-var) (append-with-separator "cogs")))
(define *NATIVE-SOURCES-DIR* (~> "STEEL_HOME" (env-var) (append-with-separator "sources")))
(define *COG-SOURCES* (~> "STEEL_HOME" (env-var) (append-with-separator "cog-sources")))

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

;;@doc
;; Given a package spec, install that package directly to the file system
(define/contract (install-package package)
  (->/c hash? string?)

  (define destination
    (string-append *STEEL_HOME* "/" (symbol->string (hash-get package 'package-name))))

  (displayln "Installing: " package)

  ; (define dylib-dependencies (or (hash-try-get package 'dylibs) '()))
  ; (define cogs-dependencies (or (hash-try-get package 'dependencies) '()))

  ;; Install the package cog sources to the target location.
  ;; When this package does not have any dylibs, this is a trivial copy to the
  ;; sources directory.
  (copy-directory-recursively! (hash-get package 'path) destination)

  (displayln "Copied package over to: " destination)

  ;; Attempt to install the package from the destination
  ; (when (not (empty? dylib-dependencies))
  ;   (for-each (lambda (dylib) (install-dylib-from-spec package dylib)) dylib-dependencies))

  (walk-and-install package)

  destination)

(define (install-dylib-from-spec package dylib-dependency)

  (displayln "Attempting to install: " dylib-dependency)

  (cond
    [(hash-contains? dylib-dependency '#:git-url)
     (download-and-install-library (hash-ref dylib-dependency '#:name)
                                   (hash-ref dylib-dependency '#:git-url)
                                   #:subdir (or (hash-try-get dylib-dependency '#:subdir) "")
                                   #:sha (or (hash-try-get dylib-dependency '#:sha) void))]

    [(hash-contains? dylib-dependency '#:workspace-root)
     (define source
       (append-with-separator (hash-get package 'path) (hash-get dylib-dependency '#:workspace-root)))
     (define destination
       (append-with-separator *NATIVE-SOURCES-DIR* (hash-get dylib-dependency '#:name)))

     (displayln "=> Copying from: " source "->" destination)

     (when (path-exists? destination)
       (delete-directory! destination))

     (copy-directory-recursively! source destination)

     (displayln "=> Finished copying!")

     (run-dylib-installation destination #:subdir (or (hash-try-get dylib-dependency '#:subdir) ""))]

    [else
     (run-dylib-installation (append-with-separator *STEEL_HOME*
                                                    (symbol->string (hash-ref package 'package-name)))
                             #:subdir (or (hash-try-get dylib-dependency '#:subdir) ""))]))

;; TODO: Decide if we actually need the package spec here
(define (fetch-and-install-cog-dependency-from-spec package cog-dependency)

  ;; For each cog, go through and install the package to the `STEEL_HOME` directory.
  ;; This should not only check if the package is installed, but also check
  ;; if the package manifest matches the one that we have. If there is a change, we
  ;; should update the installation accordingly.
  (unless (package-installed? (hash-ref cog-dependency '#:name))
    (cond
      ;; First, attempt to resolve it via the git url if it is provided.
      [(hash-contains? cog-dependency '#:git-url)

       (displayln "-- Git url found for: " (hash-ref cog-dependency '#:name))

       (let ([package (download-cog-to-sources-and-parse-module
                       (hash-ref cog-dependency '#:name)
                       (hash-ref cog-dependency '#:git-url)
                       #:subdir (or (hash-try-get cog-dependency '#:subdir) "")
                       #:sha (or (hash-try-get cog-dependency '#:sha) void))])

         ; (for-each (lambda (dylib)
         ;             (run-dylib-installation
         ;              ;; This should be where the package is downloaded to.
         ;              (append-with-separator *STEEL_HOME* (hash-ref cog-dependency '#:name))
         ;              ;; This is the subdirectory in which the individual module
         ;              ;; containing the dylib should be loaded
         ;              #:subdir (or (hash-try-get dylib '#:subdir) "")))
         ;           (or (hash-try-get package 'dylibs) '()))

         (install-package package))]

      ;; Attempt to find the local path to the package if this is
      ;; just another package installed locally.
      [(hash-contains? cog-dependency '#:path)

       (define source (hash-get cog-dependency '#:path))

       (install-package (car (parse-cog source)))]

      ;; We're unable to find the package! Logically, here would be a place
      ;; we'd check against some kind of package index to help with this.
      [else (error "Unable to resolve module!: " cog-dependency)])))

;; Go through each of the dependencies, and install the cogs
;; and subsequently go through each of the dylibs, and install
;; those as well.
(define (walk-and-install package)
  ;; Check the direct cog level dependencies
  (for-each (lambda (spec) (fetch-and-install-cog-dependency-from-spec package spec))
            (hash-ref package 'dependencies))

  ;; Check the dylibs next
  (for-each (lambda (spec) (install-dylib-from-spec package spec))
            (or (hash-try-get package 'dylibs) '())))

;;@doc
;; Checks if the package has been installed by querying against the file system.
;; Does not currently check the in memory index, since this could be done during the
;; package installation process where the index is constantly getting updated.
(define (package-installed? name)
  (define destination (string-append *STEEL_HOME* "/" (symbol->string name)))
  (path-exists? destination))

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

;; Check what dylibs are available
(define (create-dylib-index)
  (transduce (read-dir *NATIVE-SOURCES-DIR*) (mapping file-name) (into-hashset)))
