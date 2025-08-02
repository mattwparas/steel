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
                  run-dylib-installation
                  find-dylib-name))

(require-builtin steel/http as http.)

;; Used for detecting if directories/files have changed
(require "crypt.scm")

(provide package-installer-main
         parse-cog
         parse-cog-file
         create-dylib-index
         parse-cog-file
         install-package
         install-package-and-log
         *STEEL_HOME*
         check-install-package
         walk-and-install
         uninstall-package
         *DYLIB-DIR*
         *BIN*)

(struct InstallOptions (force dry-run) #:transparent)

(define (make-options #:force [force #f] #:dry-run [dry-run #f])
  (InstallOptions force dry-run))

(define SEP (if (equal? (current-os!) "windows") "\\" "/"))

(define (append-with-separator path dir)
  (if (ends-with? path SEP) (string-append path dir) (string-append path SEP dir)))

(define (convert-path path)
  (if (equal? (current-os!) "windows") (string-replace path "/" "\\") path))

;; Should make this lazy?
(define *STEEL_HOME* (~> (steel-home-location) (append-with-separator "cogs")))
(define *NATIVE-SOURCES-DIR* (~> (steel-home-location) (append-with-separator "sources")))
(define *COG-SOURCES* (~> (steel-home-location) (append-with-separator "cog-sources")))
(define *DYLIB-DIR* (~> (steel-home-location) (append-with-separator "native")))
(define *BIN* (~> (steel-home-location) (append-with-separator "bin")))

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define (shebang-line)
  "#!/usr/bin/env steel")

(define (push-path root p)
  (convert-path (append-with-separator root p)))

(define (directories-equal-excluding-target? a b)
  (define a-target (path-exists? (push-path a "Cargo.toml")))
  (define b-target (path-exists? (push-path b "Cargo.toml")))
  (directories-equal? a
                      b
                      #:ignore-a (if a-target (hashset (push-path a "target")) #f)
                      #:ignore-b (if b-target (hashset (push-path b "target")) #f)))

;;@doc
;; Given a package spec, install that package directly to the file system
(define (install-package package #:force [force #f] #:dry-run [dry-run #f])
  (define destination
    (convert-path (string-append *STEEL_HOME* "/" (symbol->string (hash-get package 'package-name)))))

  (displayln "=> Installing: " package)
  (displayln "   ...Installing to:" destination)

  ;; Check if the package has changed by comparing the hashes of the directories.
  (define package-changed?
    (if (path-exists? destination)
        (begin
          (displayln "Checking if the paths are equivalent: " (hash-get package 'path) destination)
          (not (directories-equal-excluding-target? (hash-get package 'path) destination)))
        #t))

  (if package-changed? (displayln "Package has changed.") (displayln "Package has not changed."))

  (when (not package-changed?)
    ;; Try walking the deps
    (walk-and-install package #:force force #:dry-run dry-run)
    (return! destination))

  (when (path-exists? destination)
    (delete-directory! destination))

  ;; Install the package cog sources to the target location.
  ;; When this package does not have any dylibs, this is a trivial copy to the
  ;; sources directory.

  (unless dry-run
    (copy-directory-recursively! (hash-get package 'path) destination))

  (when (and (hash-contains? package 'entrypoint)
             (not dry-run)
             (not (equal? (current-os!) "windows")))
    (define entrypoint-spec (apply hash (hash-get package 'entrypoint)))
    (define executable-name (hash-get entrypoint-spec '#:name))
    (define executable-path (hash-get entrypoint-spec '#:path))
    ;; Path to the entrypoint should go here, and since it is most likely expressed
    ;; as a path relative to the cog.scm file, we should expand the path.
    (define path-to-entrypoint
      (convert-path (append-with-separator (hash-get package 'path) executable-path)))

    (define destination-binary (convert-path (append-with-separator *BIN* executable-name)))

    (displayln "-----> Discovered entrypoint:" path-to-entrypoint)
    (displayln "-----> Entrypoint name:" executable-name)
    (displayln "-----> Resulting executable location:" destination-binary)

    (let ([binary-file (open-output-file destination-binary)])

      (write-string (shebang-line) binary-file)
      (newline binary-file)
      (let ([file (open-input-file path-to-entrypoint)])
        (write-string (read-port-to-string file) binary-file)
        (close-input-port file))
      (close-output-port binary-file))

    (~> (command "chmod" (list "755" destination-binary))
        spawn-process
        Ok->value
        wait->stdout
        Ok->value))

  (displayln "=> Copied package over to: " destination)

  (walk-and-install package #:force force #:dry-run dry-run)

  destination)

(define (find pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) (car lst)]
    [else (find pred (cdr lst))]))

(define (install-dylib-from-spec package dylib-dependency)

  (displayln "Attempting to install: " dylib-dependency)

  (cond
    ; If we have urls for dylibs, instead of manually installing
    ; the dylibs with Cargo
    ;
    ; We will download files at the specified `url` into the correct `dylib-path`
    [(hash-contains? dylib-dependency '#:urls)

      ; Name of the dylib dependency.
      ;
      ; Excludes:
      ; - File extension (e.g. `".dll"`)
      ; - Prefix (e.g. `"lib"`)
     (define dylib-name (hash-ref dylib-dependency '#:name))

     ; e.g. `x86_64-linux`
     (define target
       (string-append
         (target-arch!)
         "-"
         (current-os!)))
     
     ; Example:
     ;
     ; (
     ;  #:platform
     ;  "x86_64-linux"
     ;  #:url
     ;  "https://example.com/example.so"
     ; )
     (define url-entry
       (find (lambda (url-entry)
             (let ([m (apply hash url-entry)])
               (string=? (hash-ref m '#:platform) target)))
           (hash-ref dylib-dependency '#:urls)))

     (unless url-entry
       (displayln "dylib not found for target: " target
       (return! void)))

     ; URL to the dylib, e.g. `"https://example.com/example.so"`
     (define url (hash-ref (apply hash url-entry) '#:url))

     ; Must be `https` connection, otherwise the file could be spoofed
     ; to serve a malicious dylib
     (unless
       (starts-with? url "https")
       (displayln "url " url " must be `https://` for security purposes")
       (return! void))

     ; Where the dylib will be downloaded
     ;
     ; e.g. `"~/.steel/native/libexample.so"`
     (define dylib-path
       (string-append
         *DYLIB-DIR*
         (path-separator)
         (platform-dll-prefix!)
         dylib-name
         "."
         (platform-dll-extension!)))
     
     (http.download-file! url dylib-path)
    ]
        
    [(hash-contains? dylib-dependency '#:git-url)
     (download-and-install-library (hash-ref dylib-dependency '#:name)
                                   (hash-ref dylib-dependency '#:git-url)
                                   #:subdir (or (hash-try-get dylib-dependency '#:subdir) "")
                                   #:sha (or (hash-try-get dylib-dependency '#:sha) void))]

    [(hash-contains? dylib-dependency '#:workspace-root)
     (define source
       (append-with-separator (hash-get package 'path) (hash-get dylib-dependency '#:workspace-root)))
     (run-dylib-installation source #:subdir (or (hash-try-get dylib-dependency '#:subdir) ""))]

    [else
     (run-dylib-installation (append-with-separator *STEEL_HOME*
                                                    (symbol->string (hash-ref package 'package-name)))
                             #:subdir (or (hash-try-get dylib-dependency '#:subdir) ""))]))

(define (list-package-index)
  (eval '(require "steel/packages/packages.scm"))
  (eval 'package-index))

(define (install-package-from-pkg-index index package)

  ;; TODO: Cache this result from list-package-index
  (define pkg-index (list-package-index))

  (define remote-pkg-spec
    (hash-ref pkg-index (if (symbol? package) package (string->symbol package))))

  (define git-url (hash-ref remote-pkg-spec '#:url))
  (define subdir (or (hash-try-get remote-pkg-spec '#:path) ""))
  ;; Pass the path down as well - so that we can install things that way
  (define package-spec (download-cog-to-sources-and-parse-module package git-url #:subdir subdir))

  (check-install-package index package-spec))

;; TODO: Decide if we actually need the package spec here
(define (fetch-and-install-cog-dependency-from-spec cog-dependency
                                                    #:search-from [search-from #f]
                                                    #:force [force #f]
                                                    #:dry-run [dry-run #f])

  ;; TODO: Figure out a way to resolve if the specified package is
  ;; the correct package.
  (when (and (not force) (package-installed? (hash-ref cog-dependency '#:name)))
    (displayln "=> Package already installed:" (hash-ref cog-dependency '#:name)))

  ;; For each cog, go through and install the package to the `STEEL_HOME` directory.
  ;; This should not only check if the package is installed, but also check
  ;; if the package manifest matches the one that we have. If there is a change, we
  ;; should update the installation accordingly.
  (when (or (not (package-installed? (hash-ref cog-dependency '#:name)))
            force
            ;; If its a local path, always attempt to resolve it
            (hash-contains? cog-dependency '#:path)
            (hash-contains? cog-dependency '#:git-url))
    (cond
      ;; First, attempt to resolve it via the git url if it is provided.
      [(hash-contains? cog-dependency '#:git-url)

       (displayln "-- Git url found for: " (hash-ref cog-dependency '#:name))

       (let ([package (download-cog-to-sources-and-parse-module
                       (hash-ref cog-dependency '#:name)
                       (hash-ref cog-dependency '#:git-url)
                       #:subdir (or (hash-try-get cog-dependency '#:subdir) "")
                       #:sha (or (hash-try-get cog-dependency '#:sha) void))])

         (install-package package #:force force #:dry-run dry-run))]

      ;; Attempt to find the local path to the package if this is
      ;; just another package installed locally.
      [(hash-contains? cog-dependency '#:path)
       (define source (hash-get cog-dependency '#:path))
       (define spec (car (parse-cog source search-from)))
       (install-package spec #:force force #:dry-run dry-run)]

      ;; We're unable to find the package! Logically, here would be a place
      ;; we'd check against some kind of package index to help with this.
      [else
       (displayln "Attempting to resolve from the package index.")
       (define package-index (list-package-index))
       ;; Check if the package exists in the package index.
       ;; If it does, we can go ahead and install it into our fake
       ;; hash. Right now we pretty much install everything sequentially,
       ;; which is not the worst thing since we have to discover dependencies
       ;; along the way, but something better would be to try to find all
       ;; the leaves, and then we can install in one big command
       (if (hash-contains? package-index (hash-ref cog-dependency '#:name))
           (install-package-from-pkg-index (hash) (hash-ref cog-dependency '#:name))
           (error "Unable to resolve module!: " cog-dependency))])))

;; Go through each of the dependencies, and install the cogs
;; and subsequently go through each of the dylibs, and install
;; those as well.
(define (walk-and-install package #:force [force #f] #:dry-run [dry-run #f])

  (define current-path (hash-try-get package 'path))
  (define maybe-canonicalized (if current-path (canonicalize-path current-path) current-path))

  ;; Check the direct cog level dependencies
  (for-each (lambda (d)
              (fetch-and-install-cog-dependency-from-spec d
                                                          #:search-from maybe-canonicalized
                                                          #:force force
                                                          #:dry-run dry-run))
            (hash-ref package 'dependencies))

  ;; Check the dylibs next
  (for-each (lambda (spec) (install-dylib-from-spec package spec))
            (or (hash-try-get package 'dylibs) '())))

;;@doc
;; Checks if the package has been installed by querying against the file system.
;; Does not currently check the in memory index, since this could be done during the
;; package installation process where the index is constantly getting updated.
(define (package-installed? name)
  (define destination (string-append *STEEL_HOME* "/" (if (string? name) name (symbol->string name))))
  (path-exists? destination))

;; Given a package spec, uninstall that package by deleting the contents of the installation
(define/contract (uninstall-package package)
  (->/c hash? string?)
  (define destination
    (string-append *STEEL_HOME* "/" (symbol->string (hash-get package 'package-name))))
  (displayln "Deleting:" destination)

  ;; Check if this produced a dylib, and if so, delete it
  (when (hash-contains? package 'dylibs)
    (let ([cargo-toml-path (append-with-separator destination "Cargo.toml")])
      (when (path-exists? cargo-toml-path)
        (define dylib-name (find-dylib-name cargo-toml-path))
        (define dylib-path (append-with-separator *DYLIB-DIR* dylib-name))
        ;; Delete the dylib. If it doesn't exist, we can continue on.
        (if (path-exists? dylib-path) (delete-file! dylib-path) (displayln "Dylib not found.")))))

  (delete-directory! destination)

  destination)

(define (install-package-and-log cog-to-install [force #f])
  (let ([output-dir (install-package cog-to-install #:force force)])
    (display "✅ Installed package to: ")
    (displayln output-dir)
    (newline)))

(define (check-install-package installed-cogs cog-to-install [force #f])
  (define package-name (hash-get cog-to-install 'package-name))
  (if (hash-contains? installed-cogs package-name)
      (begin
        (displayln "Beginning installation for:" package-name)
        (displayln "    Package already installed...")
        (displayln "    Overwriting existing package installation...")
        (install-package-and-log cog-to-install force))

      (begin
        (displayln "Package is not currently installed.")
        (install-package-and-log cog-to-install force))))

(define (parse-cogs-from-command-line)
  (if (empty? std::env::args) (list (current-directory)) std::env::args))

(define (package-installer-main)

  (define cogs-to-install (parse-cogs-from-command-line))

  ;; Grab the map of installed cogs on the file system.
  ;; We will check if the cog is already installed before patching over the directory
  (define installed-cogs (discover-cogs *STEEL_HOME*))

  (when (not (path-exists? *DYLIB-DIR*))
    (displayln "dylib directory does not exist, creating now...")
    (create-directory! *DYLIB-DIR*))

  (when (not (path-exists? *BIN*))
    (displayln "bin directory does not exist, creating now...")
    (create-directory! *BIN*))

  (transduce cogs-to-install
             (flat-mapping parse-cog)
             (into-for-each (lambda (x) (check-install-package installed-cogs x)))))

;; Check what dylibs are available
(define (create-dylib-index)
  (transduce (read-dir *NATIVE-SOURCES-DIR*) (mapping file-name) (into-hashset)))
