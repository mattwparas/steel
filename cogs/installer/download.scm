;; Download a package from git - These should be stored in a reasonable location, probably under the
;; $STEEL_HOME directory.

(require-builtin steel/process)
(require-builtin steel/git)
(require "steel/result")
(require "parser.scm")

(provide maybe-git-clone
         in-directory
         run-dylib-installation
         download-and-install-library
         download-cog-to-sources-and-parse-module
         wait-for-jobs
         find-dylib-name)

(define SEP (if (equal? (current-os!) "windows") "\\" "/"))

(define (append-with-separator path dir)
  (if (ends-with? path SEP) (string-append path dir) (string-append path SEP dir)))

(define (path-from-steel-home dir)
  (~> (steel-home-location) (append-with-separator dir)))

(define *COG_DIR* (path-from-steel-home "cogs"))
(define *COG-SOURCES* (path-from-steel-home "cog-sources"))
(define *NATIVE_SOURCES_DIR* (path-from-steel-home "sources"))
(define *DYLIB-DIR* (path-from-steel-home "native"))
(define *CARGO_TARGET_DIR* (path-from-steel-home "target"))

;;@doc
;; Most likely should use gix here instead of shelling out to git?
;; Use the sha to pin to a specific commit, if interested
(define (maybe-git-clone package-name https-address installation-dir #:sha (*sha* void))

  (define resulting-path
    (string-append installation-dir
                   "/"
                   (if (symbol? package-name) (symbol->string package-name) package-name)))

  (displayln "Fetching package from git: " package-name)

  ;; Delete the target directory if it already exists
  (when (path-exists? resulting-path)
    ; (display "Clearing the target directory since it already exists: ")
    ; (displayln resulting-path)
    ; (delete-directory! resulting-path)

    (displayln "Updating git repo from remote...")

    ; (~> (command "git" (list "pull")) (in-directory resulting-path) spawn-process Ok->value wait)

    (git-pull resulting-path #f (if (void? *sha*) #f *sha*))

    (return! resulting-path))

  ;; Git clone command, run against specific directory. For now we're going to
  ;; naively install them all into the same spot.
  ; (~> (command "git" (list "clone" https-address resulting-path)) (spawn-process) (Ok->value) (wait))

  (git-clone https-address resulting-path (if (not (void? *sha*)) *sha* #f))

  ;; If we have a SHA, check out that commit
  ; (when (not (void? *sha*))

  ;   (display "...Checking out sha: ")
  ;   (displayln *sha*)

  ;   (~> (command "git" (list "checkout" *sha*))
  ;       (in-directory resulting-path)
  ;       (spawn-process)
  ;       (Ok->value)
  ;       (wait)))

  resulting-path)

;; Run the process in the given directory
(define (in-directory command directory)
  (set-current-dir! command directory)
  command)

(define (with-env-var command key value)
  (set-env-var! command key value)
  command)

;; Jobs that need to run.
(define *jobs* '())

(define (wait-for-jobs)
  (unless (empty? *jobs*)
    (displayln "Waiting for dylib builds to finish")
    (if (feature-dylib-build?) (for-each thread-join! *jobs*) (for-each wait *jobs*))))

;; Run the cargo-steel-lib installer in the target directory
; (define (run-dylib-installation target-directory #:subdir [subdir ""])
;   (wait (run-dylib-installation-in-background target-directory #:subdir subdir)))

;; At the end, we're gonna await the jobs to finish compilation
(define (run-dylib-installation target-directory #:subdir [subdir ""])
  (run-dylib-installation-in-background target-directory #:subdir subdir))

(define (run-dylib-installation-in-background target-directory #:subdir [subdir ""])
  (define target (append-with-separator target-directory subdir))
  (displayln "Running dylib build in: " target)
  (if (feature-dylib-build?)
      ;; Kick off on a new thread
      ;; TODO: Properly parallelize this
      (begin
        (#%build-dylib (list "--manifest-path" (append-with-separator target "Cargo.toml"))
                       (list (list "CARGO_TARGET_DIR"
                                   (append-with-separator *CARGO_TARGET_DIR*
                                                          (file-name target-directory)))))

        (displayln "Finished building"))

      ;; This... should be run in the background?
      (~> (command "cargo-steel-lib" '())
          (in-directory target)
          (with-env-var "CARGO_TARGET_DIR"
                        (append-with-separator *CARGO_TARGET_DIR* (file-name target-directory)))
          spawn-process
          Ok->value
          wait)))

;;@doc
;; Download cog source to sources directory, and then install from there.
;; Returns the resulting cog module hash. Will fail if the subdirectory
;; given does not contain a cog.scm file.
(define (download-cog-to-sources-and-parse-module library-name
                                                  git-url
                                                  #:subdir [subdir ""]
                                                  #:sha [*sha* void])

  (define found-library-name
    (if (void? library-name)
        (~> (split-many git-url "/") last (trim-end-matches ".git"))
        library-name))

  (~> (maybe-git-clone found-library-name git-url *COG-SOURCES* #:sha *sha*)
      ;; If we're attempting to install the package from a subdirectory of
      ;; git urls, we should do that accordingly here.
      (append-with-separator subdir)
      parse-cog
      car))

;;@doc
;; Download and install the dylib library!
(define (download-and-install-library library-name git-url #:subdir [subdir ""] #:sha [*sha* void])
  (~> (maybe-git-clone library-name git-url *NATIVE_SOURCES_DIR*)
      (run-dylib-installation #:subdir subdir)))

;; Attempt to get the toml - This should actually just expand to the function to parse it,
;; otherwise return a function that can't do anything with it if the toml library
;; isn't present.
(define (try-parse-toml str)
  ;; Include the dylib if relevant
  (eval '(#%require-dylib "libsteel_toml" (only-in toml->value string->toml)))
  (eval `(toml->value (string->toml ,str))))

;; TODO: Implement some proper error handling, assuming we can't discover
;; the reason for not finding the lib name
(define (find-dylib-name path)
  (define contents (read-port-to-string (open-input-file path)))
  (define toml-contents (try-parse-toml contents))

  (define current-os (current-os!))

  ;; TODO: Replace with the constants where `current-os!` is defined
  ;; they have the shared prefix and shared suffix information
  (define *file-extension*
    (cond
      [(equal? current-os "macos") ".dylib"]
      [(equal? current-os "linux") ".so"]
      [(equal? current-os "windows") ".dll"]
      [else ".so"]))

  (string-append "lib" (~> toml-contents (hash-ref "lib") (hash-ref "name")) *file-extension*))
