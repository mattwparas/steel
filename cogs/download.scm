;; Download a package from git - These should be stored in a reasonable location, probably under the
;; $STEEL_HOME directory.

(require-builtin steel/process)
(require "steel/result")

;; Sources!
(define (append-with-separator path dir-name)
  (if (ends-with? path "/") (string-append path dir-name) (string-append path "/" dir-name)))

(define (path-from-steel-home dir)
  (~> "STEEL_HOME" (env-var) (unwrap-ok) (append-with-separator dir)))

(define *COG_DIR* (path-from-steel-home "cogs"))
(define *NATIVE_SOURCES_DIR* (path-from-steel-home "sources"))

;; Most likely should use gix here instead of shelling out to git?
;; Use the sha to pin to a specific commit, if interested
(define (git-clone package-name https-address installation-dir #:sha (*sha* void))
  (define resulting-path (string-append installation-dir "/" package-name))
  ;; Git clone command, run against specific directory. For now we're going to
  ;; naively install them all into the same spot.
  (~> (command "git" (list "clone" https-address resulting-path)) (spawn-process) (Ok->value) (wait))
  ;; If we have a SHA, check out that commit
  (when (not (void? *sha*))
    (~> (command "git" (list "checkout" *sha*))
        (in-directory resulting-path)
        (spawn-process)
        (Ok->value)
        (wait)))

  resulting-path)

;; Run the process in the given directory
(define (in-directory command directory)
  (set-current-dir! command directory)
  command)

;; Run the cargo-steel-lib installer in the target directory
(define (run-dylib-installation target-directory)
  (~> (command "cargo-steel-lib" '())
      (in-directory target-directory)
      (spawn-process)
      (Ok->value)
      (wait)))

;; TODO: steps
;; - git clone to temporary directory (or site-packages style thing, something)
;; Probably install native dylibs to their own native section
;; Then, run the installation script.

;; Install to the im-lists directory. What we probably have to do is install it to some
;; temporary location, parse the module name, and move it back out. Unless - we do something
;; like the org name, but I don't love that.
; (git-clone "im-lists" "https://github.com/mattwparas/im-lists.git" *NATIVE_SOURCES_DIR*)

;; Download and install the library!
; (define (download-and-install-library library-name git-url)
;   (~> (git-clone library-name git-url *NATIVE_SOURCES_DIR*)
;       (run-dylib-installation installation-directory)))

;; TODO: Publish steel in its currently form, extremely experimental.
;; Once steel is in a stable position, dylibs can reference the dependency by version
;; on crates.io

;;
(git-clone "helix-configuration"
           "https://github.com/mattwparas/helix-config.git"
           *COG_DIR*
           #:sha "ae01ad7a3e7a48dad0ddbe8b812ab162aba31732")
