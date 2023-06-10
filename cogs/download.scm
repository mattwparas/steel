;; Download a package from git - These should be stored in a reasonable location, probably under the
;; $STEEL_HOME directory.

(require-builtin steel/process)
(require "steel/result")

(define (append-with-separator path)
  (if (ends-with? path "/") (string-append path "cogs") (string-append path "/cogs")))

(define *STEEL_HOME* (~> "STEEL_HOME" (env-var) (unwrap-ok) (append-with-separator)))

;; Use the sha to pin to a specific commit, if interested
(define (git-clone package-name https-address installation-dir #:sha (*sha* void))
  ;; Git clone command, run against specific directory. For now we're going to
  ;; naively install them all into the same spot.
  (~> (command "git" (list "clone" https-address (string-append installation-dir "/" "im-lists")))
      (spawn-process)
      (Ok->value)
      (wait)))

;; Install to the im-lists directory. What we probably have to do is install it to some
;; temporary location, parse the module name, and move it back out. Unless - we do something
;; like the org name, but I don't love that.
(git-clone "im-lists" "https://github.com/mattwparas/im-lists.git" *STEEL_HOME*)

;; TODO: steps
;; - git clone to temporary directory (or site-packages style thing, something)
;; Probably install native dylibs to their own native section
;; Then, run the installation script.
