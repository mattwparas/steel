(define package-name 'installer)
(define version "0.1.0")

(define dependencies '((#:name steel/command-line #:path "../command-line")))

;; Create bin directory for entrypoints, and then
;; copy this file to that location, installing a
;; shebang on it?
;;
;; That would then work
(define entrypoint '(#:name "cog" #:path "spm.scm"))
