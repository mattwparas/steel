(require "steel/command-line/args.scm")
(require "installer/package.scm")
(require "installer/parser.scm")
(require "installer/download.scm")

(define my-options
  (make-command-line-arg-parser #:positional (list '("command" "The subcommand to run"))
                                ; #:required '((("list" #f) "Setting up the values")))
                                ))

(define list-parser
  (make-command-line-arg-parser #:required '((("path" #f) "Path to discover packages"))))

(define (list-packages index)
  (define package-name-width
    (apply max
           (map (lambda (package) (string-length (symbol->string (hash-ref package 'package-name))))
                (hash-values->list index))))

  (define version-width (string-length "Version"))

  (displayln "Listing packages from: " *STEEL_HOME*)
  (displayln)

  (display "Package")
  (display (make-string (- package-name-width (string-length "Package")) #\SPACE))
  (display "  ")
  (displayln "Version")

  (display (make-string package-name-width #\-))
  (display "  ")
  (displayln (make-string version-width #\-))

  ;; Installed packages
  (for-each (lambda (package)
              (define package-name (hash-ref package 'package-name))
              (display package-name)
              (display (make-string (- package-name-width
                                       (string-length (symbol->string package-name)))
                                    #\SPACE))
              (display " ")
              (displayln (hash-ref package 'version)))
            (hash-values->list index)))

;; TODO: Move this to `installer/package.scm`
(define (refresh-package-index index)
  (define package-spec
    (download-cog-to-sources-and-parse-module "steel/packages"
                                              "https://github.com/mattwparas/steel-packages.git"))
  (check-install-package index package-spec))

;; TODO: Move this to `installer/package.scm`
(define (list-package-index)
  (eval '(require "steel/packages/packages.scm"))
  (eval 'package-index))

(define (print-package-index)
  (transduce (list-package-index)
             (into-for-each (lambda (p)
                              (display (car p))
                              (display " ")
                              (displayln (cadr p))))))

(define (install-package-temp index args)
  (define cogs-to-install (if (empty? args) (list (current-directory)) args))
  (transduce cogs-to-install
             (flat-mapping parse-cog)
             (into-for-each (lambda (x) (check-install-package index x)))))

;; TODO: Move this to `installer/package.scm`
(define (install-package-from-pkg-index index package)
  (define pkg-index (list-package-index))
  (define remote-pkg-spec (hash-ref pkg-index (string->symbol package)))
  (define git-url (hash-ref remote-pkg-spec '#:url))
  (define subdir (or (hash-try-get remote-pkg-spec '#:path) ""))
  ;; Pass the path down as well - so that we can install things that way
  (define package-spec (download-cog-to-sources-and-parse-module package git-url #:subdir subdir))

  (check-install-package index package-spec))

(define (install-dependencies index)
  (error "TODO"))

(define (install-dependencies-and-run-entrypoint index args)
  (error "TODO"))

(define (render-help)
  (displayln
   "Cog - the Steel Packager Manager

Usage:
  cog <command> [options]

Commands:
  list           List the installed packages
  install        Install a local package

  pkg <command>  Subcommand for the package repository
  pkg refresh    Update the package repository from the remote
  pkg list       List the packages in the remote index
  pkg install    Install a package from the remote index"))

(provide main)
(define (main)
  (define package-index (discover-cogs *STEEL_HOME*))
  (define command-line-args (drop (command-line) 2))
  (let ([command (car command-line-args)])
    ;; Dispatch on the command
    (cond
      [(equal? "help" command) (render-help)]
      [(equal? "--help" command) (render-help)]
      ;; list the packages
      [(equal? "list" command) (list-packages package-index)]

      ;; Build the dependencies
      [(equal? "build" command) (install-dependencies package-index)]

      ;; Run the entrypoint as specified in the cog.scm, if present.
      [(equal? "run") (install-dependencies-and-run-entrypoint package-index (cdr command-line-args))]

      ;; install the given package
      [(equal? "install" command) (install-package-temp package-index (cdr command-line-args))]
      ;; Try to install the package index from the remote
      [(equal? '("pkg" "refresh") command-line-args) (refresh-package-index package-index)]
      ;; List the remote package index
      [(equal? '("pkg" "list") command-line-args) (print-package-index)]

      ;; Install package from remote
      [(equal? '("pkg" "install") (take command-line-args 2))
       (install-package-from-pkg-index package-index (list-ref command-line-args 2))]

      ;; No matching command
      [else (displayln "No matching command: " command)])))
