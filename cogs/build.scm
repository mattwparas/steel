(struct Cog (name version path git))

(define (cog #:name name 
             #:version version 
             #:path (path void) 
             #:git (git void))
    (Cog name version path git))

(struct Dylib (name version path git))

(define (dylib #:name name
               #:version version 
               #:path (path void) 
               #:git (git void))
    (Dylib name version path void))

;; Define runtime dependencies: These are entirely necessary to use the modules
;; defined in this package
(define dependencies
    (list (dylib #:name 'steel/webserver #:version "0.1.0")
          (cog #:name 'steel/contracts #:version "0.1.0")))

;; TODO: Put license, other things, here
