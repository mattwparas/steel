(new-make-struct Cog (name version path git))

(define (cog #:name name #:version version #:path (path void) #:git (git void))
    (Cog name version path git))

(new-make-struct Dylib (name version path git))

(define (dylib #:name name #:version version #:path (path void) #:git (git void))
    (Dylib name version path void))


(define dependencies
    (list (dylib #:name 'steel/webserver #:version "0.1.0")
          (cog #:name 'steel/contracts #:version "0.1.0")))
