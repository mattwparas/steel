(define package-name 'steel-webrequests)
(define version "0.1.0")
(define dependencies '())
; '((#:name "steel-websockets"
;           #:git-url "https://github.com/mattwparas/steel.git"
;           #:subdir "crates/steel-websockets")
;   (#:name "steel-webrequests"
;           #:git-url "https://github.com/mattwparas/steel.git"
;           #:subdir "crates/steel-webrequests")))

(define dylibs
  '((#:name "steel-webrequests" #:workspace-root "../.." #:subdir "libs/steel-webrequests")))
