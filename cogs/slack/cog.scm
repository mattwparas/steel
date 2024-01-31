(define package-name 'slack/websocket)
(define version "0.1.0")

;; Core library, requires no dependencies.
; (define dependencies '((#:name "xyz-library") (#:name "foo-bar-library")))
(define dependencies '((#:name steel-websockets #:path "../libs/steel-websockets")))

; '((#:name "steel-websockets"
;           #:git-url "https://github.com/mattwparas/steel.git"
;           #:subdir "crates/steel-websockets")
;   (#:name "steel-webrequests"
;           #:git-url "https://github.com/mattwparas/steel.git"
;           #:subdir "crates/steel-webrequests")))

; (define dylibs
;   '((#:name "steel-websockets"
;             ; #:git-url "https://github.com/mattwparas/steel.git"
;             #:subdir "crates/steel-websockets")
;     (#:name "steel-webrequests"
;             ; #:git-url "https://github.com/mattwparas/steel.git"
;             #:subdir "crates/steel-webrequests")))
