(define package-name 'slack/websocket)
(define version "0.1.0")

;; Core library, requires no dependencies.
(define dependencies
  '((#:name steel-websockets #:path "../libs/steel-websockets")
    (#:name steel-webrequests #:path "../libs/steel-webrequests")))
