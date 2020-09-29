


;; goal: make web requests act concurrently
;;   i.e.
;; make

;; (async-exec
;;  (get "www.google.com")
;;  (get "blargh.com/index.html"))

 ;; => '(list-of whatever get returns)

;; super duper basic executor
(async-exec
 (async-test-func)
 (async-test-func)
 (async-test-func))

;; executors are the only way to run futures to completion
;; Right now, async-exec is simply a wait group
(async-exec
 (async-get)
 (async-get)
 (async-get)
 (async-get)
 (async-get)
 (async-get)
 (async-get)
 (async-get)
 (async-get)
 (async-get))
