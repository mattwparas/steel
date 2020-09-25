


;; goal: make web requests act concurrently
;;   i.e.
;; make

(async-exec
 (get "www.google.com")
 (get "blargh.com/index.html"))

 ;; => '(list-of whatever get returns)
