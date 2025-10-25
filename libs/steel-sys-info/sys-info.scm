(require-builtin steel/ffi)
; (require "helix/ext.scm")
(require "helix/commands.scm")
(require "steel/sync")

(#%require-dylib "libsteel_sys_info"
                 (only-in mem-info
                          MemoryInfo-total
                          MemoryInfo-avail
                          MemoryInfo-free
                          MemoryInfo-buffers
                          MemoryInfo-cached
                          MemoryInfo-swap-total
                          MemoryInfo-swap-free
                          register-logger
                          test-logging
                          spawn-logging))

(define (hx.ffi-with-context thunk)
  (define task (task #f))
  ;; Send on the main thread
  (acquire-context-lock thunk task)
  task)

(define (current-memory-usage #:memory-info (memory-info (mem-info)))
  (- (MemoryInfo-total memory-info) (MemoryInfo-free memory-info) (MemoryInfo-cached memory-info)))

;; TODO: Clean up error message if the hash is missing keys?
(define (memory-usage-as-percentage #:memory-info (memory-info (mem-info)))
  (/ (current-memory-usage #:memory-info memory-info) (MemoryInfo-total memory-info)))

;; Just convert it to a string first I suppose
(register-logger (function->ffi-function (lambda (line)

                                           ;; Attempt to find what the heck is happening?
                                           (log::info! (to-string (current-thread-id)))
                                           ; (hx.block-on-task (lambda () (theme "nord")))
                                           ; (hx.ffi-with-context (lambda () (theme "nord")))

                                           (log::info! (to-string line)))))
