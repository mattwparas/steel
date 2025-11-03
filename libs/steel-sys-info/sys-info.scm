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

(define (current-memory-usage #:memory-info (memory-info (mem-info)))
  (- (MemoryInfo-total memory-info) (MemoryInfo-free memory-info) (MemoryInfo-cached memory-info)))

;; TODO: Clean up error message if the hash is missing keys?
(define (memory-usage-as-percentage #:memory-info (memory-info (mem-info)))
  (/ (current-memory-usage #:memory-info memory-info) (MemoryInfo-total memory-info)))
