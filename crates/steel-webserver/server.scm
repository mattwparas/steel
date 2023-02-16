(define (handle-request path)
    "hello world!")

(define (loop)
    (->> (receiver/recv vm-receiver)
            (handle-request)
            (sender/send vm-sender))

    (loop))


(start-server command-channel)

(loop)