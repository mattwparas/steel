(#%require-dylib "libsteel_websockets"
                 (only-in ws/message-ping?
                          ws/message-pong?
                          ws/message-text?
                          ws/message-ping->pong
                          ws/message->text-payload
                          ws/connect))

(provide ws/message-ping
         ws/message-pong?
         ws/message-text?
         ws/message-ping->pong
         ws/message->text-payload
         ws/connect)
