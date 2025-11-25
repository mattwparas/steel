import time

def ack1(M, N):
    return (
        (N + 1)
        if M == 0
        else (ack1(M - 1, 1) if N == 0 else ack1(M - 1, ack1(M, N - 1)))
    )


start = time.time()
for i in range(50):
    ack1(3, 3)
end = time.time()
print(end - start)
