import time
def fib(n):
    if n <= 2:
        return 1
    return fib(n - 1) + fib(n - 2)

start = time.time()
fib(28)
end = time.time()
print(end - start)
