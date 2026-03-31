import time

base_range = list(range(0, 100000))

results = []

start = time.time()
for i in range(100):
    results.append([x + 1 for x in base_range])
end = time.time()
print((end - start) * 1000)
