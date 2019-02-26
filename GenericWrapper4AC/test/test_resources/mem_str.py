s = ["a"]

for i in range(1, 100000000):
    # To prevent receiving a MEMOUT faster than a TIMEOUT
    s = s + s[:100000]