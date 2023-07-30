#!/usr/bin/env python3

from math import factorial
from math import log
from math import sqrt

second = 1e6
minute = second * 60
hour = minute * 60
day = hour * 24
month = day * 30
year = month * 12
century = year * 100

timings = {
    "second": second,
    "minute": minute,
    "hour": hour,
    "day": day,
    "month": month,
    "year": year,
    "century": century,
}

orders = {
    "lg n": lambda n: log(n, 10),
    "√n": lambda n: sqrt(int(n)),
    "n": lambda n: n,
    "n lg n": lambda n: n * log(n, 10),
    "n²": lambda n: n**2,
    "n³": lambda n: n**3,
    "2^n": lambda n: 2**n,
    "n!": lambda n: factorial(int(n)),
}

if __name__ == "__main__":
    for order, fn in orders.items():
        for timing, ms in timings.items():
            i, n = 0, 10
            while True:
                if fn(n) < ms:
                    i = n
                    n = n * 1.001
                else:
                    print(f"{order} {timing}: n={i:.3e}")
                    break
