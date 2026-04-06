# 03-11-25 - euler's totient
from math import gcd

# an efficient implementation of Euler's Totient function
# counts the coprime integers from 1 to n for n >= 1
# algorithm:
#   1. start with result = n
#   2. if 2 divides n, subtract result/2 until n is odd
#   3. for each odd factor f from 3 to sqrt(n):
#        if f divides n, subtract result/f until n is not divisible by f
#   4. if n > 1, subtract result/n
#   5. return result (φ(n))
def phi(n: int) -> int:
    assert n >= 1, "phi is only defined for positive integers"
    result, remaining = n, n

    if remaining % 2 == 0:
        result -= result // 2
        while remaining % 2 == 0:
            remaining //= 2

    factor = 3
    while factor * factor <= remaining:
        if remaining % factor == 0:
            result -= result // factor
            while remaining % factor == 0:
                remaining //= factor
        factor += 2

    if remaining > 1:
        result -= result // remaining
    return result


# a smaller, much slower GCD-dependent version to show the basic
def tiny_phi(n: int) -> int:
    return sum(1 for f in range(1, n + 1) if gcd(n, f) == 1)


# test the functions
if __name__ == "__main__":
    for i in range(1, 21):
        print(f"{i} - big({phi(i)}), tiny({tiny_phi(i)})")