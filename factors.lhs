Practical 1: Factoring Numbers

Here is a simple method for finding the smallest prime factor of a positive
integer:

> factor :: Integer -> (Integer, Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>    where (q,r) = n `divMod` m

for example

*Main> factor 7654321
(19,402859)

because 

*Main> 19 * 402859
7654321

Repeatedly extracting the smallest factor will return a list
of prime factors:

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>    where (p,q) = factorFrom m n

for example

*Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]

Exercise 1:

factor 0 would return (2, 0) since 0 `divMod` 2 is 0, satisfying r == 0.
factor 1 would not return anything and get stuck in a loop, since it would continue to check 1 `divMod` 2, 1 `divMod` 3, etc. Since this never returns (q, 0), the program returns bottom.


Exercise 2:

I have checked my answers to Exercise 1.


Exercise 3:

Let f be the smallest prime factor of n, and let g be the next smallest factor of n. If g doesn't exist, then n is prime, and f == n, which is not less than n. We want to find the minimum value of n. Either f == g, in which case the minimum n can be is f^2, or f != g, in which case the minimum n can be is f*g > f^2. Since n >= f^2, then f <= sqrt (n). Thus, f cannot both be greater than sqrt(n) and less than n.

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n

> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0    = (m,q)
>		  | n <= m*m  = (n,1)
>                 | otherwise = factorFrom1 (m+1) n
>    where (q,r) = n `divMod` m

The order of the guards doesn't matter between the first 2, since on the final loop, r == 0 and n <= m*m could occur in either order, but they both obviously need to be before the otherwise guard. Approximately sqrt(n) calls are required in the worst case.


Exercise 4:

Since q is equal to floor (n/m), once m becomes large enough that q < m, then m > floor (n/m), so it has checked all the numbers up to sqrt(n). It is more efficient because it doesn't have to make the additional calculation of m*m, since it already calculates q.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0    = (m,q)
>		  | q < m     = (n,1)
>                 | otherwise = factorFrom2 (m+1) n
>    where (q,r) = n `divMod` m


Exercise 5:

> factor3 :: Integer -> (Integer, Integer)
> factor3 n = factorFrom3 2 n

> factorFrom3 :: Integer -> Integer -> (Integer, Integer)
> factorFrom3 m n | r == 0    = (m,q)
>		  | q < m     = (n,1)
>		  | m == 2    = factorFrom3 3 n
>                 | otherwise = factorFrom3 (m+2) n
>    where (q,r) = n `divMod` m

It should be about twice as efficient.


Exercise 6:

factor1 90000000019 took 0.22, 0.24, and 0.26 secs, 139 mb.
factor2 90000000019 took 0.23, 0.22, and 0.23 secs, 122 mb.
factor3 90000000019 took 0.12, 0.13, and 0.15 secs, 70 mb.


Exercise 7:

> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 2 n 4

> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | r == 0    = (m,q)
>		    | q < m     = (n,1)
>		    | m == 2    = factorFrom4 3 n s
>		    | m == 3    = factorFrom4 5 n s
>                   | otherwise = factorFrom4 (m+6-s) n (6-s)
>    where (q,r) = n `divMod` m

factor4 90000000019 took 0.11, 0.12, and 0.15 secs, 66 mb.


Exercise 8:

The issue with extending what we did is that we had to individually hardcode the cases where m == 2 and m == 3, and we wouldn't be able to manually code the cases where m == each prime number. We would also have to define s in a very complicated manner to account for all the prime numbers, which isn't possible since there are an infinite number of primes.


Exercise 9:

> factors2 :: Integer -> [Integer]
> factors2 n = factorsFrom2 2 n
>
> factorsFrom2 :: Integer -> Integer -> [Integer]
> factorsFrom2 m n | n == 1    = []
>                  | otherwise = p:factorsFrom2 p q
>    where (p,q) = if m `mod` 6 == 1 then factorFrom4 m n 2 else factorFrom4 m n 4

Map confirms that they are equal.


Exercise 10:

factors 10000001 took 0.44, 0.49, and 0.45 secs, 327 mb.
factors2 10000001 took 0.01, 0.01, and 0.01 secs, 456 kb.

factors 8616460799 took 0.05, 0.05, and 0.05 secs, 34 mb.
factors2 8616460799 took 0.07, 0.07, and 0.07 secs, 37 mb.

Interestingly, factors2 actually took longer. This is because the input number had 2 factors that were nearly the same, so factors checked up to 89681, which is approximately sqrt(8616460799). In comparison, factors2 also checked up to sqrt(8616460799), but it had to do additional calculations per loop, which lost it time.


Exercise 11:

If r < 0, then we need to increase the distance between p^2 and q^2. This can be done by increasing p or decreasing q, but since we already know that y >= q, we cannot decrease it. Thus, if r < 0, then we need to increase p by 1. Similarly, if r > 0, then we need to either decrease p or increase q, and we can only increase q by 1. This method is guaranteed to terminate for all odd n because it allows us to systematically search every possible value for p and q. There is guaranteed to be a solution because every odd number has at least 1 pair of factors, 1 and itself.

> isqrt :: Integer -> Integer
> isqrt = truncate . sqrt. fromInteger
> search :: Integer -> Integer -> Integer -> (Integer, Integer)
> search p q n | r == 0    = (p, q)
>	       | r < 0     = search (p+1) q n
>	       | otherwise = search p (q+1) n
>	where r = p*p - q*q - n
>
> fermat :: Integer -> (Integer, Integer)
> fermat n = (p+q,p-q)
>	where (p,q) = search (isqrt n) 0 n

search 1 0 8616460799 took 0.08, 0.08, and 0.08 secs, 43 mb.
fermat 8616460799 took 0.08, 0.08, and 0.08 secs, 43 mb.


Exercise 12:

Given that x = (u + v)/2, where n = u*v, and we want to find the largest 2 factors of n, then the minimum value of x is the square root of n, where n is a square number. I have modified fermat accordingly.


Exercise 13:

fermat 8616460799 took 0.01, 0.01, and 0.01 secs, 1.66 mb, returning (96079, 89681)
fermat 1963272347809 took 5.06, 5.05, and 5.25 secs, 3.28 gb, returning (8123471, 241679)


Exercise 14:

> search2 :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
> search2 p q n r | r == 0    = (p, q)
>	          | r < 0     = search2 (p+1) q n (r+2*p+1)
>	          | otherwise = search2 p (q+1) n (r-2*q-1)
>
> fermat2 :: Integer -> (Integer, Integer)
> fermat2 n = (p+q,p-q)
>	where (p,q) = search2 (isqrt n) 0 n ((isqrt n)^2-n)

fermat2 1963272347809 took 4.37, 4.42, and 4.40 secs, 3.12 gb, returning (8123471, 241679)


Exercise 15:

> maxSqr :: Integer -> Integer
> maxSqr n = sqrFrom 1 n

> sqrFrom :: Integer -> Integer -> Integer
> sqrFrom m n | m*m > n   = (m-1)
>             | otherwise = sqrFrom (m+1) n

maxSqr 100000000000 took 0.12, 0.11, and 0.12 secs, 71 mb, returning 316227


Exercise 16:

> split :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
> split (l, r, n) | n < m^2   = (l, m, n)
>	          | otherwise = (m, r, n)
>	where m = (l+r)`div`2


Exercise 17:

> isqrtBin :: Integer -> Integer
> isqrtBin n = a
>	where (a, b, c) = multiSplit(1, n, n)
>	
> multiSplit :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
> multiSplit (l, r, n) | r-l <= 1  = (l, r, n)
>	               | otherwise = multiSplit(a, b, c)
>	where (a, b, c) = split(l, r, n)

It should take approximately logBase 2 n steps.

isqrtBin 10^10^4 took 0.73, 0.80, and 0.74 secs, 480 mb, returning 10^5000


Exercise 18:

> findPow :: Integer -> Integer
> findPow n = a
>	where a = findPowFrom 1 n
> 
> findPowFrom :: Integer -> Integer -> Integer
> findPowFrom m n | m * 2 > n = m
>		  | otherwise = findPowFrom (m*2) n
>
> isqrtOpenBin :: Integer -> Integer
> isqrtOpenBin n = a
>	where (a, b, c) = multiSplit(1, findPow n, n)

It should take approximately 2 * logBase 2 n steps. This really isn't worth it.

isqrtBinOpen 10^10^4 took 0.75, 0.77, and 0.75 secs, 480 mb, returning 10^5000 (I assume 5000 0's were written out, I didn't count)


