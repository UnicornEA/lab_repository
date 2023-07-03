



pOfTwo 0 = [1]
pOfTwo n = (2^n) : pOfTwo (n - 1) 

powersOfTwo n = reverse (pOfTwo n)




