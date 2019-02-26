# calc pi
# source https://stackoverflow.com/questions/28284996/python-pi-calculation

from decimal import Decimal, getcontext
getcontext().prec=10000
print(sum(1/Decimal(16)**k * (Decimal(4)/(8*k+1) - Decimal(2)/(8*k+4) - Decimal(1)/(8*k+5) - Decimal(1)/(8*k+6)) for k in range(10000)))

