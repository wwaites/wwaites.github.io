from cmath import *

R = 50.0
L = 0.5e-3
G = 3.0e-6
C = 50.0e-9

def gamma(omega, d):
    return sqrt(complex(R*d, omega * L*d)*complex(G*d, omega*C*d))

def norm(c):
    return sqrt(c.conjugate()*c)

for i in range(100):
    f = 1000.0 * 10**(0.031 * i)
    print f/1000, 8.7*norm(gamma(f, 15)).real
