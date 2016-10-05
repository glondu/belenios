import hashlib

######################################################################
### Generate p and q according to FIPS 186-4, section A.1.1.2

L = 2048
N = 256
seedlen = 358 # not used
outlen = 256
seed = "Belenios: Verifiable online voting system "

def stringToInt(string):
    seq = [ ord(x) for x in string]
    res = ZZ(seq, 256)
    return res

def intToString(number):
    seq = number.digits(base=256)
    seq = [ chr(x) for x in seq ]
    return reduce(lambda a, b: a + b, seq)

# hash integer to an integer with SHA256
def Hash(number):
    string = intToString(number)
    return int(hashlib.sha256(string).hexdigest(), 16)

assert seedlen >= N
n = (L/outlen).n().ceiling() - 1
b = L-1-(n*outlen)
count = 0
found = False
while True:
    domain_parameter_seed = stringToInt(seed + str(count))
    U = Hash(domain_parameter_seed) % 2^(N-1)
    q = 2^(N-1) + U + 1 - (U % 2)
    count += 1
    if q.is_prime():
        break
offset = 1
counter = 0
while counter < 4*L:
    V = [ Hash(domain_parameter_seed + offset + j) for j in range(0,n+1) ]
    W = V[0]
    for j in range(1, n):
        W += V[j]*2^(outlen*j)
    W += (V[n] % 2^b)*2^(n*outlen)
    X = W + 2^(L-1)
    c = X % (2*q)
    p = X - (c-1)
    if p > 2^(L-1) and p.is_pseudoprime():
        found = True
        break
    offset = offset + n + 1
    counter += 1

if found:
    print "p = " + str(p)
    print "q = " + str(q)
    print "domain_parameter_seed = " + str(domain_parameter_seed)
    print "counter = " + str(counter)
else:
    print "Not found"

######################################################################
### Generate g according to section A.2.3

index = 0
N = q.nbits()
e = (p-1)//q
count = 1
U = count + 2^16*(index + 2^16*(0x6767656E + 2^32*domain_parameter_seed))
W = Hash(U)
g = int(GF(p, proof=False)(W)^e)
assert g >= 2
print "g = " + str(g)

print "Checking primality of p (not only pseudo-primality), this will take some time..."
assert p.is_prime()
