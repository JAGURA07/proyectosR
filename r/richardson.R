fx = function(x) x*x       # any real scalar valued function
x = 1                      # gradient at this point
h = max(1e-4, 1e-4*abs(x)) # small h
a = vector(length=4)       # for approximate gradients

# four central differences with consecutively smaller h
a[1] = (fx(x + h/2^1) - fx(x - h/2^1))/(2*h/2^1)
a[2] = (fx(x + h/2^2) - fx(x - h/2^2))/(2*h/2^2)
a[3] = (fx(x + h/2^3) - fx(x - h/2^3))/(2*h/2^3)
a[4] = (fx(x + h/2^4) - fx(x - h/2^4))/(2*h/2^4)

# first round refinement
a[1] = (a[2]*4^1 - a[1])/(4^1 - 1)
a[2] = (a[3]*4^1 - a[2])/(4^1 - 1)
a[3] = (a[4]*4^1 - a[3])/(4^1 - 1)

# second round refinement
a[1] = (a[2]*4^2 - a[1])/(4^2 - 1)
a[2] = (a[3]*4^2 - a[2])/(4^2 - 1)

# third (final) refinement
a[1] = (a[2]*4^3 - a[1])/(4^3 - 1)
a[1] # Answer
}