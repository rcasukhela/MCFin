# User-defined parameters
# set.seed(1)
num_indices = 500
mu = 1
sigma = 200
VAR_THRESHOLD = 0.99

# Covariance Matrix of Brownian Motion vector
C = matrix(data = NA, nrow=num_indices, ncol=num_indices, dimnames = list(c(1:num_indices), c(1:num_indices)))
for (i in 1:num_indices){
  for (j in 1:num_indices){
    C[i, j] = sigma^2*min(i, j)
  }
}

# Mean Vector of Brownian Motion Vector
mean_vector = mu * 1:num_indices

# Cholesky Decomposition of Covariance Matrix
# Remember that you're actually getting A'!
A_T = chol(C)
A = t(A_T)
A%*%t(A) # = C

# Now, get eigen-values of C.
# This will be used to calculate the percentage of variability explained
# by the first k eigvenvalues.
eig_C = eigen(C)
n = length(eig_C$values)

# The explained variability will be sum(first_k_elem) / sum(all_elem) * 100.
exp_var = rep(NA, n)
for (i in 1:n){
  exp_var[i] =  sum(eig_C$values[1:i]) / sum(eig_C$values)
}
rm(n)

plot(exp_var, pch=5, cex=0.7)
lines(exp_var)

# Given a user-specified threshold (between 0 and 1),
# we can select the number of first k eig-vals
# that we want to use to express the Brownian Motion.
k = which(exp_var >= VAR_THRESHOLD)[1]


"
Principal Components Construction.

To do this, of the first k columns of A, multiply
each ith column by a standard normal r.v.,
and sum.
"
tmp = rep(0, num_indices)
for (i in 1:k){
  tmp = tmp + A[ , i]*rnorm(1)
}
W = tmp
remove(tmp)

plot(W, pch=5, cex=0.7)
lines(W)

"
Findings:

So, you definitely can reduce the number of computations
you do with this method.

However, the problem with this is that if the
explained variability is not very high (like higher than 0.999),
the B.M. will completely flatten out after a certain time index to the end.

So how useful is this really?
"





