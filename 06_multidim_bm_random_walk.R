"
The goal is to generate a multi-dimensional Brownian Motion.

We will create a d-dimensional BM.
"
require(MASS) # import mvrnorm()
n = 1000 # number of entries
d = 3 # dimension of B.M.
mu = matrix(runif(d, -5, 5), ncol= 1)

# This generates a d-dimensional standard normal vector.
# Z = mvrnorm(n=1, rep(0, d), diag(d))



"
This generates a d-dimensional integer-valued random matrix
that is SYMMETRIC POSITIVE-DEFINITE!!! Remember, this needs to be
such a matrix because we need a covariance matrix.
"
nrows = sample.int(d, 1)
ncols = d
problem_dim = max(nrows, ncols)

B_T = matrix(round(rnorm(n=nrows*ncols, 0, 10), 0), nrow=nrows, ncol=ncols)
B = t(B_T)
Sigma = B %*% t(B) + diag(problem_dim)*1e-6

# This is how to check that the matrix is symm pos-def.
sum(eigen(Sigma)$values > 0) == d



"
Multi-dimensional Brownian Motion: Random Walk Construction.
"
X = matrix(
  data = NA,
  nrow=d,
  ncol=n, dimnames = list(c(1:d), c(1:n)))

X[ , 1] = rep(0, d)
for ( i in 1:(n-1) ){
  Z = matrix(mvrnorm(n=1, rep(0, dim(B)[2]), diag(dim(B)[2])), ncol=1)
  X[ , i+1] = X[ , i]  +  mu * (i+1 - i)  +  sqrt(i+1 - i) * B %*% Z
}
# Just putting the (i+1 - i) in for pedagogical purposes.

# Apparently, (i in 1:n-1) actually makes i start from 0 and go to n-1 !!
# Never knew that.

matplot(t(X), type = "l", xlab = "Time", ylab = "Value")




















