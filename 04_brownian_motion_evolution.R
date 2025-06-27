# User-defined parameters
# set.seed(1)
num_indices = 5000
mu = 0
sigma = 2

# X ~ BM(mu, sigma^2) with constant mu and sigma and given X(0)
X = rep(NA, num_indices)
# Initial Value
X[1] = 0

for (i in 2:num_indices){
  X[i] = X[i-1] + mu*(i - (i-1)) + sigma*sqrt(i - (i-1))*rnorm(1)
}

plot(X, pch=5, cex=0.2)
lines(X)

# Covariance Matrix of Brownian Motion vector
# C = matrix(data = NA, nrow=num_indices, ncol=num_indices, dimnames = list(c(1:num_indices), c(1:num_indices)))
# for (i in 1:num_indices){
#   for (j in 1:num_indices){
#     C[i, j] = sigma^2*min(i, j)
#   }
# }
# Mean Vector of Brownian Motion Vector
mean_vector = mu * 1:num_indices

# Cholesky Decomposition of Covariance Matrix
# A = chol(C)
# A
# t(A)%*%A # = C
