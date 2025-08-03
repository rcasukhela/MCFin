# set.seed(1)
"
Goal: Write a multi-dimensional Brownian Bridge Construction.
"




bmDraw = function(s, drift, diffusion){
  x = rnorm(1, 0, sqrt(s))
  return (x)
}

bbInt = function(drift, diffusion, s_i, s_j, x_i, x_j, s){
  x = rnorm(
    1,
    (x_i + ( (s - s_i) / (s_j - s_i) ) * (x_j - x_i)),
    sqrt(((s_j - s) * (s - s_i)) / (s_j - s_i))
  )
  return (x)
}


"
Step 1:

User-defined variables.

We first define n, which is used to determine the maximum number of time
indices that we will generate for each process.

Next, we define the dimension of the problem, d.

Lastly, we will define num_indices, which will be useful later.
"
n = 100
d = 5
num_indices = 5


"
Here we determine the problem dimension.

While the user specifies d dimensions, I allow for the case where certain
B.M.s are exactly correlated and thus the full covariance matrix might not
be of full rank. So B becomes a d x k matrix.
"
k = sample.int(d, 1)
problem_dim = min(d, k)

"
I have some code here outside of functions to define the mean and covariance
matrices that we will need.

Something to keep in mind is that there is a global covariance matrix between
the different Brownian Motions and each Brownian Motion has a local covariance
matrix. I'm still wrapping my head around this.
"
mu = matrix(runif(problem_dim, -1, 1), ncol= 1)

"
This generates a d-dimensional integer-valued random matrix
that is SYMMETRIC POSITIVE-DEFINITE!!! Remember, this needs to be
such a matrix because we need a covariance matrix.
"
B_T = matrix(round(rnorm(n=d*k, 0, 10), 0), nrow=problem_dim, ncol=k)
B = t(B_T)
Sigma = B %*% t(B) + diag(problem_dim)*1e-6

# This is how to check that the matrix is symm pos-def.
sum(eigen(Sigma)$values > 0) == problem_dim


"
Now, we begin the steps to simulate the Brownian Bridge.

"
# Define a matrix that collects the multiple BMs.
X = matrix(
  data = NA,
  nrow=problem_dim,
  ncol=n+1, dimnames = list(c(1:problem_dim), c(1:(n+1)))
)

X[ , 1] = rep(0, problem_dim)






for (ind_bm in 1:(dim(X)[1])){
  drift = 0
  diffusion = 1
  
  s_i = 0
  s_j = n
  
  W = rep(NA, s_j - s_i + 1)
  W[s_i +1] = bmDraw(s_i, drift, diffusion)
  x_i = W[s_i +1]
  W[s_j +1] = bmDraw(s_j, drift, diffusion)
  x_j = W[s_j +1]
  
  for (s in (s_i +1) : (s_j -1)) {
    W[s +1] = bbInt(
      drift, diffusion,
      s_i, s_j, W[s_i +1], W[s_j +1], s
    )
  }
  
  X[ind_bm, ] = W
}

t = 0:n

# Transform the BM vector W to include drift and diffusion
X = X + mu %*% c(t) + B %*% X

matplot(t(X), type = "l", xlab = "Time", ylab = "Value")
