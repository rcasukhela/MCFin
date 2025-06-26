# set.seed(1)

drift = 10
diffusion = 10000

s_i = 0
s_j = 1000

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

time_indices = seq_along(W) - 1 # This starts the time series at 0.

# Transform the BM vector W to include drift and diffusion
W = drift * time_indices + sqrt(diffusion) * W

plot(W)
lines(W)





