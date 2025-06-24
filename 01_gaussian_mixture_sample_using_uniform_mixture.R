require(quantmod)

# Task: Create a rejection sampling algorithm for a defined mixture of normals.

# Step 1: Create the Normal-mixture distribution.

### User-Defined
n_dists = 10
n_draws_mix = 100000
BIN_APPROX = 5000
M = 20
###

# Mixture Distribution: Weights
weights = sample(1:10000, n_dists, replace=T)
weights = weights / sum(weights) # Normalize the weights to yield a convex combination

# Mixture Distribution: Means (Variance of each dist is just set to 1)
means = sample(-10:10, n_dists, replace=T)

# Draw from the Mixture Distribution:
mix_draws = c()
mix_draw = 0

for (j in 1:n_draws_mix) {
  i = sample(1:n_dists, replace=T, prob=weights)
  mix_draw = rnorm(1, mean=means[i], sd=1)
  mix_draws = c(mix_draws, mix_draw)
  mix_draw = 0
}

freq = hist(mix_draws, breaks=BIN_APPROX, freq=F, include.lowest=T, plot=F)
plot(freq$mids, freq$density)

smoothed_mix = predict(loess(freq$counts ~ freq$mids, span=0.1))
smoothed_mix = cbind(freq$mids, smoothed_mix, colnames=c('x', 'smoothed y'))

plot(freq$counts[findPeaks(freq$counts, thresh=50)])



sim_data = matrix(
  nrow=length(2:length(freq$breaks)),
  ncol=4,
  dimnames = list(NULL, 
                  c("Left Break", "Right Break", "Target Density", "Proposal Density")
  ))
for (i in 2:length(freq$breaks)) {
  sim_data[i-1, 1] = freq$breaks[i-1]
  sim_data[i-1, 2] = freq$breaks[i]
  sim_data[i-1, 3] = freq$density[i]
  sim_data[i-1, 4] = freq$density[i]*M
}

sim_data = data.frame(sim_data)

plot(freq$mids, sim_data$Proposal.Density)
points(freq$mids, sim_data$Target.Density)

# Get density of number

target_draws = c()
for (i in 1:10000) {
  u = runif(1, 0, 1)
  y = runif(1, min=min(sim_data$Left.Break), max=max(sim_data$Right.Break))

  if (u > 1/M) {target_draws = c(target_draws, y)}
}
hist(target_draws)

# This is the only sample method that I've gotten to work. It uses the histogram as an approximation.
hist(sample(freq$mids, replace=T, prob=freq$density), breaks=500)

# Whatever I've done here is pretty general, actually. Histogram sampling can get
# arbitrarily close to the distribution and is really just sampling from a KDE
# using a Uniform mixture.
