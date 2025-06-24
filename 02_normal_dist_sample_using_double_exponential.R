# Quite a few simplifications have been made to the functional forms of
# the checks required to run this simulation. I have them in my notes proper.

target_samples = c()
j = 0
n_sim = 1000

for (i in 1:1000) {
  # Initial Conditions
  U = runif(3)
  proposal_sample = -1 * log(U[1])
  
  # Proposal
  while ( U[2] > exp(-0.5 * (proposal_sample - 1) ^ 2) ) {
    U = runif(3)
    proposal_sample = -1 * log(U[1])
    j = j + 1
  }
  
  # Randomize the sign to generate the negative part of the distribution.
  if (U[3] < 0.5) {
    proposal_sample = -1 * proposal_sample
  }
  target_samples = c(target_samples, proposal_sample)
}

# Plot the histogram
hist(target_samples, freq = FALSE, col = "lightblue", border = "black", 
     main = "Double Exponential Proposed & Accepted Samples",
     xlab = "Value", ylab = "Density")

# Add the normal curve
curve(dnorm(x, mean = 0, sd = 1), 
      from = -5, to = 5, 
      col = "red", add = TRUE)

# Print simulation efficiency
cat("% Rejected: ", j/i * 100, "  % Accepted: ", (1 - j/i) * 100)
# % Rejected:  28.8   % Accepted:  71.2