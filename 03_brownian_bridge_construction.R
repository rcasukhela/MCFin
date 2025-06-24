# Goal: Write a Brownian Bridge Construction.
set.seed(1)
num_indices = 5

time_indices = sort(sample(1:10000, num_indices, replace = F))
time_indices = c(0, time_indices)

W = rep(0, num_indices+1)

stopifnot(length(W) == length(time_indices))
t_n = length(W)

W[t_n] = rnorm(1, 0, sd=sqrt(time_indices[t_n]))

for (i in time_indices){
  # t_0 case.
  if (i == 0) {
    W[i] = rnorm(1, 0, sd=sqrt(0))
  }
  
  # 
}
