# General linear congruential generator
# Apparently, linear congruential generators are widely used.
# They're fast and easy to code up.

a = 17
c = 0 # setting c != 0 doesn't improve generality, so c is generally 0
x = 5
m = 9

for (i in 1:100){
  x = ( (a*x + c) %% m ) / m
  print(as.integer(x*10))
}

