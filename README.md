# Probability-problem

## Problem 1
sum_over_1_unif = function(){
  sum_x = 0
  iter = 0
  while(sum_x < 1){
    x = runif(1)
    sum_x = sum_x + x
    iter = iter + 1
  }
  return(iter)
}
## The expectation of sum of Uniform r.v which exceed 1
## answer = exp(1)
