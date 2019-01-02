####################
######################
# rnorm with borders
#############################
# output is vector of numbers
#################
####################
#######
# n number of sample
# mn mean of sample
# s sd of samople
# lower limit of generation
# upper limit of generation
#
my_sample <- function(n, mn, s, lower, upper) {
  sample <- rnorm(n, mn, s) 
  sample[sample < lower] <- lower #makes everithing that less than limit equal to lower limit
  sample[sample > upper] <- upper #makes everithing that more than limit equal to upper limit
  sample # return sample
}




##################################
##### Power of test
##################################
##### Output is number between 1 and 0
#################################
# alpha significance of test
# eff effect size, according to Cohen 0.1 is small effect
#                                     0.25 is medium effect
#                                     0.5 is strong effect
#n : sample size
#k  number of groups of the model 
#l :number of parameters of the submodel
###############################

power <- function(alpha, eff, n, k, l) {
  df1 <- k - l
  df2 <- n - k
  c <- qf(1 - alpha, df1, df2)
  lambda <- eff^2 * n
  pow <- pf(c, df1, df2, ncp = lambda, lower.tail = FALSE)
  return(pow)
}
