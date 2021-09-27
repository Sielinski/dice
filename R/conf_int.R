# test frequentist confidence interval

# see if the expected value is between the mean ± the conf interval
# for a range of sample sizes (n_start to n_stop) 
n_start <- 25
n_stop <- 500

# a vector to hold the results
clt_test <- rep(NA, n_stop - n_start)

# our known expected value
expected_value <- 14.7

for (i in n_start:n_stop) {
  #take a sample from our results
  i_sample <- sample(results, i)
  
  # calculate the statitics
  i_mean <- mean(i_sample)
  i_std_err <- sd(i_sample) / sqrt(i)
  t_value <- qt(0.975, df = i - 1)
  upper_limit <- i_mean + t_value * i_std_err
  lower_limit <- i_mean - t_value * i_std_err
  
  # see if the expected value is between the upper and lower limits
  clt_test[i - n_start] <- lower_limit <= expected_value & expected_value <= upper_limit 
}

# what percentage of the time was the expected value between the limits?
sum(clt_test) / length(clt_test)
