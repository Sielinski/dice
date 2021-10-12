# how many times do you need to roll a die before getting at least one of
# every side?

library(ggplot2)
library(tidyr)
library(dplyr)

# monte carlo simulation
# n = number of sides
# repetitions = number of times to repeat the simulation
# seed = random seed
simulation <- function(n = 6, repetitions = 1000, rnd_seed = 42) {
  # create a vector to hold simulation results
  results <- rep(NA, repetitions)
  
  # set a seed to ensure the results are repeatable
  set.seed(rnd_seed)
  
  # conduct the simulation
  for (i in 1:repetitions) {
    # create a vector to track how many times each side is rolled
    die <- rep(0, n)
    
    # roll the die and track number of times each side comes up
    # until all sides have come up
    while (any(die == 0)) {
      roll <- sample(n, 1)
      die[roll] <- die[roll] + 1
    }
    
    # record the result (i.e., the total number of rolls)
    results[i] <- sum(die)
  }
  
  return(results)
}

n <- 6
results <- simulation(n, repetitions = 1000, rnd_seed = 42)

# the mean 
(continous_mean <- mean(results))

# std error of the mean 
# see 'conf_int.R'
#std_err <- sd(results) / sqrt(repititions)


# look at the distribution of results as a histogram 
hist(results, breaks = max(results) - n)
abline(v = continous_mean, col = 'red')

data.frame(rolls = results) |>
  group_by(rolls) |>
  summarize(cnt = n()) |>
  ggplot(aes(x = rolls, y = cnt)) +
  geom_col() + 
  geom_vline(xintercept = continous_mean, col = 'red')


# look at the quantiles (which are all discrete)
quantile(results, probs = c(0.3, 0.4, 0.5, 0.6))

discrete_mode <- table(results) |>
  which.max() |>
  names() |>
  as.numeric()

discrete_median <- quantile(results, probs = 0.5) |>
  as.numeric()

sum(results <= discrete_mode) / length(results)
sum(results <= discrete_median) / length(results)

max(results)
min(results)

boxplot(results)


##########################
# geometric distribution #
##########################

# how many times do you have to roll a die before you get one of the 
# remaining faces (i.e., the target_values)

target_values <- c(1, 2, 4, 5)

# monte carlo
repititions <- 5000
tries <- rep(NA, repititions)

for (i in 1:repititions) {
  cnt <- 0 
  while(!(sample(1:n, 1) %in% target_values)) {
    cnt <- cnt + 1
  }
  tries[i] <- cnt + 1
}

# plot the table and density chart
# note that the mode of any geometric distribution is 1
# the number of times that you need to roll the die is always 1 
plot(table(tries) / repititions)

mean(tries)
#table(tries)
(table(tries) / length(tries))[1:6] 
(table(tries) / length(tries))[1:6] |> sum()


# probability for a geometric distribution
# i.e., probability of success on the n-th attempt
# n = number of trials
# p = probability 
geom_prob <- function(n, p = 1/6) {
  p * (1 - p) ^ (n - 1)
}

# compare the results of the formula to monte carlo simulation
geom_prob(1:6, p = length(target_values) / 6)
table(tries)[1:6] / repititions


##########################################
# pmf for each number of remaining faces # 
##########################################

pip_cnt <- n
max_x <- 10  # x-axis of density chart
# 10 is the most number of rolls that we can make for any 1 face

# matrix of results
pmf <- matrix(rep(0, pip_cnt * max_x), nrow = pip_cnt)

# prob for each number of remaining faces
probs <- (pip_cnt:1) / pip_cnt

# calculate the pmf for the number of rolls for each number of 
# remaining faces
for (i in 1:pip_cnt) {
  pmf[i, ] <- geom_prob(1:max_x, probs[i])
}

# look at all PMFs
pmf_df <- as.data.frame(pmf) |>
  mutate(face_cnt = row_number()) |>
  pivot_longer(cols = !face_cnt,
               names_to = 'roll_cnt',
               values_to = 'probability') |>
  mutate(roll_cnt = as.numeric(substring(roll_cnt, 2))) 

ggplot(pmf_df, aes(x = roll_cnt, y = probability, fill = as.factor(face_cnt))) +
  geom_col() + 
  facet_grid(. ~ face_cnt)


#######################################
# Brute-force calculation of the mode # 
#######################################

# this only works for n = 6
# a more generalized solution is required for other values

# Our mean is 14.7, and since the mode will be less than the median, the mode 
# will occur at < 15 total rolls. Given 15 total rolls and 6 faces, the 
# we most times that we can roll to find any one pip is 10 
# (e.g., 1, 1, 1, 1, 1, 10), so we can shift 1:10 to 0:9 and use regular 
# base-10 numbers as our indices to permutations. Using the same example, 
# the shifted pip counts would be 0, 0, 0, 0, 0, 9, and the max value is 
# likewise shifted by 6 (i.e., once for each pip). 

# Finally, since the first pip is always found on the first roll, we only 
# need to look at numbers 5-digit numbers. 

# Find all permutations of a 5-digit base-10 numbers that add up to <= 9

# permutations between 6 and 15 (the ceiling of the mean)
digitsum <- function(x) sum(floor(x / 10 ^ (0:(nchar(x) - 1))) %% 10)
digitsum <- Vectorize(digitsum)

up_shift <- function(x){
  # same as digitsum function, but no sum and add 1 to up shift
  digits_mirror <- floor(x / 10 ^ (0:(nchar(x) - 1))) %% 10 + 1
  # create a results vector
  digits <- rep(1, 5)
  # reverse digit order and right-shift into results
  digits[(6 - nchar(x)):5] <- digits_mirror[nchar(x):1]
  # pad with a 1
  c(1, digits)
}
up_shift <- Vectorize(up_shift)

joint_prob <- function(x) {
  # probability of needing exactly x[i] rolls to get the i-th new face. E.g., 
  # needing exactly 2 rolls to get the 4th new face (after getting 3 faces)
  prob_hit <- rep(NA, 6)
  
  for (i in 1:6) {
    prob_hit[i] <- pmf[i, x[i]]
  }
  
  # conditional probability
  prod(prob_hit)
} 

target_permuations <- c(0, which(digitsum(1:99999) <= 9))

target_shifted <- up_shift(target_permuations)

dat <- data.frame(rolls = apply(target_shifted, 2, sum), 
           prob = apply(target_shifted, 2, joint_prob)
)

# we cover ~2/3 of the total probability  
sum(dat$prob)

# summarize the conditional probabilities for all possibilities
by(dat, dat$rolls, function(x) sum(x$prob)) #|> as.numeric()

dat |>
  group_by(rolls) |>
  summarize(sum_prob = sum(prob))

# compare the calculated probabilities to the monte carlo results
(table(results) / sum(table(results)))[1:10]

# getting a new face on the first roll (i.e., 6 total rolls) is the most 
# likely permutation 
dat[which.max(dat$prob), ]

# however, there's only 1 way to get a new face on every roll, and 5 ways 
# of having to roll twice for at least one new face (i.e., 7 total rolls)
dat |>
  filter(rolls == 6 | rolls == 7)

# and the sum of probabilities for 7 rolls is higher 
dat |>
  filter(rolls == 6 | rolls == 7) |>
  group_by(rolls) |>
  summarize(sum_prob = sum(prob))

# sum of probabilities for rolls ≤ mode
rolls_sum_prob <- dat |>
  group_by(rolls) |>
  summarize(sum_prob = sum(prob)) 

rolls_mode <- rolls_sum_prob[which.max(rolls_sum_prob$sum_prob), ]$rolls

rolls_sum_prob |>
  filter(rolls <= rolls_mode) |>
  summarize(sum_prob = sum(sum_prob))

dat |>
  group_by(rolls) |>
  summarize(count = n())


#######################
## Formula/Algorithm ##
#######################

# from https://math.stackexchange.com/questions/28905/expected-time-to-roll-all-1-through-6-on-a-die
mean_prob <- function(n) {
  n * sum(1 / (1:n))
}

# a rough approximation for the mode (for large n)
# from http://archives.math.utk.edu/ICTCM/VOL27/A016/paper.pdf
# p 137
mean_approx <- function(n) {
  n * log(n)
}

# p 141
mean_approx_refined <- function(n) {
  n * log(n) + (-digamma(1)) * n
}

# for large n
# p 145
mode_approx <- function(n) {
  n * log(n) + (n * log(log(n)))
}

n <- 50

mean_prob(n)
mean_approx(n)
mean_approx_refined(n)
mean(results)
mode_approx(n)

# probability of d different unique results, completed on the k-th attempt
# where
# d = number of different results (e.g., coupons, faces, etc.)
# k = number of attempts (e.g., draws, rolls, etc.)
# n = size of complete set
permutations_d <- function(i, d, k) {
  (-1) ^ (i + 1) * choose(d, i) * i * (d - i) ^ (k - 1)
}

prob_d_k_exact <- function(d, k, n) {
  if (d > k | d > n)
    return(NA)
  permutations_d_sum <- permutations_d(1:(d - 1), d, k) |>
    sum()
  choose(n, d) / (n ^ k) * permutations_d_sum
}

prob_d_k_exact <- Vectorize(prob_d_k_exact)

prob_d_k_sum <- function(d, k, n){
  sum(prob_d_k_exact(d, d:k, n))
}

prob_d_k_sum <- Vectorize(prob_d_k_sum)


# test that the formula works 
d <- 4
k <- 16
n <- 6

prob_d_k_exact(d, k, n) 
prob_d_k_exact(6, 6, 6)

prob_d_k_exact(d, d:k, n) |> round(6)
prob_d_k_exact(d, d:k, n) |> plot()
prob_d_k_sum(d, d:k, n) |> plot()

sample(1:n, 9, replace = T) |> 
  unique() |> 
  length()

d <- 6
k <- 12
n <- 6

prob_d_k_exact(d, d:k, n)
prob_d_k_sum(d, k, n)

#rolls_sum_prob$sum_prob[which(rolls_sum_prob$rolls <= k)] |> sum()

prob_d_k_sum(d, k, n) - prob_d_k_sum(d, k - 1, n)
prob_d_k_exact(d, k, n)

n <- 44
prob_d_k_exact(n, floor(mean_approx(n)):ceiling(mean_prob(n)), n) #|> which.max()

permutations_d(1:n-1, n, discrete_mode)


######################################################
## how many permutations add up to specific number? ##
######################################################

# the number of permutations that add up to a fixed number rolls
# where 
# sum_all = sum of all digits (i.e., number of rolls)
# digits_cnt = number of remaining digits 

cnt_recursive <- function(sum_all, digits_cnt) {
  # on the last digit, so exit recursion 
  if (digits_cnt == 1) return(1)
  
  # the largest value in any position (x) is 
  # (sum of all remaining digits) - (count of remaining digits) + 1
  x_max <- sum_all - digits_cnt + 1
  
  # count of permutations (reset at each level of recursion)
  cnt <- 0
  
  for (i in 1:x_max)
    cnt <- cnt + cnt_recursive(sum_all - i, digits_cnt - 1)
  
  return(cnt)
}

cnt_recursive <- Vectorize(cnt_recursive)

n <- 6
k <- 7
# the number of rolls that it takes to get the first
# side is always 1 (i.e., there are no permutations), 
# so subtract 1 from both k and n
cnt_recursive(k - 1, n - 1) 

data.frame(k = c(6:14)) |>
  mutate(perms = cnt_recursive(k - 1, n - 1))

k <- 51
cnt_recursive(k - 1, n - 1) 
