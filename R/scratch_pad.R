##############
# cacluate e #
##############

sum(1 / factorial(0:100))
1 + sum(1 / factorial(1:100))


###################################
## Combinations and permutations ##
###################################

# see https://www.mathsisfun.com/combinatorics/combinations-permutations.html
# A permutation is an ordered combination

# where n is the number of things to choose from,
# and we choose r of them,
# repetition is allowed,
# order matters
combos_replace <- function(n, r) {
  n ^ r
} 

# where n is the number of things to choose from,
# and we choose r of them,
# no repetition,
# order matters
combos_no_replace <- function(n, r) {
  factorial(n) / factorial(n - r)
} 

# where n is the number of things to choose from,
# and we choose r of them,
# no repetition,
# order doesn't matter.
perms_no_replace <- function(n, r) {
  choose(n, r)
  # choose is also known as the Binomial Coefficient
} 

# where n is the number of things to choose from,
# and we choose r of them,
# repetition is allowed,
# order doesn't matter.
perms_replace <- function(n, r) {
  choose(r + n - 1, r)
} 

perms_replace(5, 3)


#######
# MLE #
#######

# likelihood function
# effectively: p ^ n * (1 - p) ^ (sum(1:(n - 1)))
# n = number of trials
# ... = used to pass probability of success (p) to geom_pmf()
l <- function(n, ...) {
  prod(geom_pmf(1:n, ...))
}

l(4, p = length(target_values) / n)


# maximum likelihood estimate
# p_hat
# n = number of trials
# X = results of trials
# see https://www.projectrhea.org/rhea/index.php/MLE_Examples:_Exponential_and_Geometric_Distributions_OldKiwi
p_hat <- function(X) {
  length(X) / sum(X)
}

# p_hat is basically 1/mean
p_hat(tries)

p <- .3
geom_exp <- 1 / p
geom_var <- (1 - p) / p ^ 2  

