coin_sides <- c("head", "tail")
n_flips <- 10
# Sample from coin_sides n_flips times with replacement
sample(coin_sides,n_flips, replace = T)

# Update the function to return n coin tosses
toss_coin <- function(n_flips) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, n_flips, replace = T)
}
toss_coin(10)

coin_sides <- c("head", "tail")
n_flips <- 10
p_head <- 0.8

# Define a vector of weights
weights <- 0.2

# Update so that heads are sampled with prob p_head
sample(coin_sides, n_flips, replace = TRUE, prob=c(p_head, weights))

toss_coin_prob <- function(n_flips){
  coin_sides <- c("head", "tail")
  n_flips <- 10
  p_head <- 0.8
  
  # Define a vector of weights
  weights <- 0.2
  
  # Update so that heads are sampled with prob p_head
  sample(coin_sides, n_flips, replace = TRUE, prob=c(p_head, weights))
}
toss_coin_prob(10)


# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1 - p_head)
  # Modify the sampling to be weighted 
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(10, p_head = 0.8)

# writing custom glm model
# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}


# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_explanatory %>%
  mutate(predicted_n_visits = predict(model, ., type = "response"))%>%
  arrange(desc(predicted_n_visits))


# Set the default for na.rm to FALSE
cut_by_quantile <- function(x, n = 5, na.rm=FALSE, labels, interval_type) {
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the na.rm argument from the call
cut_by_quantile(
  n_visits, 
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]"
)

library(devtools)
install_bitbucket("richierocks/assertive")
library(assertive)
library(assertive.code)
library(assertive.base)

calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  # Check if any values of x are non-positive
  if(any(is_non_positive(x), na.rm = TRUE)) {
    # Throw an error
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it negative numbers
calc_harmonic_mean(mtcars$mpg - 20)


# Update the function definition to fix the na.rm argument
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  if(any(is_non_positive(x), na.rm = TRUE)) {
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  # Use the first value of na.rm, and coerce to logical
  na.rm <- coerce_to(use_first(na.rm), target_class = "logical")
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it malformed na.rm
calc_harmonic_mean(mtcars$mpg, na.rm = 1:5)



is_leap_year <- function(year,n) {
  # If year is div. by 400 return TRUE
  if(is_divisible_by(year,400)) {
    return(TRUE)
  }
  # If year is div. by 100 return FALSE
  if(is_divisible_by(year,100) ) {
    return(FALSE)
  }  
  # If year is div. by 4 return TRUE
  if(is_divisible_by(year,4))
    return(TRUE)
  
  # Otherwise return FALSE
  
  return(FALSE)
}

is_leap_year(year = 2004)



