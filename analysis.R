### R CODE

# Research Question:
# Are the means for leave and remain the same or is there a significant 
# difference?

# Data Exploration/transformation:
# First transform dataset so a t-test can be performed on it:
library(dslabs)
remain <- data.frame("Prop" = brexit_polls$remain, "Group" = "Remain")
leave <- data.frame("Prop" = brexit_polls$leave, "Group" = "Leave")
brexit <- rbind(remain, leave)

# Parametric
t.test(Prop ~ Group, data = brexit, paired = TRUE)

# Non-parametric
wilcox.test(Prop ~ Group, data = brexit, paired = TRUE)

# Visualise observed data using a plot
library(tidyverse)
p1 <- ggplot(brexit) + geom_boxplot(aes(x = Group, y = Prop)) + 
  ylab("Proportion") + ggtitle("Observed Data")

# Create sampling function:

## Function which creates a sample of polling data with required characteristics
# INPUTS:
#   p_remain: probability of remain
#   p_leave: probability of leave
#   n_polls: number of polls in the sample
#   poll_size: number of observations included in each poll
# PROCESS:
#   Uses R's multinomial sampling function to create a sample, which is divided
#   by the poll size to have it as a proportion. Data is then transformed into
#   the correct format
# OUTPUT:
#   sample: a polling sample with the desired characteristics

brexit_sampler <- function(p_remain, p_leave, n_polls, poll_size = 100) {
  # Undecided column is calculated by what is leftover
  p_other <- 1 - p_remain - p_leave
  # Generate intial sample using built in function
  sample_raw <- rmultinom(n_polls, poll_size, 
                          c(p_remain, p_leave, p_other)) / poll_size
  # Transform data to desired format
  sample_remain <- data.frame("Prop" = sample_raw[1, ], Group = "Remain")
  sample_leave <- data.frame("Prop" = sample_raw[2, ], Group = "Leave")
  sample <- rbind(sample_remain, sample_leave)
  return(sample)
}


# Compare to simulated data
metadata <- brexit %>% 
  group_by(Group) %>%
  summarise(count = n(), mean = mean(Prop), sd = sd(Prop))
metadata
set.seed(1999)
brexit_sample <- brexit_sampler(metadata$mean[2], 
                                metadata$mean[1],
                                metadata$count[1])
p2 <- ggplot(brexit_sample) + geom_boxplot(aes(x = Group, y = Prop)) +
  ylab("Proportion") + ggtitle("Simulated Data")

# Plot the two plots side-by-side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2, 
             top = "Boxplots of Voting Proportions for Each Group")

# Create simulation function:

## Function which runs the simulation and calculates size/power
# INPUTS:
#   alpha: significance level of the tests. Default is 0.05
#   iter: number of iterations of the sampling and testing
#   p_remain: probability of remain
#   p_leave: probability of leave
#   n_polls: number of polls in the sample
#   poll_size: number of observations included in each poll
# PROCESS:
#   Creates a sample using sampling function above, runs the tests using this
#   sample and concludes whether h_0 was rejected or not by using the p-value.
#   Size/power are then calculated by how many times h_0 was rejected
# OUTPUT: List containing the following:
#   Type_of_test: either "size" or "power" depending on which was being tested
#   Effect_Size: difference between mean of remain and leave
#   Parametric: the estimated size/power for the parametric test
#   Nonparametric: the estimated size/power for the non-parametric test

test_calc <- function(alpha = 0.05, iter = 1000, p_remain, p_leave, 
                      sample_size = 100, poll_size = 100) {
  # Initialise values
  h1Sum_param <- 0
  h1Sum_nonparam <- 0
  type <- "Power"
  # Check whether testing for size or power
  if (p_remain == p_leave) {
    type <- "Size"
  }
  # Begin iterating
  for (i in 1:iter) {
    # Generate sample
    sample <- brexit_sampler(p_remain, p_leave, sample_size, poll_size)
    # Run tests and save p-values
    test_param <- t.test(Prop ~ Group, data = sample, paired = TRUE)
    pValue_param <- test_param$p.value
    test_nonparam <- wilcox.test(Prop ~ Group, data = sample, paired = TRUE)
    pValue_nonparam <- test_nonparam$p.value
    # Record whether h_0 was rejected or not on this iteration
    if (pValue_param < alpha) {
      h1Sum_param <- h1Sum_param + 1
    }
    if (pValue_nonparam < alpha) {
      h1Sum_nonparam <- h1Sum_nonparam + 1
    }
  }
  # Calculate estimate for size/power
  param <- h1Sum_param / iter
  nonparam <- h1Sum_nonparam / iter
  return(list("Type_of_test" = type,
              "Effect_Size" = (p_remain - p_leave),
              "Parametric" = param, 
              "Nonparametric" = nonparam))
}

# Run different scenarios:

## SCENARIO 1 - How does size change as sample becomes larger?

# Create range of values for sample size
sample_size <- seq(from = 10, to = 300, by = 10)
# Create empty vectors to store values
size_param <- rep(NA, length(sample_size))
size_nonparam <- rep(NA, length(sample_size))
# Set seed for reproducibility
set.seed(1999)
# Run simuation code for range of values and store outputs
for (i in 1:length(sample_size)) {
  test <- test_calc(p_remain = 0.45, p_leave = 0.45, 
                    sample_size = sample_size[i])
  size_param[i] <- test$Parametric
  size_nonparam[i] <- test$Nonparametric
}
# Create plots of outputs
p1 <- ggplot() + geom_point(aes(x = sample_size, y = size_param)) +
  geom_hline(yintercept = 0.05, linetype="dashed", color = "red") +
  xlab("Sample Size") + ylab("Size") + 
  ggtitle("Parametric (Paired t-test)")
p2 <- ggplot() + geom_point(aes(x = sample_size, y = size_nonparam)) + 
  geom_hline(yintercept = 0.05, linetype="dashed", color = "red") +
  xlab("Sample Size") + ylab("Size") + 
  ggtitle("Non-parametric (Wilcoxon Signed-Rank Test)")
# Arrange plots side-by side
grid.arrange(p1, p2, ncol = 2, 
             top = "Scatter Plots of How Size of Test is Affected by Sample Size")


## SCENARIO 2 - How does power change as samples become larger?
# Create range of values for sample size
sample_size <- seq(from = 10, to = 300, by = 10)
# Create empty vectors to store values
power_param <- rep(NA, length(sample_size))
power_nonparam <- rep(NA, length(sample_size))
# Set seed for reproducibility
set.seed(1999)
# Run simuation code for range of values and store outputs
for (i in 1:length(sample_size)) {
  test <- test_calc(p_remain = 0.442, p_leave = 0.422, 
                    sample_size = sample_size[i])
  power_param[i] <- test$Parametric
  power_nonparam[i] <- test$Nonparametric
}
# Create plots of outputs
p1 <- ggplot() + geom_point(aes(x = sample_size, y = power_param)) +
  xlab("Sample Size") + ylab("Power") + 
  ggtitle("Parametric (Paired t-test)")
p2 <- ggplot() + geom_point(aes(x = sample_size, y = power_nonparam)) +
  xlab("Sample Size") + ylab("Power") + 
  ggtitle("Non-parametric (Wilcoxon Signed-Rank Test)")
# Arrange plots side-by side
grid.arrange(p1, p2, ncol = 2, 
             top = "Scatter Plots of How Power of Test is Affected by Sample Size")


## SCENARIO 3 - How does power change as effect size changes?

# Create range of values for p_remain and p_leave (to vary effect size)
p_remain <- seq(from = 0.425, to = 0.475, by = 0.0025)
p_leave <- seq(from = 0.475, to = 0.425, by = -0.0025)
# Create empty vectors to store values
power_param <- rep(NA, length(p_remain))
power_nonparam <- rep(NA, length(p_remain))
effect_size <- rep(NA, length(p_remain))
# Set seed for reproducibility
set.seed(1999)
# Run simuation code for range of values and store outputs
for (i in 1:length(p_remain)) {
  test <- test_calc(p_remain = p_remain[i], p_leave = p_leave[i])
  power_param[i] <- test$Parametric
  power_nonparam[i] <- test$Nonparametric
  effect_size[i] <- test$Effect_Size
}
# Create plots of outputs
p1 <- ggplot() + geom_point(aes(x = effect_size, y = power_param)) +
  xlab("Effect Size") + ylab("Power") + 
  ggtitle("Parametric (Paired t-test)")
p2 <- ggplot() + geom_point(aes(x = effect_size, y = power_nonparam)) +
  xlab("Effect Size") + ylab("Power") + 
  ggtitle("Non-parametric (Wilcoxon Signed-Rank Test)")
# Arrange plots side-by side
grid.arrange(p1, p2, ncol = 2, 
             top = "Scatter Plots of How Power of Test is Affected by Effect Size")



