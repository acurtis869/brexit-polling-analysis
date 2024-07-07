# Analysis of Brexit Polling Data

Created as part of a university module on computing in statistics, this repository contains R code to analyze Brexit polling data, focusing on comparing proportions between "Remain" and "Leave" groups using parametric and non-parametric tests.

## Research Question
Are the means for "Remain" and "Leave" groups significantly different?

## Data Exploration and Transformation
The dataset is transformed to facilitate a paired t-test and a Wilcoxon signed-rank test.

### Steps
1. **Data Preparation**: Transforming raw polling data into a format suitable for statistical tests.
2. **Parametric Test**: Performing a paired t-test to compare means between groups.
3. **Non-parametric Test**: Conducting a Wilcoxon signed-rank test as an alternative non-parametric approach.
4. **Visualisation**: Visualising observed data using boxplots.

## Simulation and Analysis

### Sampling Function
A function (`brexit_sampler`) is defined to generate simulated polling data based on specified probabilities and sample sizes.

### Simulation Function
Another function (`test_calc`) runs simulations to evaluate the statistical power or size of tests under different scenarios.

### Scenarios Explored
1. **Impact of Sample Size on Test Size**: Analysing how the size of tests (Type I error rate) changes with sample size.
2. **Impact of Sample Size on Test Power**: Examining how statistical power changes with sample size.
3. **Effect of Effect Size on Test Power**: Investigating how changes in effect size (difference between "Remain" and "Leave" proportions) affect test power.

### Outputs
The simulations produce scatter plots illustrating the relationship between sample size, effect size, and test characteristics (size or power).