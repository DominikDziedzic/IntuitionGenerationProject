# Required packages:
install.packages("insight")
install.packages("effectsize")
install.packages("rstanarm")
install.packages("easystats")
install.packages("dplyr")
install.packages("performance")

# Load the required packages:
library(insight)
library(effectsize)
library(rstanarm)
library(easystats)
library(dplyr)
library(performance)

# Import data:
# a) if in .txt:
data <- read.delim(file.choose())

# b) if in .csv:
data <- read.csv(file.choose(), sep = ";")

data$condition <- as.numeric(as.factor(data$condition))
data$condition[data$condition == 2] = 0 # code: "primed" = 0, "unprimed" = 1, "control" = 2
data$condition[data$condition == 1] = 2
data$condition[data$condition == 3] = 1

# Fit the model - frequentist framework:
summary(lm(final ~ condition + training, data = data))
model <- lm(final ~ condition + training, data = data)

insight::get_parameters(model)

effectsize::eta_squared(model)

# Compute beta coefficients
dataStand <- lapply(data, scale) # standardizes all variables
modelStand <- lm(final ~ condition + training, data = dataStand)
beta_coef <- coef(modelStand)
beta_coef

# The linear relationship between final responses (DV) and training (IV) is
# positive and significant (insignificant relationship between final responses
# and condition). Final reference judgments were significantly correlated  with
# training (b = 0.501, t(42) = 3.939, p < 0.001, eta^2 = 0.27, beta = 5.170).
# Condition: b = 0.119, t(42) = 1.592, p > 0.1, eta^2 = 0.03, beta = 2.090.
# Together, the two variables explain R^2 = 0.29% of the variability in final
# reference judgments, which is a large effect.

# Bayesian framework (the results may slightly vary - each time the model is generated anew)
model <- stan_glm(final ~ condition + training, data = data)
posteriorsDs <- describe_posterior(model)
# for a nicer table
print_md(posteriorsDs, digits = 2)
report(model)

# Extracting the posteriors
posteriors <- insight::get_parameters(model)
head(posteriors)

# Point-estimate (similar to b in frequentist regression)
median(posteriors$condition) # at 0.117, there is 50% chance that the true effect is higher and 50% chance that the effect is lower
median(posteriors$training) # at 0.497, there is 50% chance that the true effect is higher and 50% chance that the effect is lower

# Uncertainty
# Compute credible interval (similar to a frequentist confidence interval), use 89% CIs instead of 95%  CIs (as in frequentist framework), as 89% level gives more stable results
hdi(posteriors$condition, ci = 0.89) # the effect has 89% chance of falling within the [0.00, 0.24] range
hdi(posteriors$training, ci = 0.89) # the effect has 89% chance of falling within the [0.30, 0.71] range

# Effect significance
ropeVal <- 0.1 * sd(data$final) # define ROPE (Region of Practical Equivalence: https://easystats.github.io/bayestestR/articles/region_of_practical_equivalence.html) as the tenth (1/10 = 0.1) of the standard deviation (SD) of the response variable, which can be considered as a “negligible” effect size (Cohen, 1988)
ropeRange <- c(-ropeVal, ropeVal)
ropeRange

rope(posteriors$condition, range = ropeRange, ci = 0.89) # inside ROPE: 13.65%!
rope(posteriors$training, range = ropeRange, ci = 0.89)

# Probability of Direction (pd)
positivecondition <- posteriors %>%
  filter(condition > 0) %>% # select only positive values
  nrow() # Get length
positivecondition / nrow(posteriors) * 100 # the effect is positive with a probability of 93.8%, i.e., absence of true effect!

positivetraining <- posteriors %>%
  filter(training > 0) %>% # select only positive values
  nrow() # Get length
positivetraining / nrow(posteriors) * 100 # the effect is positive with a probability of 99.68%

# All of that with just one function describe_posterior():
describe_posterior(model, test = c("p_direction", "rope", "bayesfactor"))

# Describe the logistic model for comparison:
model <- stan_glm(final ~ condition + training, data = data, family = "binomial", refresh = 0)
describe_posterior(model, test = c("pd", "ROPE", "BF"))

model_performance(model)

# References
library(report)
cite_packages()
