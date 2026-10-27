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

# Review the dataset:
str(data)

# Fit the model - frequentist framework:
summary(lm(reference ~ valence * probability, data = data)) #with interaction: insignificant interaction
summary(lm(reference ~ valence + probability, data = data))
model <- lm(reference ~ valence + probability, data = data)

insight::get_parameters(model)

effectsize::eta_squared(model)
# Compute beta coefficients
dataStand <- lapply(data, scale) # standardizes all variables
modelStand <- lm(reference ~ valence + probability, data = dataStand)
beta_coef <- coef(modelStand)
beta_coef

# The linear relationship between reference (DV) and valence & probability (IV) is positive and significant.
# valence: (coefficient) b = 0.394, t(56) = 3.436, p < 0.01, eta^2 = 0.17,  beta = 3.958
# probability: b = 0.324, t(56) = 2.817, p < 0.01, eta^2 = 0.12, beta = 3.244
# Together, the two variables explain R^2 = 0.25% of the variability in reference judgments, which is a large effect.
# Reference: Cohen J. (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd Ed. Hillsdale, NJ: Laurence Erlbaum Associates.

# Bayesian framework (the results may slightly vary - each time the model is generated anew)
model <- stan_glm(reference ~ valence + probability, data = data)
posteriorsDs <- describe_posterior(model)
# for a nicer table
print_md(posteriorsDs, digits = 2)

# Extracting the posteriors
posteriors <- insight::get_parameters(model)
head(posteriors)

# Point-estimate (similar to b in frequentist regression)
median(posteriors$valence) # at 0.396, there is 50% chance that the true effect is higher and 50% chance that the effect is lower
median(posteriors$probability) # at 0.321, there is 50% chance that the true effect is higher and 50% chance that the effect is lower

# Uncertainty
# Compute credible interval (similar to a frequentist confidence interval), use 89% CIs instead of 95%  CIs (as in frequentist framework), as 89% level gives more stable results
hdi(posteriors$valence, ci = 0.89) # the effect has 89% chance of falling within the [0.19, 0.58] range
hdi(posteriors$probability, ci = 0.89) # the effect has 89% chance of falling within the [0.14, 0.50] range

# Effect significance
ropeVal <- 0.1 * sd(data$reference) # define ROPE (Region of Practical Equivalence: https://easystats.github.io/bayestestR/articles/region_of_practical_equivalence.html) as the tenth (1/10 = 0.1) of the standard deviation (SD) of the response variable, which can be considered as a “negligible” effect size (Cohen, 1988)
ropeRange <- c(-ropeVal, ropeVal)
ropeRange

rope(posteriors$valence, range = ropeRange, ci = 0.89)
rope(posteriors$probability, range = ropeRange, ci = 0.89)

# Probability of Direction (pd)
positiveValence <- posteriors %>%
  filter(valence > 0) %>% # select only positive values
  nrow() # get length
positiveValence / nrow(posteriors) * 100 # the effect is positive with a probability of 99.95%

positiveProbability <- posteriors %>%
  filter(probability > 0) %>% # select only positive values
  nrow() # get length
positiveProbability / nrow(posteriors) * 100 # the effect is positive with a probability of 99.68%

# All of that with just one function describe_posterior():
describe_posterior(model, test = c("p_direction", "rope", "bayesfactor"))

# Describe the logistic model for comparison:
model <- stan_glm(reference ~ valence + probability, data = data, family = "binomial", refresh = 0)
describe_posterior(model, test = c("pd", "ROPE", "BF"))

model_performance(model)

# References
library(report)
cite_packages()
