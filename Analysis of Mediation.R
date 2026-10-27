# Required packages:
install.packages("mediation")
install.packages("bayestestR")
install.packages("brms")
install.packages("rstanarm")

# Load the required packages:
library(mediation)
library(bayestestR)
library(brms)
library(rstanarm)

# Import data:
# a) if in .txt:
data <- read.delim(file.choose())

# b) if in .csv:
data <- read.csv(file.choose(), sep = ";")


# Frequentist framework: valence
summary(lm(reference ~ valence, data = data)) #a1) IV correlates with DV
summary(lm(reference ~ sub_valence, data = data)) #b) mediator correlates with DV
summary(lm(sub_valence ~ valence, data = data)) #a2) mediator correlates with IV
summary(lm(reference ~ sub_valence + valence, data = data)) #c) the correlation of the IV on the DV becomes weaker once the influence of the mediator is accounted for
cor.test(data$valence, data$reference) #correlation of IV and DV
cor.test(data$sub_valence, data$reference) #correlation of mediator and DV

stepa2 <- lm(sub_valence ~ valence, data = data) #a2) mediator correlates with IV
stepc <- lm(reference ~ sub_valence + valence, data = data) #c) the correlation of the IV on the DV becomes weaker once the influence of the mediator is accounted for
m1 <- mediate(stepa2, stepc, sims = 1000, treat = "valence", mediator = "sub_valence")
summary(m1)

# Frequentist framework: probability
summary(lm(reference ~ probability, data = data)) #a1) IV correlates with DV
summary(lm(reference ~ sub_probability, data = data)) #b) mediator correlates with DV
summary(lm(sub_probability ~ probability, data = data)) #a2) mediator does not correlate with IV
summary(lm(reference ~ sub_probability + probability, data = data)) #c) the correlation of the IV on the DV does not become weaker once the influence of the mediator is accounted for
cor.test(data$probability, data$reference) #correlation of IV and DV
cor.test(data$sub_probability, data$reference) #correlation of mediator and DV

# Bayesian framework: valence, model in brms
stepa2 <- bf(sub_valence ~ valence)
stepc <- bf(reference ~ sub_valence + valence)
m2 <- brm(stepa2 + stepc + set_rescor(FALSE), data = data, cores = 4)

mediation(m2, ci = 0.95)

# Bayesian framework: valence, model in rstanarm
m3 <- stan_mvmer(
  list(sub_valence ~ valence + (1 | sub_probability),
       reference ~ sub_valence + valence + (1 | sub_probability)),
  data = data,
  cores = 4,
  refresh = 0
)
mediation(m3, ci = 0.95)

# References
library(report)
cite_packages()
