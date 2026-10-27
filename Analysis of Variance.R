# Required packages:
install.packages("dplyr")
install.packages("effectsize")
install.packages("rstatix")

# Load the required packages:
library(dplyr)
library(effectsize)
library(rstatix)

# Import data:
# a) if in .txt:
data <- read.delim(file.choose())

# b) if in .csv:
data <- read.csv(file.choose(), sep = ";")

# Attach your data, so that objects in the dataset can be accessed by simply
# giving their names:
attach(data)

# Random data sample:
set.seed(1234)
dplyr::slice_sample(data, n = 10)

# Hypotheses: Is there a significant effect of 'condition' and 'training' type
# on the final responses? And are there significant interactions between these
# two factors?

# Types of both independent and dependent variables:
str(data)
data$condition <- as.factor(data$condition)
data$training <- as.factor(data$training)

# The means of the responses:
group_by(data, condition, training) %>%
  summarise(
    count = n(),
    mean = mean(final, na.rm = TRUE),
    sd = sd(final, na.rm = TRUE)
  )

########################################
#          results comparison          #
########################################
#                Carnap     Elvis      #
#                (0)        (1)        #
# primed    (p)  0.20       0.43       #
# unprimed  (u)  0.20       0.88       #
# control   (c)  0.09       1.00       #
########################################

# Two-Way 3x2 ANOVA

# Frequency tables (unbalanced design):
table(data$condition, data$training)

# Method 1 basic R function aov():
model0 <- aov(final ~ condition * training, data = data)
summary(model0)

options(contrasts = c("contr.sum", "contr.poly")) # type-III SS adjustments 
model0 <- aov(final ~ condition * training, data = data)
drop1(model0,~.,test="F")

effectsize::eta_squared(model0) # partial eta squared

# Method 2 function anova_test() in "rstatix" package:
model1 <- anova_test(data, final ~ condition * training,
                     type = 3, effect.size = "ges")
model1

# One-way model of condition at each level of training:
model <- lm(final ~ condition * training, data = data)
data %>%
  group_by(training) %>%
  anova_test(final ~ condition, error = model)

# Pairwise comparisons:
pwc <- data %>% 
  group_by(training) %>%
  emmeans_test(final ~ condition, p.adjust.method = "bonferroni") 
pwc

posthoc <- lsmeans(model0, # applies to model0
                   pairwise ~ condition*training, 
                   adjust="tukey")
posthoc

# Additional comparisions:
TukeyHSD(model0, which = "training")
TukeyHSD(model0, which = "condition")
pairwise.t.test(data$final, data$training,
                p.adjust.method = "bonferroni")
pairwise.t.test(data$final, data$condition,
                p.adjust.method = "bonferroni")

library(multcomp)
summary(glht(model0, linfct = mcp(training = "Tukey")))
summary(glht(model0, linfct = mcp(condition = "Tukey")))

# Check assumptions

# Outliers
library(rstatix)
data %>% identify_outliers(final)
# Normality
# Build the linear model
model <- lm(final ~ condition * training,
            data = data)
# Create a QQ plot of residuals
library(ggpubr)
ggqqplot(residuals(model))

shapiro_test(residuals(model))

# Homogeneity of variance
data %>% levene_test(final ~ condition * training)

res.aov <- data %>% anova_test(final ~ condition * training)
res.aov

# Homogeneity of variance 
plot(model1, 1)

library(car)
leveneTest(final ~ condition * training, data = data)
# Normality
plot(model1, 2)

# Extract the residuals
aov_residuals <- residuals(object = model0)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

# References
library(report)
cite_packages()
