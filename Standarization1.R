library(tidyverse)
library(datawizard)
library(effectsize)

# https://easystats.github.io/datawizard/articles/standardize_data.html
# https://neuropsychology.github.io/psycho.R/2018/07/14/standardize_grouped_df.html

# Download the 'emotion' dataset
load(url("https://raw.githubusercontent.com/neuropsychology/psycho.R/master/data/emotion.rda"))

Z_ParticipantWise <- emotion %>%
  group_by(Participant_ID) %>%
  standardize()

fit <- rstanarm::stan_lmer(Subjective_Arousal ~ Emotion_Condition / Recall + (1|Participant_ID) + (1|Item_Category) + (1|Trial_Order), data=Z_ParticipantWise,
                           prior=normal(c(0, 0, 0), c(1, 1, 1)),
                           prior_intercept=normal(0, 1),
                           chains = 3, iter = 1000, seed=666)

fit <- rstanarm::stan_glmer(Subjective_Arousal ~ Emotion_Condition + Emotion_Condition:Recall + (1|Participant_ID),
                               family = "gaussian",
                               data = Z_ParticipantWise,
                               chains = 10, iter = 5000, warmup = 1000, refresh = 0,
                            seed = 1)
posteriors1 <- describe_posterior(fit, dispersion = TRUE, ci = 1,
                                  test = c("pd", "ROPE", "BF"))
beep()


posteriors1 <- describe_posterior(model1, dispersion = TRUE, ci = 1,
                                  test = c("pd", "ROPE", "BF"))
beep()


model <- rstanarm::stan_lmer(Belief ~ AttributedCue / ScenarioType + (1|id), data=data.tmp,
                           prior=normal(c(0, 0, 0), c(1, 1, 1)),
                           prior_intercept=normal(0, 1),
                           chains = 3, iter = 1000, seed=666)

model <- stan_glmer(Belief ~ AttributedCue / ScenarioType + (1|id),
                                 family = "gaussian",
                                 data = data.tmp,
                                 chains = 10, iter = 5000, warmup = 1000, refresh = 0)

posteriors.DV.b <- describe_posterior(fit, dispersion = TRUE, ci = 1,
                                      test = c("pd", "ROPE", "BF"))

model <- rstanarm::stan_glmer(Belief ~ AttributedCue / AttributedCue + ScenarioType + (1|id),
                    family = "gaussian",
                    data = tmp,
                    chains = 10, iter = 5000, warmup = 1000, refresh = 0)

################################################################################
# https://discourse.mc-stan.org/t/why-does-changing-reference-level-of-a-factor-change-estimates-no-intercept-no-interactions/34913
df_3_ref <- mtcars %>%
  mutate(gear = factor(gear) %>% fct_relevel("3", "4", "5"))

df_5_ref <- mtcars %>%
  mutate(gear = factor(gear) %>% fct_relevel("5", "4", "3"))
{
  tmp1 <- data.l %>%
    mutate(AttributedCue = factor(AttributedCue) %>% fct_relevel("FALSE", "TRUE"))
  model1 <- rstanarm::stan_glmer(Belief ~ AttributedCue / ScenarioType + (1|id),
                                family = "gaussian",
                                data = tmp1,
                                chains = 10, iter = 5000, warmup = 1000, refresh = 0)
  posteriors1 <- describe_posterior(model1, dispersion = TRUE, ci = 1,
                                        test = c("pd", "ROPE", "BF"))
  beep()
  }

{
  tmp2 <- data.tmp %>%
    mutate(AttributedCue = factor(AttributedCue) %>% fct_relevel("TRUE", "FALSE"))
  model2 <- rstanarm::stan_glmer(Belief ~ AttributedCue / ScenarioType + (1|id),
                                 family = "gaussian",
                                 data = tmp2,
                                 chains = 10, iter = 5000, warmup = 1000, refresh = 0)
  posteriors2 <- describe_posterior(model2, dispersion = TRUE, ci = 1,
                                    test = c("pd", "ROPE", "BF"))
  beep()
}

{
  tmp3 <- data.tmp %>%
    mutate(ScenarioType = factor(ScenarioType) %>% fct_relevel("PossibleDiscrepancy", "NoDiscrepancy"))
  model3 <- rstanarm::stan_glmer(Belief ~ AttributedCue / ScenarioType + (1|id),
                                 family = "gaussian",
                                 data = tmp3,
                                 chains = 10, iter = 5000, warmup = 1000, refresh = 0)
  posteriors3 <- describe_posterior(model3, dispersion = TRUE, ci = 1,
                                    test = c("pd", "ROPE", "BF"))
  beep()
}
report(posteriors3)
report_effectsize(posteriors3)
report_parameters(model3)
beep()
# 1. Intercept::False-NoDiscrepancy::0.51
# 3.            True-NoDiscrepancy::0.13
# 2.            False-PossibleDisc::0.02
# 5.            True-PossibleDiscr::0.05
# 4.False-Poss::True-PossibleDiscr::0.17
1, 3, 2, 5, false possible - > true possible