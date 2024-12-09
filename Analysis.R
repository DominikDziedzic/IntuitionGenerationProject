### Install and load the required packages:

# Librarian::shelf() installs the packages and loads them, while lib_startup()
# creates a .Rprofile file in the working directory to tell R to load the
# packages at the start of every session.
{
  install.packages("librarian")
  library(librarian)
  
  shelf(tidyverse, rstanarm, bayestestR, performance, lmerTest, beepr)
  lib_startup(librarian, tidyverse, rstanarm, bayestestR, performance, lmerTest, 
              beepr, lib = getwd(), global = FALSE)
} # Note: There may be problems installing some packages; in particular, RStudio
# gets stuck in an endless loop of restarting the session when trying to
# install/update packages on my PC. If this is the case, shelf() may not attach
# any packages. Try installing them manually in R instead of Rstudio.


### Install the required packages:
# https://github.com/easystats/report

### Import data

# a) if in .txt:
data <- read.delim(file.choose())
# b) if in .csv:
data <- read.csv(file.choose(), sep = ";")

### Data frame format guidelines

## Variables identification: Exported data is in a "wide" form. Columns in the
## data frame correspond to the following variables: 

# 1:13: various ID info including participant gender, age, years of superior
# education;

# 14:17: Instructions screen + Instructions comprehension questions;

# 18:38: Training phase;

# 39:48: one task out of twenty in the study; the task consists of:
# - 39: RNG to determine a condition – either TRUE (1) or FALSE (2),
# - 40:41: one of two screens corresponding to the condition is displayed for 5
# sec.,
# -	42:44: the condition is repeated on the next screen + one scenario out of
# twenty is displayed,
# -	45:47: questions about the nature of the scenarios; that is,
# conventionality, objectivity and permissibility, respectively,
# -	48: question whether the participant agrees with AI judgment;

# 49:245: rest of the twenty tasks + three sets of comprehension questions
# (89:90, 141:142, 193:194) + participants’ feedback on the study (245);

# 246:516: various recorded times.

## Run the following to format the data:
{
  data.w <- data # make a backup of the data in the "wide" format.
  
# The following measures are taken to format the data:
  
# 1. Only the participants who answered comprehension questions (paid attention
# to the displayed cues) are selected for further calculations.
  
# 2. Outcome variables (i.e., answers to questions about Conventionality,
# Objectivity and Permissibility of the demonstrative use given a scenario,
# e.g., columns 45:47) as well as participants’ beliefs (whether they agree with
# AI judgment, e.g., column 48) are standardized participant-wise. Guide:
# https://neuropsychology.github.io/psycho.R/2018/07/14/standardize_grouped_df.html
  
# 3. The "wide" data frame is transformed into a "long" variant, and all the
# columns containing participants' responses are stacked on top of each other to
# produce such variables as ‘Attributed Cue’ to keep track of the displayed
# cues.
  
# 4. Missing info (not included in the online questionnaire, e.g., scenario type
# categorization) is supplemented along the way.
  
# Comprehension questions: The goal is, first, to count the number of mistakes
# each participant made with respect to the attributed cues (i.e., whether she
# remembers the ‘TRUE’/‘False’ cue displayed just now) and the definitions of
# Conventionality, Objectivity and Permissibility (whether she remembers the
# definitions). Then, the calculations are used to remove the participants who
# disremembered the attributed cues; we will not, however, remove those who
# disremembered the definitions since quite a lot of participants experienced
# problems with this part of the study (see ‘Feedback2’ variable); that is, the
# study may be flawed in this part.
  {
    data.tmp <- data.w
    
    data.tmp$Mistake1 <- data.tmp$randnumber5
    data.tmp$Mistake2 <- data.tmp$randnumber10
    data.tmp$Mistake3 <- data.tmp$randnumber16
    
    data.tmp$Mistake1[data.tmp$Mistake1==2] <- 0
    data.tmp$Mistake1 <- data.tmp$Mistake1 == data.tmp$CQ11
    
    data.tmp$Mistake2[data.tmp$Mistake2==2] <- 0
    data.tmp$Mistake2 <- data.tmp$Mistake2 == data.tmp$CQ21
    
    data.tmp$Mistake3[data.tmp$Mistake3==2] <- 0
    data.tmp$Mistake3 <- data.tmp$Mistake3 == data.tmp$CQ31
    
    data.tmp$MistakeConv <- data.tmp$CQ12 == "Rules for preserving the connection between words and what they stand for"
    data.tmp$MistakeObje <- data.tmp$CQ22 == "Type of meaning determined by what the speaker wishes to talk about and not necessarily by what she indicated in practice"
    data.tmp$MistakePerm <- data.tmp$CQ32 == "Rules governing how to communicate with others to ensure clarity and understanding"
    
    for (i in 517:522) {
      tmp <- as.factor(data.tmp[,i])
      data.tmp[,i] <- as.numeric(as.character(factor(tmp, levels=c('FALSE', 'TRUE'), labels=c(1, 0))))
    }
    
    data.tmp$AttCueMistakes <- rowSums(data.tmp[,517:519])
    data.tmp$DefMistakes <- rowSums(data.tmp[520:522])
    data.tmp$TotalMistakes <- rowSums(data.tmp[523:524])
    
    data.tmp <- data.tmp[,c(-517:-522)]
    
    # erase those who made
    # at least 1 mistake assessing the attributed cues
    # data.tmp1 <- data.tmp[-which(data.tmp$AttCueMistakes > 0),]
    
    # at least 1 mistake (or 3 mistakes) remembering the definitions
    # data.tmp2 <- data.tmp[-which(data.tmp$DefMistakes > 0),]
  }
  ######### Notes to delete later:
  # at least 1 mistake anywhere
  # data.tmp1 <- data.tmp[-which(data.tmp$TotalMistakes > 0),]
  
  # at least 1 mistake remembering the definitions but no assessing the cues
  # data.tmp3 <- data.tmp[-which(data.tmp$DefMistakes > 0 & data.tmp$AttCueMistakes > 0),]
  
  # at least 1 mistake assessing the cues and all 3 remembering the definitions
  # data.tmp4 <- data.tmp[]
  #########
  # Tutaj znajduje siê rozró¿nienie na 3 tabele danych: (1) ze wszystkimi
  # uczestnikami, (2) oprócz tych z AttCueMistakes, (3) oprócz tych z 
  # DefMistakes, ewentaulnie tak¿e (4) oprócz tych z obiema Mistakes.
  #########
  
  data.tmp <- subset(data, select = -c(89:90, 141:142, 193:194, 245:516)) 
  # the removed data can be accessed in data.w

# Note: The online survey tool used in this study did not include information
# about the types of examined scenarios. Therefore this information must be
# entered into the dataset manually. The information about the types of
# scenarios (i.e., whether a given scenario is of type ‘NoDiscrepancy’ or
# ‘PossibleDiscrepancy’) comes from the Discrepancy Coding Procedure that
# accompanies the current study, see:
#https://github.com/DominikDziedzic/DemonstrativesDiscrepancyCoding
  {
    data.tmp$Scenario2 <- "PossibleDiscrepancy" # (Kaplan, 1978, p. 239)
    data.tmp$Scenario3 <- "PossibleDiscrepancy" # (Gauker, 2008, p. 363)
    data.tmp$Scenario4 <- "PossibleDiscrepancy" # (Reimer, 1991a, pp. 190–191)
    data.tmp$Scenario5 <- "PossibleDiscrepancy" # (Reimer, 1991b, p. 180)
    data.tmp$Scenario6 <- "NoDiscrepancy" # (de Gaynesford, 2006, p. 169)
    data.tmp$Scenario7 <- "NoDiscrepancy" # (McGinn, 1981, pp. 161–162)
    data.tmp$Scenario8 <- "PossibleDiscrepancy" # (McGinn, 1981, p. 162)
    data.tmp$Scenario9 <- "NoDiscrepancy" # (Perry, 2009, p. 193)
    data.tmp$Scenario10 <- "PossibleDiscrepancy" # (Siegel, 2002, pp. 10–11)
    data.tmp$Scenario11 <- "NoDiscrepancy" # "A Pianist at the Party"
    data.tmp$Scenario12 <- "NoDiscrepancy" # (Textor, 2007, p. 955)
    data.tmp$Scenario13 <- "NoDiscrepancy" # (Perry, 2017, p. 979)
    data.tmp$Scenario14 <- "NoDiscrepancy" # (Ciecierski & Makowski, 2020)
    data.tmp$Scenario15 <- "NoDiscrepancy" # (King, 1999, p. 156)
    data.tmp$Scenario17 <- "PossibleDiscrepancy" # (Reimer, 1991a, p. 194)
    data.tmp$Scenario21 <- "PossibleDiscrepancy" # (King, 2014, p. 224)
    data.tmp$Scenario22 <- "PossibleDiscrepancy" # (Radulescu, 2019, p. 7)
    data.tmp$Scenario23 <- "PossibleDiscrepancy" # (Reimer, 1991b, p. 182)
    data.tmp$Scenario24 <- "NoDiscrepancy" # (Radulescu, 2019, p. 15)
    data.tmp$Scenario26 <- "NoDiscrepancy" # (Ullman et al., 2012, p. 457)
    
    index <- grep("^Scenario", colnames(data.tmp))
    names <- c("(Kaplan, 1978, p. 239)","(Gauker, 2008, p. 363)",
               "(Reimer, 1991a, pp. 190–191)", "(Reimer, 1991b, p. 180)",
               "(de Gaynesford, 2006, p. 169)", "(McGinn, 1981, pp. 161–162)",
               "(McGinn, 1981, p. 162)", "(Perry, 2009, p. 193)",
               "(Siegel, 2002, pp. 10–11)", "A Pianist at the Party",
               "(Textor, 2007, p. 955)", "(Perry, 2017, p. 979)",
               "(Ciecierski & Makowski, 2020)", "(King, 1999, p. 156)",
               "(Reimer, 1991a, p. 194)", "(King, 2014, p. 224)",
               "(Radulescu, 2019, p. 7)", "(Reimer, 1991b, p. 182)",
               "(Radulescu, 2019, p. 15)", "(Ullman et al., 2012, p. 457)")
    for (i in 1:20) {
      data.tmp <- data.tmp %>% add_column(ScenarioName = names[i], .after = index[i]+i-1, .name_repair = "universal")
    }
  }
    
# Now, the goal is to transform the data frame into a "long" variant with all
# the columns containing participants' responses stacked on top of each other;
# in other words: to have all of the required variables for the analysis. A
# helpful guide:
# https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames/24376207#24376207

# Independent variable: Attributed cue.
  list.tmp <- list()
  for (i in 1:20){
    list.tmp[[i]] <- cbind(data.tmp[1:38], data.tmp[33+11*i], data.tmp[34+11*i],
                           stack(data.tmp[28+11*i]))
    names(list.tmp[[i]])[39] <- "ScenarioType"
    names(list.tmp[[i]])[40] <- "ScenarioName"
  }
  # str(list.tmp) # list of the left side of the data frame + scenario types +
  # names + attributed cues.
  data.p1 <- do.call(what = rbind, args = list.tmp) # combining the list into a
  # single data frame by stacking the "small" data frames on top of each other.
  names(data.p1)[41] <- 'AttributedCue'
  data.p1$AttributedCue[data.p1$AttributedCue==2] <- 0
  data.p1$AttributedCue <- as.logical(data.p1$AttributedCue) # note: now, the 
  # condition is either TRUE (following 1) or FALSE (0).
  data.p1$ScenarioType <- as.factor(data.p1$ScenarioType)
  data.p1$ScenarioName <- as.factor(data.p1$ScenarioName)
  
# Independent variable: Belief.
  list.tmp <- list()
  for (i in 1:20){
    list.tmp[[i]] <- stack(data.tmp[38+11*i])
  }
  # str(list.tmp) # list of replies to 'Do you agree with the AI?' question.
  data.p2 <- do.call(what = rbind, args = list.tmp)
  names(data.p2)[1] <- 'Belief'
  # Note: There are two IV: AttributedCue & Belief; while the former has only
  # two logical values [see: str(data.p1[40])], the latter ranges from 0 to 100
  # [see: str(data.p2)]. It is recommended to scale the Belief variable for
  # representative results:
  data.p2$Belief <- data.p2$Belief / 100
  
# Dependent variable: Conventionality.
  list.tmp <- list()
  for (i in 1:20){
    list.tmp[[i]] <- stack(data.tmp[35+11*i])
  }
  # str(list.tmp) # list of replies to 'The use... was conventional' question.
  data.p3 <- do.call(what = rbind, args = list.tmp)
  names(data.p3)[1] <- 'Conventionality'
  
# Dependent variable: Objectivity.
  list.tmp <- list()
  for (i in 1:20){
    list.tmp[[i]] <- stack(data.tmp[36+11*i])
  }
  # str(list.tmp) # list of replies to 'The use... was with the lexical,
  # objective meaning' question.
  data.p4 <- do.call(what = rbind, args = list.tmp)
  names(data.p4)[1] <- 'Objectivity'

# Dependent variable: Permissibility.
  list.tmp <- list()
  for (i in 1:20){
    list.tmp[[i]] <- stack(data.tmp[37+11*i])
  }
  # str(list.tmp) # list of replies to 'The use... was permissible' question.
  data.p5 <- do.call(what = rbind, args = list.tmp)
  names(data.p5)[1] <- 'Permissibility'
  
# Binding it all together.
  data.l <- cbind(data.p1, data.p2, data.p3, data.p4, data.p5)
  data.l <- data.l[with(data.l, order(data.l$id)),]
  for (i in 1:5){
    names(data.l)[40+2*i] <- paste0('Legend', i)
  }
}

### Model selection

# Run the following to, first, fit Bayesian and frequentist versions of
# regression models and, second, compute indices of model performance for these
# models. When the program stops executing this part of the script, all indices
# will be stored in the ‘metrics’ table. Note that the execution of the
# 'stan_glmer' and ‘model_performance’ functions may take a few minutes.

{
  set.seed(25.8069758011) # for reproducible outputs--the seed is used in the
  # following computations. 
  
################################################################################
# Delete:  
## Adding trial number variable that becomes random intercept.
  # Instead add random intercepts - for paticipants and items (ScenarioName?)
  # Test random intercept - trial number - for fatigue, regardless of same order
  data.l <- data.l %>% add_column(TrialNumber = rep(1:20, times = length(unique(data.l[,1]))))

################################################################################

## Conventionality.
  # Conventionality ~ AttributedCue.
  { # Bayesian.
    model <- stan_glm(Conventionality ~ ScenarioType + ScenarioType:AttributedCue,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    posteriors.con.a <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                           test = c("pd", "ROPE", "BF"))
    
    metrics.con1 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Conventionality ~ AttributedCue, data = data.l)
    
    indices.con1 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  ##############################################################################
  # Conventionality ~ ScenarioType / AttributedCue
  # Conventionality ~ ScenarioType / Belief
  # Conventionality ~ ScenarioType / AttributedCue + Belief
  # Conventionality ~ ScenarioType / AttributedCue * Belief
  # lm(y ~ x/z, data) is just a shortcut for lm(y ~ x + x:z, data)
  # Delete:
  { # Bayesian.                         No/Possible D.  True/False Intercept: False/No, effect of Possible, in False, effect of No, in True, effect of Possible in True.
    model <- stan_glm(Conventionality ~ ScenarioType / AttributedCue,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    posteriors.con.b <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                           test = c("pd", "ROPE", "BF"))
    
    metrics.con2 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Conventionality ~ AttributedCue, data = data.l)
    
    indices.con1 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  ##############################################################################
  # Delete: what to do, what to test.
  # Conventionality ~ A, B, A + B, A:B, A*B (read help(formula) for * and /)
  # read (Wysocki on normality) for nested
  # Conventionality ~ X/A, X/B, X/A+B, X/A:B, X/A*B
  # The same for Objectivity and Permissibility
  # Remember to set.seed!
  help(formula)
  ##############################################################################
  # Conventionality ~ Belief.
  { # Bayesian.
    model <- stan_glm(Conventionality ~ Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.con.b <- describe_posterior(model, test = c("pd", "ROPE", "BF"))
    
    metrics.con2 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Conventionality ~ Belief, data = data.l)
    
    indices.con2 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Conventionality ~ AttributedCue + Belief.
  { # Bayesian.
    model <- stan_glm(Conventionality ~ AttributedCue + Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.con.ab <- describe_posterior(model,
                                             #test = c("pd", "ROPE", "BF"))
    
    metrics.con3 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Conventionality ~ AttributedCue + Belief, data = data.l)
    
    indices.con3 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Conventionality ~ AttributedCue * Belief.
  { # Bayesian.
    model <- stan_glm(Conventionality ~ AttributedCue * Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.con.AB <- describe_posterior(model,
                                             #test = c("pd", "ROPE", "BF"))
    
    metrics.con4 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Conventionality ~ AttributedCue * Belief, data = data.l)
    
    indices.con4 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  
  # Build a table to store the indices of models performance for comparison.
  {
  metrics <- data.frame(
    matrix("", ncol = 2+length(metrics.con1)+length(indices.con1), nrow = 3*4))
  for (i in 1:4) {
    colnames(metrics) <- c("Variable", "Model", colnames(metrics.con1),
                           colnames(indices.con1)) 
    metrics[i,1] <- paste0("Conventionality")
    row.tmp <- c("A", "B", "A+B", "A*B")
    metrics[i,2] <- row.tmp[i]
    
    row.tmp <- get(paste0("metrics.con", i))
    metrics[i,3:6] <- row.tmp
    
    row.tmp <- get(paste0("indices.con", i))
    metrics[i,7:8] <- row.tmp
    }
  }

## Objectivity.
  # Objectivity ~ AttributedCue.
  { # Bayesian.
    model <- stan_glm(Objectivity ~ AttributedCue,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    posteriors.obj.a <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                           test = c("pd", "ROPE", "BF"))
    
    metrics.obj1 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Objectivity ~ AttributedCue, data = data.l)
    
    indices.obj1 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Objectivity ~ Belief.
  { # Bayesian.
    model <- stan_glm(Objectivity ~ Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.obj.b <- describe_posterior(model, test = c("pd", "ROPE", "BF"))
    
    metrics.obj2 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Objectivity ~ Belief, data = data.l)
    
    indices.obj2 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Objectivity ~ AttributedCue + Belief.
  { # Bayesian.
    model <- stan_glm(Objectivity ~ AttributedCue + Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.obj.ab <- describe_posterior(model,
    #test = c("pd", "ROPE", "BF"))
    
    metrics.obj3 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Objectivity ~ AttributedCue + Belief, data = data.l)
    
    indices.obj3 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Objectivity ~ AttributedCue * Belief.
  { # Bayesian.
    model <- stan_glm(Objectivity ~ AttributedCue * Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.obj.AB <- describe_posterior(model,
    #test = c("pd", "ROPE", "BF"))
    
    metrics.obj4 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Objectivity ~ AttributedCue * Belief, data = data.l)
    
    indices.obj4 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  
  # Fill out the table with indices of models performance for comparison.
  {
    for (i in 1:4) {
    metrics[i+4,1] <- paste0("Objectivity")
    row.tmp <- c("A", "B", "A+B", "A*B")
    metrics[i+4,2] <- row.tmp[i]
    
    row.tmp <- get(paste0("metrics.obj", i))
    metrics[i+4,3:6] <- row.tmp
      
    row.tmp <- get(paste0("indices.obj", i))
    metrics[i+4,7:8] <- row.tmp
    }
  }
  
## Permissibility.
  # Permissibility ~ AttributedCue.
  { # Bayesian.
    model <- stan_glm(Permissibility ~ AttributedCue,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    posteriors.per.a <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                           test = c("pd", "ROPE", "BF"))
    
    metrics.per1 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Permissibility ~ AttributedCue, data = data.l)
    
    indices.per1 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Permissibility ~ Belief.
  { # Bayesian.
    model <- stan_glm(Permissibility ~ Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.per.b <- describe_posterior(model, test = c("pd", "ROPE", "BF"))
    
    metrics.per2 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Permissibility ~ Belief, data = data.l)
    
    indices.per2 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Permissibility ~ AttributedCue + Belief.
  { # Bayesian.
    model <- stan_glm(Permissibility ~ AttributedCue + Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.per.ab <- describe_posterior(model,
    #test = c("pd", "ROPE", "BF"))
    
    metrics.per3 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Permissibility ~ AttributedCue + Belief, data = data.l)
    
    indices.per3 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  # Permissibility ~ AttributedCue * Belief.
  { # Bayesian.
    model <- stan_glm(Permissibility ~ AttributedCue * Belief,
                      family = "gaussian",
                      data = data.l,
                      chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
    #posteriors.per.AB <- describe_posterior(model,
    #test = c("pd", "ROPE", "BF"))
    
    metrics.per4 <- model_performance(model, metrics = c("LOOIC"))
    
    # Frequentist.
    model.freq <- lm(Permissibility ~ AttributedCue * Belief, data = data.l)
    
    indices.per4 <- model_performance(model.freq, metrics = c("AIC", "BIC"))
    beep()
  }
  
  # Fill out the table with indices of models performance for comparison.
  {
    for (i in 1:4) {
      metrics[i+8,1] <- paste0("Permissibility")
      row.tmp <- c("A", "B", "A+B", "A*B")
      metrics[i+8,2] <- row.tmp[i]
      
      row.tmp <- get(paste0("metrics.per", i))
      metrics[i+8,3:6] <- row.tmp
      
      row.tmp <- get(paste0("indices.per", i))
      metrics[i+8,7:8] <- row.tmp
    }
  }
}

### Experimental manipulation check.

## Bayesian. 
# Indices relevant to describing posterior distributions for AttributedCue
# across the three DVs are stored in the following objects; for instance, the
# posterior distribution for AttributedCue and Conventionality can be described
# by calling ‘posteriors.con.a’.

posteriors.con.a
# The intercept, corresponding to the AttributedCueFALSE condition, is 61.42
# (MAD = 0.94, 100% CI [57.63, 65.14], 0% in ROPE); that is, there is a 50%
# chance that the Conventionality in the False condition is judged higher than
# 61.42 and a 50% chance that the same is judged lower. The median absolute
# deviation (MAD, equivalent to standard error) is 0.94. Compared with that, the
# effect of the AttributedCueTRUE has a probability of 100% of being positive
# (pd: the probability of direction) (Median = 8.75, MAD = 1.33, 100% CI [3.26,
# 14.22], and the effect can be considered as significant (0% in ROPE: region of
# practical equivalence).

## Frequentist.
# The above is analogous to results obtained in the frequentist framework. 

model <- lm(Conventionality ~ AttributedCue, data = data.l)
summary(model)
# The intercept, corresponding to the AttributedCueFALSE condition, is positive
# and significant: beta = 61.42, SE = 0.94, t(2218) = 65.49, p < 0.001. The
# linear relationship between Conventionality and AttributedCueTRUE is also
# positive and significant: beta = 8.74, SE = 1.33, t(2218) = 6.58, p < 0.001.

## Bayesian.

posteriors.obj.a
# Note the minor significance of the AttributedCueTRUE effect: 36.35% in ROPE.
# Compute a custom ROPE range. Guide:
# https://easystats.github.io/bayestestR/articles/region_of_practical_equivalence.html

rope_value <- 0.1 * sd(data.l$Objectivity)
rope_range <- c(-rope_value, rope_value)
rope(posteriors$AttributedCueTRUE, range = rope_range, ci = 0.89)
# Analogous result: 34.86% in ROPE.

## Frequentist.

model <- lm(Objectivity ~ AttributedCue, data = data.l)
summary(model)
# As was the case in the Bayesian framework, the significance of the effect of
# the True condition is undecided: 0.001 < p < 0.01.

## Bayesian.

posteriors.per.a

## Frequentist.

model <- lm(Permissibility ~ AttributedCue, data = data.l)
summary(model)

# Construct one variable using the DVs (Conventionality, Objectivity,
# Permissibility) and check whether the TRUE/FALSE manipulation had significant
# effects on the data as a whole.

{
  list.tmp <- list()
  for (i in 1:3){
    list.tmp[[i]] <- cbind(data.l[39:44], stack(data.l[43+2*i]))
  }
  data.tmp <- do.call(what = rbind, args = list.tmp)
  names(data.tmp)[7:8] <- c('DVs','Legend3')
}
str(list.tmp)
## Bayesian.

{
  set.seed(25.8069758011) # for reproducible outputs--the seed is used in the
  # following computations. 
  
  model <- stan_glm(DVs ~ AttributedCue,
                    family = "gaussian",
                    data = data.tmp,
                    chains = 10, iter = 5000, warmup = 1000, refresh = 0)
    
  posteriors.DVs.a <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                         test = c("pd", "ROPE", "BF"))
  beep()
  print(posteriors.DVs.a)
}

## Frequentist.

model <- lm(DVs ~ AttributedCue, data = data.tmp)
summary(model)
# In both Bayesian and frequentist variants, the effects of AttributedCue are
# significant; FALSE (beta = 59.03, SE = 0.57, t(6658) = 103.62, p < 0.001),
# TRUE (beta = 6.50, SE = 0.81, t(6658) = 8.06, p < 0.001).

# Check whether a significant effect of the Belief variable on DVs was observed.

## Bayesian.

{
  set.seed(25.8069758011) # for reproducible outputs--the seed is used in the
  # following computations. 
  
  model <- stan_glm(DVs ~ Belief,
                    family = "gaussian",
                    data = data.tmp,
                    chains = 10, iter = 5000, warmup = 1000, refresh = 0)
  
  posteriors.DVs.b <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                       test = c("pd", "ROPE", "BF"))
  beep()
  print(posteriors.DVs.b)
}

## Frequentist.

model <- lm(DVs ~ Belief, data = data.tmp)
summary(model)

# Check whether a significant effect of the ScenarioType variable on DVs was 
# observed.

## Bayesian.

{
  set.seed(25.8069758011) # for reproducible outputs--the seed is used in the
  # following computations. 
  
  model <- stan_glm(DVs ~ ScenarioType,
                    family = "gaussian",
                    data = data.tmp,
                    chains = 10, iter = 5000, warmup = 1000, refresh = 0)
  
  posteriors.DVs.s <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                         test = c("pd", "ROPE", "BF"))
  beep()
  print(posteriors.DVs.s)
}

## Frequentist.

model <- lm(DVs ~ ScenarioType, data = data.tmp)
summary(model)

################################################################################

## Bayesian.
# https://discourse.mc-stan.org/t/function-for-repeatedly-updating-models-not-working/21269


get_par <- function(model) {
  mod_name <- deparse(substitute(model))
  parameters::model_parameters(model) %>%
    mutate(Model = mod_name) %>%
    select(-Parameter) %>%
    select(Model, everything()) %>%
    .[-1, ]
}

rbind(
  get_par(m.raw),
  get_par(m.raw1),
  get_par(m.raw2)
)

## Frequentist.

m.raw <- lm(DVs ~ AttributedCue, data = data.tmp)
m.raw1 <- update(m.raw, ~ Belief)
m.raw2 <- update(m.raw, ~ ScenarioType)

experimental.manipulation <- function(model) {
  mod.name <- deparse(substitute(model))
  parameters::model_parameters(model) %>%
    mutate(Model = mod.name, .before = Parameter) %>%
    mutate(Model = ifelse(duplicated(Model), NA, Model))
}

rbind(
  experimental.manipulation(m.raw),
  experimental.manipulation(m.raw1),
  experimental.manipulation(m.raw2)
)

#########

# Notes:
# 1. Compute custom ROPE (DONE)
# 2. Test scaled Belief (or DVs instead)
# 3. Test scaled dependent variables
# 4. Sprawdziæ pytania kontrolne, odrzuciæ uczestników ze z³ymi odpowiedziami, 
# porównaæ wyniki miêdzy pe³n¹ baz¹ i t¹ po redukcji
# 5. Dodaæ "trial number"?
# 6. Sprawdziæ, na czym polega taki zapis modelu:
# "Z_Subjective_Arousal ~ Emotion / Subjective_Condition"
# 7. Modele nr 9 i 10 daj¹ b³¹d: Possible multicollinearity between ... See
# 'Details' in '?rope'. Sprawdziæ dok³adniej, np. https://discourse.mc-stan.org/t/too-much-shrinkage-with-horseshoe-prior/19041/6
# Sprawdziæ te¿ otwarte zak³adki "Stats" w poszukiwaniu multicollinearity, jak
# równie¿ spróbowaæ dopisaæ dispersion = TRUE i/lub ci = 1
# 8. spróbowaæ: check_model(model) https://cran.r-project.org/web/packages/performance/readme/README.html
# uzyc parametrow modelu od Makowskiego
# 9. Sprawdziæ, czy zachodzi ró¿nica po wpisaniu "id" lub "seed", lub "ProlificID" jako random intercepts
# 10. ZnaleŸæ sposób na wpisanie "participants", scenariusza"ScenarioName" i trial number
# *** Odnoœnie trial number: sprawdziæ, czy scenariusze by³y wyœwietlane w losowej kolejnoœci
# 11. Sprawdziæ, czy modele z/bez random intercepts siê ró¿ni¹

### Belief rate
{
# data.l.tmp <- data.l[-which(data.l$id == 51 | data.l$id == 101 | data.l$id == 143),]
  
data.tmp <- data.l %>% 
  group_by(id, AttributedCue, ScenarioType) %>% 
  dplyr::summarise(Belief = mean(Belief))
  
Belief.stats <- data.frame(Mean = mean(data.tmp$Belief),
                           SD = sd(data.tmp$Belief),
                           N = length(levels(as.factor(data.tmp$id))))
print(Belief.stats)
} # TODO: add test for "significantly higher than 0.5"? (Makowski, 2019, p.9)

t.test(data.tmp$Belief, mu = 0.5)

{
data.tmp <- data.l %>%
  group_by(AttributedCue, ScenarioType) %>%
  dplyr::summarise(Belief = mean(Belief))
  
data.tmp %>% 
  ggplot() +
  aes(x = ScenarioType, y = Belief, color = AttributedCue) +
  geom_line(aes(group = AttributedCue)) +
  geom_point()
}

### Belief rate: design
# Calculate anew on the basis 1. data frame without those who failed
# Comprehension Q. 2. and without those who scored sd = 0. 3. with scaled Belief
# factor (scaled how?).
# Participants 51 and 143 have sd = 0 in Conventionality
# Participants 101 and 143 have sd = 0 in Permissibility
# Conclusion: most difference gives 1. with 3.

{
  set.seed(25.8069758011) # for reproducible outputs--the seed is used in the
  # following computations. 
  
  model <- stan_glm(Belief ~ AttributedCue1 / ScenarioType1,
                    family = "gaussian",
                    data = data.tmp,
                    chains = 10, iter = 5000, warmup = 1000, refresh = 0)
  
  posteriors.DV.b <- describe_posterior(model, dispersion = TRUE, ci = 1,
                                        test = c("pd", "ROPE", "BF"))
  beep()
  print(posteriors.DV.b)
}
report(model)
beep()
## Frequentist.
model <- lm(Belief ~ AttributedCue1 + ScenarioType1, data = data.tmp)
summary(model)

# tmp <- as.factor(data.tmp[,i])
# data.tmp[,i] <- as.numeric(as.character(factor(tmp, levels=c('FALSE', 'TRUE'), labels=c(1, 0))))
tmp <- as.factor(data.tmp$AttributedCue)
data.tmp$AttributedCue1 <- as.numeric(as.character(factor(tmp, levels=c('FALSE', 'TRUE'), labels=c(0, 1))))
tmp <- as.factor(data.tmp$ScenarioType)
data.tmp$ScenarioType1 <- as.numeric(as.character(factor(tmp, levels=c('NoDiscrepancy', 'PossibleDiscrepancy'), labels=c(0, 1))))
str(data.tmp)

str(b)
### I have: Intercept:Fals-NoDis
#                     Fals-PoDis : with Inter
#                     True-NoDis : with Inter
#                     True-PoDis : with Fals-PoDis
#                     True-PoDis : with True-NoDis
#                     

# I need:   Intercept:Real-Neut compared with Real-Neut
#                     Real-Nega               Real-Nega : with Inter
#                     Simu-Neut               Simu-Neut : with Inter
#                     Simu-Nega               Simu-Nega : with Real-Nega
#                     Simu-Nega               Simu-Nega : with Simu-Neut

### Try the following format: Y ~ Z / X
### Test whether the 3 DVs differ significantly


################################################################################
# Tests and future work:
{
model1 <- lmer(Belief ~ AttributedCue + (1 | ScenarioType), data = data.l)
model2 <- lm(Belief ~ AttributedCue, data = data.l)
model3 <- lm(Belief ~ ScenarioType, data = data.l)
model4 <- lm(Belief ~ AttributedCue + ScenarioType, data = data.l)
model5 <- lmer(Belief ~ AttributedCue + ScenarioType + (1 | ProlificID), data = data.l)
model6 <- lmer(Belief ~ AttributedCue * ScenarioType + (1 | ProlificID), data = data.l)
anova(model1, model2, model3, model4, model5, model6)
}
summary(model5)
#               DV:Conventionality    ScenarioType Model
# lmerTest::lmer(Z_Subjective_Arousal ~ Emotion / Subjective_Condition + (1|Participant_ID) + (1|Item) + (1|Order), data=df)
fiMo <- glm(Conventionality ~ ScenarioType / AttributedCue, data = data.l)
fiMoC <- glm(Conventionality ~ ScenarioType / Belief, data = data.l); anova(fiMo, fiMoC)
fiMoC <- glm(Conventionality ~ ScenarioType / AttributedCue + Belief, data = data.l); anova(fiMo, fiMoC)
fiMoC <- glm(Conventionality ~ ScenarioType / AttributedCue * Belief, data = data.l); anova(fiMo, fiMoC)

fiMo <- lmerTest::lmer(Conventionality ~ ScenarioType / AttributedCue + (1|ProlificID), data = data.l)
fiMoC <- lmerTest::lmer(Conventionality ~ ScenarioType / Belief + (1|ProlificID), data = data.l); anova(fiMo, fiMoC)
fiMoC <- lmerTest::lmer(Conventionality ~ ScenarioType / AttributedCue + Belief + (1|ProlificID), data = data.l); anova(fiMo, fiMoC)
fiMoC <- lmerTest::lmer(Conventionality ~ ScenarioType / AttributedCue * Belief + (1|ProlificID), data = data.l); anova(fiMo, fiMoC)

library(report)
report(fiMoC)

################################################################################
# Legend: 
# Reality / Simulation = True / False (OBJECTIVE CONDITION)
# Neutral / Negative Emotion = No / Possible Discrepancy 
# DV: Arousal / Valence / Feeling of Control = Conventionality / Objectivity / 
# Permissibility
# Assessment: "I agreed with the description" = Belief (SUBJECTIVE CONDITION)
