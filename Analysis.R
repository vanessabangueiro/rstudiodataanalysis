IAT_clean_df <- read.csv("~/Desktop/IAT_Data_Revised/IAT_data_imported.csv")
view (IAT_clean_df)

# Descriptive statistics -------------------------------------------------------
summary (IAT_clean_df)
# boxplot for internet usage by Gender
boxplot(Internet_usg ~ Gender, data = IAT_clean_df, main = "Internet Usage by Gender", xlab = "Gender", ylab = "Internet Usage")

#boxplot for internet add by Gender
boxplot2(Internet_Add ~ Gender, data = IAT_clean_df, main = "Internet Add by Gender", xlab = "Gender", ylab = "Internet Add")

# boxplot for SQR scores by Gender
boxplot3(SQR_Total ~ Gender, data = IAT_clean_df, main = "SQR scores by Gender", xlab = "Gender", ylab = "SQR_Total")


# Linear regression models ------------------------------------------------------------
Y = b0 + b1*X1 + b2*X2 + ... + bn*Xn + ϵ
# SQR as outcome variable predicting internet addiction variables 
model1 <- lm(SQR_Total ~ stay_online + neglect_chores + excitement + relationships + life_complaint + school_work + email_socialmedia + job_performance + defensive_secretive + disturbing_thoughts +
               online_anticipation + life_no_internet + act_annoyed + late_night_logins + feel_preoccupied + online_glued + time_cutdown + hide_online + more_online_time + feel_depressed,
             data = IAT_clean_df)
summary(model1)
confint(model1)
# internet addiction as outcome variable predicting SQR variables
model2 <- lm(Internet_Add ~ headache + appetite + sleep + fear + shaking + nervous + digestion + troubled + unhappy + cry + enjoyment + decisions + work + play + interest + worthless + suicide + tiredness + uncomfortable + easily_tired,
             data = IAT_clean_df)
summary(model2)
confint(model2)


# Bayesian models ---------------------------------------------------------------
install.packages("brms")
library(brms)
install.packages("lme4")
library(lme4)
formula <- "P(A | B) = P(B | A) * P(A) / P(B)"
print(formula)
# Define the Bayesian model with all 20 predictors
# SQR as outcome variable predicting internet addiction variables 
bayesian_model1 <- brm(
  formula = SQR_Total ~ stay_online + neglect_chores + excitement + 
    relationships + life_complaint + school_work + email_socialmedia + 
    job_performance + defensive_secretive + disturbing_thoughts + 
    online_anticipation + life_no_internet + act_annoyed + 
    late_night_logins + feel_preoccupied + online_glued + 
    time_cutdown + hide_online + more_online_time + feel_depressed,
  data = IAT_clean_df,
  family = gaussian(),  # Specify the likelihood function (e.g., 'gaussian' for linear regression)
  prior = c(prior(normal(0, 10), class = Intercept),  # Prior for intercept
            prior(normal(0, 5), class = b)),          # Prior for slopes
  chains = 4,           # Number of Markov chains
  iter = 2000           # Number of iterations per chain
)
bayesian_model1
fixef(bayesian_model1)
# internet addiction as outcome variable predicting SQR variables
bayesian_model2 <- brm(
  formula = Internet_Add ~ headache + appetite + sleep + fear + shaking + nervous 
  + digestion + troubled + unhappy + cry + enjoyment + decisions + work + play + 
    interest + worthless + suicide + tiredness + uncomfortable + easily_tired,
  data = IAT_clean_df,
  family = gaussian(),  # Specify the likelihood function (e.g., 'gaussian' for linear regression)
  prior = c(prior(normal(0, 10), class = Intercept),  # Prior for intercept
            prior(normal(0, 5), class = b)),          # Prior for slopes
  chains = 4,           # Number of Markov chains
  iter = 2000           # Number of iterations per chain
)
bayesian_model2
fixef(bayesian_model2)

# Exploratory factor analysis --------------------------------------------------
install.packages("psyntur")
library(psyntur)
install.packages("psych", dependencies = TRUE)
library(psych)
install.packages("devtools")
library(devtools)
devtools::install_github("mark-andrews/psyntur")
# Exploratory factor analysis for SQR and internet addiction variables
variables_of_interest <- c("SQR_Total", "stay_online", "neglect_chores", "excitement", "relationships", 
                           "life_complaint", "school_work", "email_socialmedia", "job_performance", 
                           "defensive_secretive", "disturbing_thoughts", "online_anticipation", 
                           "life_no_internet", "act_annoyed", "late_night_logins", "feel_preoccupied", 
                           "online_glued", "time_cutdown", "hide_online", "more_online_time", "feel_depressed")
data_subset <- IAT_clean_df[, variables_of_interest]
efa_result <- psych::fa(data_subset, nfactors = 3, rotate = "varimax")
print(efa_result)
summary(efa_result)
# Extract factor loadings
factor_loadings <- efa_result$loadings

# Regression model -------------------------------------------------------------
# Extract factor scores
factor_scores <- as.data.frame(efa_result$scores)
# combine factor scores with the outcome variable
IAT_clean_df <- cbind(data_subset, factor_scores)
# fit the regression model
regression_model1 <- lm(SQR_Total ~ MR1 + MR2 + MR3 , data = IAT_clean_df)
# view the regression results
summary(regression_model1)

