install.packages("tidyverse")
install.packages("psyntur")
install.packages("plyr")
install.packages("emmeans")
install.packages("modelr")
install.packages("afex")
install.packages("lavaan")
install.packages("lavaanPlot")
install.packages("readxl")
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")


library(tidyverse)
library(psyntur)
library(plyr)
library(emmeans)
library(modelr)
library(afex)
library(lavaan)
library(lavaanPlot)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)


replace_with_na <- function(variable, value){
  plyr::mapvalues(variable, from = value, to = NA)
}

normalize_variable <- function(variable){
  scale(variable)[,1]
}

IAT_raw_df <- read_csv("~/Desktop/IAT_Data_Revised/IAT_data_imported.csv")

IAT_clean_df <- IAT_raw_df %>% 
  mutate(Gender = replace_with_na(Gender, 2),
         Age_grp = replace_with_na(Age_grp, 9),
         across(Level_study:Discipline, ~replace_with_na(., 7)),
         across(stay_online:feel_depressed, ~replace_with_na(., 99)),
         across(headache:easily_tired, ~replace_with_na(., 7)))

IAT_clean_df <- mutate(IAT_clean_df,
                       # iun stands for "internet usage normalized"
                       iun = normalize_variable(Internet_usg),
                       # mhn stands for "mental health normalized"
                       mhn = normalize_variable(SQR_Total)
)

IAT_clean_df <- mutate(IAT_clean_df, across(Gender:Discipline, factor))

saveRDS(IAT_clean_df, file = 'IAT_clean.Rds')

view(IAT_clean_df)

#replacing missing values
IAT_clean_df$Level_study <- replace_with_na(IAT_clean_df$Level_study, value = 7)
IAT_clean_df$Yr_study <- replace_with_na(IAT_clean_df$Yr_study, value = 7)
IAT_clean_df$Discipline <- replace_with_na(IAT_clean_df$Discipline, value = 7)

IAT_clean_df$stay_online <- replace_with_na(IAT_clean_df$stay_online, value = 99)
IAT_clean_df$neglect_chores <- replace_with_na(IAT_clean_df$neglect_chores, value = 99)
IAT_clean_df$excitement <- replace_with_na(IAT_clean_df$excitement, value = 99)
IAT_clean_df$relationships <- replace_with_na(IAT_clean_df$relationships, value = 99)
IAT_clean_df$life_complaint <- replace_with_na(IAT_clean_df$life_complaint, value = 99)
IAT_clean_df$school_work <- replace_with_na(IAT_clean_df$school_work, value = 99)
IAT_clean_df$email_socialmedia <- replace_with_na(IAT_clean_df$email_socialmedia, value = 99)
IAT_clean_df$job_performance <- replace_with_na(IAT_clean_df$job_performance, value = 99)
IAT_clean_df$defensive_secretive <- replace_with_na(IAT_clean_df$defensive_secretive, value = 99)
IAT_clean_df$disturbing_thoughts <- replace_with_na(IAT_clean_df$disturbing_thoughts, value = 99)
IAT_clean_df$online_anticipation <- replace_with_na(IAT_clean_df$online_anticipation, value = 99)
IAT_clean_df$life_no_internet <- replace_with_na(IAT_clean_df$life_no_internet, value = 99)
IAT_clean_df$act_annoyed <- replace_with_na(IAT_clean_df$act_annoyed, value = 99)
IAT_clean_df$late_night_logins <- replace_with_na(IAT_clean_df$late_night_logins, value = 99)
IAT_clean_df$feel_preoccupied <- replace_with_na(IAT_clean_df$feel_preoccupied, value = 99)
IAT_clean_df$online_glued <- replace_with_na(IAT_clean_df$online_glued, value = 99)
IAT_clean_df$time_cutdown <- replace_with_na(IAT_clean_df$time_cutdown, value = 99)
IAT_clean_df$hide_online <- replace_with_na(IAT_clean_df$hide_online, value = 99)
IAT_clean_df$more_online_time <- replace_with_na(IAT_clean_df$more_online_time, value = 99)
IAT_clean_df$feel_depressed <- replace_with_na(IAT_clean_df$feel_depressed, value = 99)

IAT_clean_df$headache <- replace_with_na(IAT_clean_df$headache, value = 7)
IAT_clean_df$appetite <- replace_with_na(IAT_clean_df$appetite, value = 7)
IAT_clean_df$sleep <- replace_with_na(IAT_clean_df$sleep, value = 7)
IAT_clean_df$fear <- replace_with_na(IAT_clean_df$fear, value = 7)
IAT_clean_df$shaking <- replace_with_na(IAT_clean_df$shaking, value = 7)
IAT_clean_df$nervous <- replace_with_na(IAT_clean_df$nervous, value = 7)
IAT_clean_df$digestion <- replace_with_na(IAT_clean_df$digestion, value = 7)
IAT_clean_df$troubled <- replace_with_na(IAT_clean_df$troubled, value = 7)
IAT_clean_df$unhappy <- replace_with_na(IAT_clean_df$unhappy, value = 7)
IAT_clean_df$cry <- replace_with_na(IAT_clean_df$cry, value = 7)
IAT_clean_df$enjoyment <- replace_with_na(IAT_clean_df$enjoyment, value = 7)
IAT_clean_df$decisions <- replace_with_na(IAT_clean_df$decisions, value = 7)
IAT_clean_df$work <- replace_with_na(IAT_clean_df$work, value = 7)
IAT_clean_df$play <- replace_with_na(IAT_clean_df$play, value = 7)
IAT_clean_df$interest <- replace_with_na(IAT_clean_df$interest, value = 7)
IAT_clean_df$worthless <- replace_with_na(IAT_clean_df$worthless, value = 7)
IAT_clean_df$suicide <- replace_with_na(IAT_clean_df$suicide, value = 7)
IAT_clean_df$tiredness <- replace_with_na(IAT_clean_df$tiredness, value = 7)
IAT_clean_df$uncomfortable <- replace_with_na(IAT_clean_df$uncomfortable, value = 7)
IAT_clean_df$easily_tired <- replace_with_na(IAT_clean_df$easily_tired, value = 7)
                                           
view(IAT_clean_df)                                          
              
                                           





