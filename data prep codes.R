library(tidyverse)

# Gender =  0= female ,1= male , 2= prefer not to say
# `Age_grp` 0 = 15-19 , 1= 20=24 , 2 =25-29, 3 =30-39, 4 =35-39 , 5 =40 + , 9 = missing
# `Level_study` 0= undergraduate , 1= postgraduate 7 = missing
# `Yr_study` 0 = year 1, 1=  year 2, 2=  year 3 , 3 =  year 4+ , 7 = missing
# `Discipline` 0= science 1= humanities and social science 7 = missing
# Variables `stay_online` to `feel_depressed` 0= does not apply, 1= rarely , 2= occasionally , 3=
# frequently,  4= often,  5= always , 99= missing
# The variables `headache` to `easily_tired`: 0= yes, 1= no,  7= missing
# Filter_$ = SQR_catg1: 0= yes  1= no 7= missing
# SQR_total = total score of SQR - 20
# New_SQR_total = SQR -20 summarised total items
# SQR_catg = categorised SQR-total score
# 0= non-existence of a problematic mental disorder </ =7
# 1= existence of a mental disorder >/= 8



# User defined functions --------------------------------------------------

# for replacing values with NA
replace_with_na <- function(variable, value){
  plyr::mapvalues(variable, from = value, to = NA)
}

# for normalizing variables, i.e. so they have mean of 0 and sd of 1
normalize_variable <- function(variable){
  scale(variable)[,1]
}


# Read in data ------------------------------------------------------------

IAT_raw_df <- read_csv("IAT_data_imported.csv")

# Replace missing values --------------------------------------------------

# NOTE: More to do here probably ....
# replace the 2 with NA in the `Gender` variable
# replace the value `9` with NA in the `Age_grp` variable
# and replace `7` with NA in `Level_study`, `Yr_study`, `Discipline`
# and replace 99 with NA in any internet usage item
# and replace 7 with NA in any SQR item
IAT_clean_df <- IAT_raw_df %>% 
  mutate(Gender = replace_with_na(Gender, 2),
         Age_grp = replace_with_na(Age_grp, 9),
         across(Level_study:Discipline, ~replace_with_na(., 7)),
         across(stay_online:feel_depressed, ~replace_with_na(., 99)),
         across(headache:easily_tired, ~replace_with_na(., 7)))


# Rescale some variables --------------------------------------------------

IAT_clean_df <- mutate(IAT_clean_df,
                       # iun stands for "internet usage normalized"
                       iun = normalize_variable(Internet_usg),
                       # mhn stands for "mental health normalized"
                       mhn = normalize_variable(SQR_Total)
)


# Recode the data type in some variables ----------------------------------

# Indicate that some variables are categorical using the `factor` function. For
# example, `Gender` is 0,1 and 2 but these are not numbers per se, they are
# labels. Same for `Age_grp`, `Level_study`, `Yr_study`, `Discipline`
IAT_clean_df <- mutate(IAT_clean_df, across(Gender:Discipline, factor))

# Save cleaned data file as Rds -------------------------------------------

saveRDS(IAT_clean_df, file = 'IAT_clean.Rds')