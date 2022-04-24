#### Preamble ####
# Purpose: Clean and prepare the 2018 National Graduates Survey data for Shiny App
# Author: Yunkyung Park
# Data: 27 April 2022
# Contact: clara.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2018 National Graduates Survey and 
# run scripts/ngs_cleaning.R
# - Don't forget to gitignore it!


#### Workspace set-up ####
library(tidyverse)

# load the dataset
ngs<-read_csv('../outputs/data/ngs2018.csv', show_col_types = FALSE)

# choose the columns that will be needed in creating a shiny app
ngs_shiny <-
  ngs %>%
  select(age_at_grd, 
         lev_of_study, 
         program, 
         job_related_to_program, 
         debt_loan_at_grd, 
         time_until_first_job, 
         gender)

ngs_shiny <-
  ngs_shiny %>%
  mutate(age_at_grd0 = 
           case_when(age_at_grd == "Less than 25" & gender == "Male" ~ "Male",
                     age_at_grd == "Less than 25" & gender == "Female" ~ "Female"),
         age_at_grd1 = 
           case_when(age_at_grd == "25 to 29" & gender == "Male" ~ "Male",
                     age_at_grd == "25 to 29" & gender == "Female" ~ "Female"),
         age_at_grd2 = 
           case_when(age_at_grd == "30 to 39" & gender == "Male" ~ "Male",
                     age_at_grd == "30 to 39" & gender == "Female" ~ "Female"),
         age_at_grd3 = 
           case_when(age_at_grd == "40 or more" & gender == "Male" ~ "Male",
                     age_at_grd == "40 or more" & gender == "Female" ~ "Female"),
         lev_of_study0 = 
           case_when(lev_of_study == "Bachelor's" & gender == "Male" ~ "Male",
                     lev_of_study == "Bachelor's" & gender == "Female" ~ "Female"),
         lev_of_study1 = 
           case_when(lev_of_study == "College" & gender == "Male" ~ "Male",
                     lev_of_study == "College" & gender == "Female" ~ "Female"),
         lev_of_study2 = 
           case_when(lev_of_study == "Master's / Doctorate" & gender == "Male" ~ "Male",
                     lev_of_study == "Master's / Doctorate" & gender == "Female" ~ "Female"),
         program0 = 
           case_when(program == "Agriculture, natural resources and conservation" & gender == "Male" ~ "Male",
                     program == "Agriculture, natural resources and conservation" & gender == "Female" ~ "Female"),
         program1 = 
           case_when(program == "Architecture, engineering, and related technologies" & gender == "Male" ~ "Male",
                     program == "Architecture, engineering, and related technologies" & gender == "Female" ~ "Female"),
         program2 = 
           case_when(program == "Business, management and public administration" & gender == "Male" ~ "Male",
                     program == "Business, management and public administration" & gender == "Female" ~ "Female"),
         program3 = 
           case_when(program == "Education" & gender == "Male" ~ "Male",
                     program == "Education" & gender == "Female" ~ "Female"),
         program4 = 
           case_when(program == "Humanities" & gender == "Male" ~ "Male",
                     program == "Humanities" & gender == "Female" ~ "Female"),
         program5 = 
           case_when(program == "Mathematics, computer and information sciences" & gender == "Male" ~ "Male",
                     program == "Mathematics, computer and information sciences" & gender == "Female" ~ "Female"),
         program6 = 
           case_when(program == "Physical and life sciences and technologies" & gender == "Male" ~ "Male",
                     program == "Physical and life sciences and technologies" & gender == "Female" ~ "Female"),
         program7 = 
           case_when(program == "Social and behavioural sciences and law" & gender == "Male" ~ "Male",
                     program == "Social and behavioural sciences and law" & gender == "Female" ~ "Female"),
         program8 = 
           case_when(program == "Visual and performing arts, and communications technologies" & gender == "Male" ~ "Male",
                     program == "Visual and performing arts, and communications technologies" & gender == "Female" ~ "Female"),
         program9 = 
           case_when(program == "Other" & gender == "Male" ~ "Male",
                     program == "Other" & gender == "Female" ~ "Female"),
         job_related_to_program0 = 
           case_when(job_related_to_program == "Closely related" & gender == "Male" ~ "Male",
                     job_related_to_program == "Closely related" & gender == "Female" ~ "Female"),
         job_related_to_program1 = 
           case_when(job_related_to_program == "Somewhat related" & gender == "Male" ~ "Male",
                     job_related_to_program == "Somewhat related" & gender == "Female" ~ "Female"),
         job_related_to_program2 = 
           case_when(job_related_to_program == "Not at all related" & gender == "Male" ~ "Male",
                     job_related_to_program == "Not at all related" & gender == "Female" ~ "Female"),
         debt_loan_at_grd0 = 
           case_when(debt_loan_at_grd == "$0" & gender == "Male" ~ "Male",
                     debt_loan_at_grd == "$0" & gender == "Female" ~ "Female"),
         debt_loan_at_grd1 = 
           case_when(debt_loan_at_grd == "Less than $5,000" & gender == "Male" ~ "Male",
                     debt_loan_at_grd == "Less than $5,000" & gender == "Female" ~ "Female"),
         debt_loan_at_grd2 = 
           case_when(debt_loan_at_grd == "$5,000 to less than $10,000" & gender == "Male" ~ "Male",
                     debt_loan_at_grd == "$5,000 to less than $10,000" & gender == "Female" ~ "Female"),
         debt_loan_at_grd3 = 
           case_when(debt_loan_at_grd == "$10,000 to less than $25,000" & gender == "Male" ~ "Male",
                     debt_loan_at_grd == "$10,000 to less than $25,000" & gender == "Female" ~ "Female"),
         debt_loan_at_grd4 = 
           case_when(debt_loan_at_grd == "$25,000 or more" & gender == "Male" ~ "Male",
                     debt_loan_at_grd == "$25,000 or more" & gender == "Female" ~ "Female"),
         time_until_first_job0 = 
           case_when(time_until_first_job == "Already working at a job or business" & gender == "Male" ~ "Male",
                     time_until_first_job == "Already working at a job or business" & gender == "Female" ~ "Female"),
         time_until_first_job1 = 
           case_when(time_until_first_job == "Less than 6 monthss" & gender == "Male" ~ "Male",
                     time_until_first_job == "Less than 6 months" & gender == "Female" ~ "Female"),
         time_until_first_job2 = 
           case_when(time_until_first_job == "6 months to less than 12 months" & gender == "Male" ~ "Male",
                     time_until_first_job == "6 months to less than 12 months" & gender == "Female" ~ "Female"),
         time_until_first_job3 = 
           case_when(time_until_first_job == "1 year to less than 2 years" & gender == "Male" ~ "Male",
                     time_until_first_job == "1 year to less than 2 years" & gender == "Female" ~ "Female"),
         time_until_first_job4 = 
           case_when(time_until_first_job == "2 years or more" & gender == "Male" ~ "Male",
                     time_until_first_job == "2 years or more" & gender == "Female" ~ "Female")
  ) %>%
  select(-c(age_at_grd, 
            lev_of_study, 
            program, 
            job_related_to_program, 
            debt_loan_at_grd, 
            time_until_first_job, 
            gender))

write_csv(ngs_shiny, "shiny/data/ngs2018.csv")
