#### Preamble ####
# Purpose: Clean and prepare the 2018 National Graduates Survey data for Shiny App
# Author: Yunkyung Park
# Data: 24 April 2022
# Contact: clara.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2018 National Graduates Survey and 
# run scripts/ngs_cleaning.R
# - Don't forget to gitignore it!


#### Workspace set-up ####
library(tidyverse)

# load the dataset
ngs<-read_csv('outputs/data/ngs2018.csv', show_col_types = FALSE)

# choose the columns that will be needed in creating a shiny app
ngs_shiny <-
  ngs %>%
  select(age_at_grd, 
         lev_of_study, 
         program, 
         job_related_to_program, 
         debt_loan_at_grd, 
         time_until_first_job, 
         gender,
         participate_co_op,
         work_placement_during_program) %>%
  mutate(work_experience = 
           case_when(participate_co_op == "Yes" | work_placement_during_program == "Yes" ~ "Yes",
                     participate_co_op == "No" & work_placement_during_program == "No" ~ "No"),
         work_experience = 
           factor(work_experience,
                  levels = c("Yes",
                             "No")))

ngs_shiny <-
  ngs_shiny %>%
  mutate(age_at_grd0 = 
           case_when(age_at_grd == "Less than 25" & work_experience == "Yes" ~ "Yes",
                     age_at_grd == "Less than 25" & work_experience == "No" ~ "No"),
         age_at_grd1 = 
           case_when(age_at_grd == "25 to 29" & work_experience == "Yes" ~ "Yes",
                     age_at_grd == "25 to 29" & work_experience == "No" ~ "No"),
         age_at_grd2 = 
           case_when(age_at_grd == "30 to 39" & work_experience == "Yes" ~ "Yes",
                     age_at_grd == "30 to 39" & work_experience == "No" ~ "No"),
         age_at_grd3 = 
           case_when(age_at_grd == "40 or more" & work_experience == "Yes" ~ "Yes",
                     age_at_grd == "40 or more" & work_experience == "No" ~ "No"),
         lev_of_study0 = 
           case_when(lev_of_study == "Bachelor's" & work_experience == "Yes" ~ "Yes",
                     lev_of_study == "Bachelor's" & work_experience == "No" ~ "No"),
         lev_of_study1 = 
           case_when(lev_of_study == "College" & work_experience == "Yes" ~ "Yes",
                     lev_of_study == "College" & work_experience == "No" ~ "No"),
         lev_of_study2 = 
           case_when(lev_of_study == "Master's / Doctorate" & work_experience == "Yes" ~ "Yes",
                     lev_of_study == "Master's / Doctorate" & work_experience == "No" ~ "No"),
         program0 = 
           case_when(program == "Agriculture, natural resources and conservation" & work_experience == "Yes" ~ "Yes",
                     program == "Agriculture, natural resources and conservation" & work_experience == "No" ~ "No"),
         program1 = 
           case_when(program == "Architecture, engineering, and related technologies" & work_experience == "Yes" ~ "Yes",
                     program == "Architecture, engineering, and related technologies" & work_experience == "No" ~ "No"),
         program2 = 
           case_when(program == "Business, management and public administration" & work_experience == "Yes" ~ "Yes",
                     program == "Business, management and public administration" & work_experience == "No" ~ "No"),
         program3 = 
           case_when(program == "Education" & work_experience == "Yes" ~ "Yes",
                     program == "Education" & work_experience == "No" ~ "No"),
         program4 = 
           case_when(program == "Humanities" & work_experience == "Yes" ~ "Yes",
                     program == "Humanities" & work_experience == "No" ~ "No"),
         program5 = 
           case_when(program == "Mathematics, computer and information sciences" & work_experience == "Yes" ~ "Yes",
                     program == "Mathematics, computer and information sciences" & work_experience == "No" ~ "No"),
         program6 = 
           case_when(program == "Physical and life sciences and technologies" & work_experience == "Yes" ~ "Yes",
                     program == "Physical and life sciences and technologies" & work_experience == "No" ~ "No"),
         program7 = 
           case_when(program == "Social and behavioural sciences and law" & work_experience == "Yes" ~ "Yes",
                     program == "Social and behavioural sciences and law" & work_experience == "No" ~ "No"),
         program8 = 
           case_when(program == "Visual and performing arts, and communications technologies" & work_experience == "Yes" ~ "Yes",
                     program == "Visual and performing arts, and communications technologies" & work_experience == "No" ~ "No"),
         program9 = 
           case_when(program == "Other" & work_experience == "Yes" ~ "Yes",
                     program == "Other" & work_experience == "No" ~ "No"),
         job_related_to_program0 = 
           case_when(job_related_to_program == "Closely related" & work_experience == "Yes" ~ "Yes",
                     job_related_to_program == "Closely related" & work_experience == "No" ~ "No"),
         job_related_to_program1 = 
           case_when(job_related_to_program == "Somewhat related" & work_experience == "Yes" ~ "Yes",
                     job_related_to_program == "Somewhat related" & work_experience == "No" ~ "No"),
         job_related_to_program2 = 
           case_when(job_related_to_program == "Not at all related" & work_experience == "Yes" ~ "Yes",
                     job_related_to_program == "Not at all related" & work_experience == "No" ~ "No"),
         debt_loan_at_grd0 = 
           case_when(debt_loan_at_grd == "$0" & work_experience == "Yes" ~ "Yes",
                     debt_loan_at_grd == "$0" & work_experience == "No" ~ "No"),
         debt_loan_at_grd1 = 
           case_when(debt_loan_at_grd == "Less than $5,000" & work_experience == "Yes" ~ "Yes",
                     debt_loan_at_grd == "Less than $5,000" & work_experience == "No" ~ "No"),
         debt_loan_at_grd2 = 
           case_when(debt_loan_at_grd == "$5,000 to less than $10,000" & work_experience == "Yes" ~ "Yes",
                     debt_loan_at_grd == "$5,000 to less than $10,000" & work_experience == "No" ~ "No"),
         debt_loan_at_grd3 = 
           case_when(debt_loan_at_grd == "$10,000 to less than $25,000" & work_experience == "Yes" ~ "Yes",
                     debt_loan_at_grd == "$10,000 to less than $25,000" & work_experience == "No" ~ "No"),
         debt_loan_at_grd4 = 
           case_when(debt_loan_at_grd == "$25,000 or more" & work_experience == "Yes" ~ "Yes",
                     debt_loan_at_grd == "$25,000 or more" & work_experience == "No" ~ "No"),
         time_until_first_job0 = 
           case_when(time_until_first_job == "Already working at a job or business" & work_experience == "Yes" ~ "Yes",
                     time_until_first_job == "Already working at a job or business" & work_experience == "No" ~ "No"),
         time_until_first_job1 = 
           case_when(time_until_first_job == "Less than 6 monthss" & work_experience == "Yes" ~ "Yes",
                     time_until_first_job == "Less than 6 months" & work_experience == "No" ~ "No"),
         time_until_first_job2 = 
           case_when(time_until_first_job == "6 months to less than 12 months" & work_experience == "Yes" ~ "Yes",
                     time_until_first_job == "6 months to less than 12 months" & work_experience == "No" ~ "No"),
         time_until_first_job3 = 
           case_when(time_until_first_job == "1 year to less than 2 years" & work_experience == "Yes" ~ "Yes",
                     time_until_first_job == "1 year to less than 2 years" & work_experience == "No" ~ "No"),
         time_until_first_job4 = 
           case_when(time_until_first_job == "2 years or more" & work_experience == "Yes" ~ "Yes",
                     time_until_first_job == "2 years or more" & work_experience == "No" ~ "No")
  ) %>%
  select(-c(age_at_grd, 
            lev_of_study, 
            program, 
            job_related_to_program, 
            debt_loan_at_grd, 
            time_until_first_job, 
            gender,
            participate_co_op,
            work_placement_during_program,
            work_experience))

write_csv(ngs_shiny, "shiny/data/ngs2018.csv")
