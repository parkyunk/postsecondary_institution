#### Preamble ####
# Purpose: Simulate some data that could resemble the 2018 National Graduates Survey dataset.
# Author: Yunkyung Park
# Data: 26 April 2022
# Contact: clara.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Don't forget to gitignore it!

#### Workspace set-up ####
library(janitor)
library(tidyverse)
library(ggplot2)

#### Simulate data ####
set.seed(304)

simulated_ngs <-
  tibble(
    # Unique identifier
    'caseID' = 1:19564,
    # Randomly choose one of the given options, with replacement, 19,564 times
    ## age_at_grd
    'Age at graduation' = sample(
      x = c(
        'Less than 25',
        "25 to 29",
        "30 to 39",
        "40 or more"
      ),
      size = 19564,
      replace = TRUE
    ),
    ## gender
    'Gender' = sample(
      x = c(
        'Male',
        'Female'
      ),
      size = 19564,
      replace = TRUE
    ),
    ## lev_of_study
    'Level of study' = sample(
      x = c("Bachelor's",
            "College",
            "Master's / Doctorate"
            ),
      size = 19564,
      replace = TRUE
    ),
    ## time_until_first_job
    'Time until first job' = sample(
      x = c("Already working at a job or business",
            "Less than 6 months",
            "6 months to less than 12 months",
            "1 year to less than 2 years",
            "2 years or more"
      ),
      size = 19564,
      replace = TRUE
    ),
    ## program
    'Program' = sample(
      x = c('Agriculture, natural resources and conservation',
            'Architecture, engineering, and related technologies',
            'Business, management and public administration',
            'Mathematics, computer and information sciences',
            'Physical and life sciences and technologies',
            'Social and behavioural sciences and law',
            'Visual and performing arts, and communications technologies',
            'Other'),
      size = 19564,
      replace = TRUE
    ),
    ## participate_co_op
    'Participate Co-Op' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
      ),
    ## work_placement_during_program
    'Work-placement during program' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## entrepreneurial_completed_courses
    'Completed entrepreneurial course' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## worked_during_program
    'Worked during program' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## volunteered_during_program
    'Volunteered during program' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## outside_canada_included_program
    'Outside Canada opportunity included in program' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_resp
    'Sources of funding-resp' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_govt_grants
    'Sources of funding-govt grants' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_non_govt_grants
    'Sources of funding-nongovt grants' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_scholarships
    'Sources of funding-scholarships' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_earnings
    'Sources of funding-earnings' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_ra_ta
    'Sources of funding-ra/ta' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_parents
    'Sources of funding-parents' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_bank_loans
    'Sources of funding-bank loans' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_credit_cards
    'Sources of funding-credit cards' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## sources_of_funding_employer
    'Sources of funding-employer' = sample(
      x = c('Yes',
            'No'),
      size = 19564,
      replace = TRUE
    ),
    ## debt_loan_at_grd
    'Debt Loan at graduation' = sample(
      x = c("$0",
            "Less than $5,000",
            "$5,000 to less than $10,000",
            "$10,000 to less than $25,000",
            "$25,000 or more"),
      size = 19564,
      replace = TRUE
    ),
    ## job_related_to_program
    'Job related to program' = sample(
      x = c("Closely related",
            "Somewhat related",
            "Not at all related"),
      size = 19564,
      replace = TRUE
    )
  )
