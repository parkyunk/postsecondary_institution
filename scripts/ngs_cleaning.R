#### Preamble ####
# Purpose: Clean and prepare the 2018 National Graduates Survey
# Author: Yunkyung Park
# Data: 27 April 2022
# Contact: clara.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2018 National Graduates Survey and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("inputs/data/ngs_data.csv")
dict <- read_lines("inputs/data/ngs_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("inputs/data/ngs_labels.txt")


# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case 
# when statement for the categorical variables
ngs <- raw_data %>% 
  select(aft_020, 
         aft_050,
         aft_070,
         aft_090,
         certlevp,
         coop,
         dbtalgrd,
         gradagep,
         lfcoccp,
         oweslgd,
         pgmcipap,
         pgm_p405,
         resp,
         scholar,
         stuloans,
         pgm_034,
         pgm_100,
         pgm_280a,
         pgm_280b,
         pgm_280c,
         pgm_280d,
         pgm_280e,
         pgm_280f,
         pgm_290,
         pgm_350,
         pgm_380,
         pgm_410,
         pgm_415,
         stl_080,
         stl_100a,
         stl_100b,
         stl_100c,
         stl_100d,
         stl_130,
         stl_150,
         stl_160b,
         stl_160c,
         stl_160d,
         stl_160e,
         stl_160f,
         stl_160g,
         stl_160h,
         stl_160i,
         stl_160j,
         stl_160l,
         stl_190,
         vr2_080) %>% 
  mutate_at(vars(aft_020:vr2_080), 
            .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(aft_020:vr2_080),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
ngs <- ngs %>% 
  clean_names() %>% 
  rename(
    time_until_first_job = aft_020, 
    full_job = aft_050,
    permanent_job = aft_070,
    job_related_to_program = aft_090,
    lev_of_study = certlevp,
    participate_co_op = coop,
    debt_loan_at_grd = dbtalgrd,
    age_at_grd = gradagep,
    occupation_gp = lfcoccp,
    debt_gov_sponsored_student_loan_at_grd = oweslgd,
    program = pgmcipap,
    gpa = pgm_p405,
    received_from_resp = resp,
    received_from_scholarship = scholar,
    received_gov_sponsored_student_loan = stuloans,
    student_status = pgm_034,
    work_placement_during_program = pgm_100,
    entrepreneurial_started_business = pgm_280a,
    entrepreneurial_completed_courses = pgm_280b,
    entrepreneurial_business_plan = pgm_280c,
    entrepreneurial_visited_centre = pgm_280d,
    entrepreneurial_worked_on_project = pgm_280e,
    entrepreneurial_none = pgm_280f,
    worked_during_program = pgm_290,
    volunteered_during_program = pgm_350,
    outside_canada_included_program = pgm_380,
    factor_choosing_institution = pgm_410,
    factor_choosing_program = pgm_415,
    received_govt_loan = stl_080,
    receieved_govt_loan_repayment_assistance = stl_100a,
    receieved_govt_loan_revision_terms = stl_100b,
    receieved_govt_loan_interest_only = stl_100c,
    receieved_govt_loan_other = stl_100d,
    govt_loan_pay_off_expected_date = stl_130,
    borrowed_from_financial_institution = stl_150,
    sources_of_funding_resp = stl_160b,
    sources_of_funding_govt_grants = stl_160c,
    sources_of_funding_non_govt_grants = stl_160d,
    sources_of_funding_scholarships = stl_160e,
    sources_of_funding_earnings = stl_160f,
    sources_of_funding_ra_ta = stl_160g,
    sources_of_funding_parents = stl_160h,
    sources_of_funding_bank_loans = stl_160i,
    sources_of_funding_credit_cards = stl_160j,
    sources_of_funding_employer = stl_160l,
    need_to_pay_back_money_borrowed_from_family = stl_190,
    gender = vr2_080)

#### Clean up ####
ngs <- ngs %>% 
  mutate_at(vars(time_until_first_job:gender), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .)))

write_csv(ngs, "outputs/ngs2018.csv")
