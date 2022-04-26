#### Preamble ####
# Purpose: Make interactive web applications using the 2018 National Graduates Survey data
# Author: Yunkyung Park
# Date: 26 April 2022
# Contact: clara.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2018 National Graduates Survey and 
# run shiny/data/01-clean_and_prepare_data_for_shiny.R to prepare the data
# - Don't forget to gitignore it!

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Workspace set-up ####
library(shiny)
library(tidyverse)
library(ggplot2)

# load the dataset
ngs<-read_csv('data/ngs2018.csv', show_col_types = FALSE)

# Define UI for application that prints the characteristics of the dataset
ui <- fluidPage(
    titlePanel("National Graduates Survey 2018 (Class of 2015)"),
    
    sidebarLayout(
        sidebarPanel("Select an item from the drop down.",
                     selectInput("dropdown", label = "Characteristics", choices = list("AGE" = list("<25", "25-29", "30-39", ">=40"),
                                                                                       "EDUCATION" = list("Bachelor's",
                                                                                                          "College",
                                                                                                          "Master's / Doctorate"),
                                                                                       "PROGRAM" = list('Agriculture',
                                                                                                        'Architecture, engineering',
                                                                                                        'Business',
                                                                                                        'Education',
                                                                                                        'Humanities',
                                                                                                        'Mathematics, computer science',
                                                                                                        'Physical and life science',
                                                                                                        'Social science',
                                                                                                        'Visual and performing arts',
                                                                                                        'Other'),
                                                                                       "JOB RELATED TO THE PROGRAM" = list("Closely related",
                                                                                                                           "Somewhat related",
                                                                                                                           "Not at all"),
                                                                                       "DEBT LOAN" = list("None",
                                                                                                          "<$5,000",
                                                                                                          "$5,000-$10,000",
                                                                                                          "$10,000-$25,000",
                                                                                                          ">$25,000"),
                                                                                       "TIME TOOK TO FIND THE JOB AFTER GRADUATION" = list("Already working",
                                                                                                                                           "<6 months",
                                                                                                                                           "6 months - 12 months",
                                                                                                                                           "1 year - 2 years",
                                                                                                                                           ">2 years")
                     )
                     )
        ),
    mainPanel(
        tabsetPanel(
            tabPanel("Figure", plotOutput("figure", width="100%")), 
            tabPanel("About", verbatimTextOutput("about"))
        )
    )))

# Define server logic required to draw a bar plot
server <- function(input, output) {
    output$about <- renderText({
        paste("This website is created with data from the National Graduates Survey 2018 conducted on Class of 2015.",
              "It provides a more thorough understanding of the dataset with taking relevant work experience into account.",
              "However, note that only co-op and work placement are included, and the internship experience is excluded in plotting the figures.",
              sep = '\n')
              
    })

    output$figure <- renderPlot({
        if(input$dropdown == "<25"){choice <- subset(ngs$age_at_grd0, !is.na(ngs$age_at_grd0))}
        else if (input$dropdown == "25-29"){choice <- subset(ngs$age_at_grd1, !is.na(ngs$age_at_grd1))}
        else if (input$dropdown == "30-39"){choice <- subset(ngs$age_at_grd2, !is.na(ngs$age_at_grd2))}
        else if (input$dropdown == ">=40"){choice <- subset(ngs$age_at_grd3, !is.na(ngs$age_at_grd3))}
        
        else if (input$dropdown == "Bachelor's"){choice <- subset(ngs$lev_of_study0, !is.na(ngs$lev_of_study0))}
        else if (input$dropdown == "College"){choice <- subset(ngs$lev_of_study1, !is.na(ngs$lev_of_study1))}
        else if (input$dropdown == "Master's / Doctorate"){choice <- subset(ngs$lev_of_study2, !is.na(ngs$lev_of_study2))}
        
        else if (input$dropdown == "Agriculture"){choice <- subset(ngs$program0, !is.na(ngs$program0))}
        else if (input$dropdown == 'Architecture, engineering'){choice <- subset(ngs$program1, !is.na(ngs$program1))}
        else if (input$dropdown == "Business"){choice <- subset(ngs$program2, !is.na(ngs$program2))}
        else if (input$dropdown == 'Education'){choice <- subset(ngs$program3, !is.na(ngs$program3))}
        else if (input$dropdown == "Humanities"){choice <- subset(ngs$program4, !is.na(ngs$program4))}
        else if (input$dropdown == 'Mathematics, computer science'){choice <- subset(ngs$program5, !is.na(ngs$program5))}
        else if (input$dropdown == 'Physical and life science'){choice <- subset(ngs$program6, !is.na(ngs$program6))}
        else if (input$dropdown == 'Social science'){choice <- subset(ngs$program7, !is.na(ngs$program7))}
        else if (input$dropdown == 'Visual and performing arts'){choice <- subset(ngs$program8, !is.na(ngs$program8))}
        else if (input$dropdown == 'Other'){choice <- subset(ngs$program9, !is.na(ngs$program9))}
        
        else if (input$dropdown == "Closely related"){choice <- subset(ngs$job_related_to_program0, !is.na(ngs$job_related_to_program0))}
        else if (input$dropdown == "Somewhat related"){choice <- subset(ngs$job_related_to_program1, !is.na(ngs$job_related_to_program1))}
        else if (input$dropdown == "Not at all"){choice <- subset(ngs$job_related_to_program2, !is.na(ngs$job_related_to_program2))}
       
        else if (input$dropdown == "None"){choice <- subset(ngs$debt_loan_at_grd0, !is.na(ngs$debt_loan_at_grd0))}
        else if (input$dropdown == "<$5,000"){choice <- subset(ngs$debt_loan_at_grd1, !is.na(ngs$debt_loan_at_grd1))}
        else if (input$dropdown == "$5,000-$10,000"){choice <- subset(ngs$debt_loan_at_grd2, !is.na(ngs$debt_loan_at_grd2))}
        else if (input$dropdown == "$10,000-$25,000"){choice <- subset(ngs$debt_loan_at_grd3, !is.na(ngs$debt_loan_at_grd3))}
        else if (input$dropdown == ">$25,000"){choice <- subset(ngs$debt_loan_at_grd4, !is.na(ngs$debt_loan_at_grd4))}
        
        else if (input$dropdown == "Already working"){choice <- subset(ngs$time_until_first_job0, !is.na(ngs$time_until_first_job0))}
        else if (input$dropdown == "<6 months"){choice <- subset(ngs$time_until_first_job1, !is.na(ngs$time_until_first_job1))}
        else if (input$dropdown == "6 months - 12 months"){choice <- subset(ngs$time_until_first_job2, !is.na(ngs$time_until_first_job2))}
        else if (input$dropdown == "1 year - 2 years"){choice <- subset(ngs$time_until_first_job3, !is.na(ngs$time_until_first_job3))}
        else if (input$dropdown == ">2 years"){choice <- subset(ngs$time_until_first_job4, !is.na(ngs$time_until_first_job4))}
        
        ggplot(mapping=aes(x=choice, fill=choice)) +
            geom_bar() +
            scale_x_discrete(limits = c("Yes", "No")) + 
            theme_minimal() +
            labs(x='Work experience', 
                 y='Number of respondents', 
                 fill='') +
            theme(legend.position='none')
            
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
