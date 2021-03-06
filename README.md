# What can you expect from the post-secondary institution?

## Overview

This repository contains the full source code for the paper "What can you expect from the post-secondary institution?". This paper uses data gathered from the 2018 National Graduates Survey (NGS) which was conducted on the 2015 graduates of public postsecondary educational institutions across Canada. It examines the experiences during the program and life after graduation. 

## Repo Structure

- 'inputs' folder contains an empty folder 'data' since the NGS data is not permitted to be shared to public. Instead, the steps to obtain the data are indicated below.
- 'scripts' folder contains two scripts; 00-simulation.R which simulates the 2018 NGS dataset and 01-clean_and_prepare_data.R which takes raw data from 'inputs/data' as an input and outputs cleaned data into 'outputs/data'.
- 'outputs' folder contains the files necessary to reproduce this paper as well as the pdf copy of the paper. It includes the rmd file which is needed to reproduce it and a reference bib file containing the references used for the paper.
- 'shiny' folder contains two scripts; 01-clean_and_prepare_data_for_shiny.R which takes cleaned data from 'outputs/data' as an input and processes data to create a shiny app and app.R which makes interactive web applications using the R package 'shiny'.

## Steps to access 2018 National Grauates Survey

1. Navigate to http://www.chass.utoronto.ca/
2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
3. Click SDA @ CHASS, should redirect to sign in
4. Sign in and continue in English 
5. Crtl+F NGS, click
6. Click "Data" on 'National survey of 2015 graduates, 2018'
7. Click download
8. Select CSV data file, data definitions for STATA
9. Select all variables by clicking button next to green colored "All". Then continue
10. Create the files, download and save
11. Scroll down the STATA file, and copy and paste the STATA definitions text to create a ngs_dict.txt file
13. Rename the .csv file raw_ngs.csv and the .dta file ngs_label.txt
14. Place the files in the inputs/data folder of this directory
