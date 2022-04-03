# Postsecondary Institution

## Repo Structure

- 'inputs' folder contains an empty folder 'data' since I do not have permission to share the NGS data. Instead, the steps to obtain the data are indicated below.
- 'scripts' folder contains a single script, 00_data_clean.R, which takes raw data from 'inputs/data' as an input and outputs cleaned data into 'inputs/data'.
- 'outputs' folder contains the files necessary to reproduce this paper. It includes the rmd file which is needed to reproduce it and a reference bib file containing the references used for the paper. In addition, it includes the pdf copy of the paper. 

## Steps to access 2018 National Grauates Survey

1. Navigate to http://www.chass.utoronto.ca/
2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
3. Click SDA @ CHASS, should redirect to sign in
4. Sign in and continue in English 
5. Crtl+F NGS, click
6. Click "Data" on the one you want. We used 2018, but you may want a different wave 
7. Click download
8. Select CSV data file, data definitions for STATA
9. Select all variables by clicking button next to green colored "All". Then continue
10. Create the files, download and save
11. Scroll down the STATA file, and copy and paste the STATA definitions text to create a ngs_dict.txt file
13. Rename the .csv file raw_ngs.csv and the .dta file ngs_label.txt
14. Place the files in the inputs/data folder of this directory
15. 
