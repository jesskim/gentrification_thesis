# gentrification_thesis
This repo includes scripts I used to complete my thesis in the MA, Quantitative Methods in the Social Sciences program at Columbia University. Unless stated otherwise, I created these codes.

## File: thesis_wrangling.R
Cleans and merges the following datasets:

1. Annualized Sales Update, NYC Department of Finance (2003 to 2014)  
2. United Hospital Fund Neighborhoods and Zip codes  
3. NYPD Historical Crime Data  
4. NYCDOHMH Community Health Survey (2003 to 2014)  

## File: data_sum.rb
Created by Jordan Holliday (https://github.com/jmhol9) on 10/8/2016.

Converts .xls files on NYC property sales to .csv files and then merges them into a master data file. The master file includes total sales by NYC neighborhood, zip code, and year. This will be merged with NYPD Historical Crime Data to complete my Master's thesis.

### To run: 

1. In terminal, go to folder with .xls files 
2. Run "convert2csv" (no quotes) to convert .xls files to .csv files and concatenate into all_data.csv
3. Run ruby "data_sum.rb"

*Raw files were downloaded here: https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page
