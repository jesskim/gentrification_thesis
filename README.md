# gentrification_thesis

Created by Jordan Holliday (https://github.com/jmhol9) on 10/8/2016.

Converts .xls files on NYC property sales to .csv files and then merges them into a master data file. The master file includes total sales by NYC neighborhood, zip code, and year. This will be merged with NYPD Historical Crime Data to complete my Master's thesis.
## To run: 
1. In terminal, go to folder with .xls files 
2. Run "convert2csv" (no quotes) to convert .xls files to .csv files and concatenate into all_data.csv
3. Run ruby "data_sum.rb"

*Raw files were downloaded here: https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page
