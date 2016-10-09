import xlrd
import csv

def csv_from_excel():

   wb = xlrd.open_workbook('/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales/2014_manhattan copy.xls')
   sh = wb.sheet_by_name('Manhattan')
   your_csv_file = open('your_csv_file.csv', 'wb')
   wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)

   for rownum in xrange(sh.nrows):
       wr.writerow(sh.row_values(rownum))

   your_csv_file.close()
