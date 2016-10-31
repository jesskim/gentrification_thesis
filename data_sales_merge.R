install.packages("gdata")
library(gdata)
library(readr)

setwd("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales")
list.files("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales")

mastfiles <- list()
for file in files {
  file1 <- read.xls("1_manhattan.xls", pattern = "BOROUGH")
  mastfiles <- mastfiles[file1]
}

nysls_2011 <- read.xls("2011_manhattan.xls", pattern = "BOROUGH")
nysls_2012 <- read.xls("2012_manhattan.xls", pattern = "BOROUGH")
nysls_2013 <- read.xls("2013_manhattan.xls", pattern = "BOROUGH")
nysls_2014 <- read.xls("2014_manhattan.xls", pattern = "BOROUGH")