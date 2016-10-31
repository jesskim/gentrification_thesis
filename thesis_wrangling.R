# Code to pre-process data for thesis
# Jessica Kim
# Fall 2016

############# MERGE CHS DATA ############# 
setwd("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_chs")
setwd("/Users/jsk2225/Downloads/data_chs")

# Read in all datasets using haven package
library(haven)
# library(sas7bdat)
chs2014 <- read_sas("chs2014_public.sas7bdat")
chs2013 <- read_sas("chs2013_public.sas7bdat")
chs2012 <- read_sas("chs2012_public.sas7bdat")
chs2011 <- read_sas("chs2011_public.sas7bdat")
chs2010 <- read_sas("chs2010_public.sas7bdat")
chs2009 <- read_sas("chs2009_public.sas7bdat")
chs2008 <- read_sas("chs2008_public.sas7bdat")
chs2007 <- read_sas("chs2007_public.sas7bdat")
chs2006 <- read_sas("chs2006_public.sas7bdat")
chs2005 <- read_sas("chs2005_public.sas7bdat")
chs2004 <- read_sas("chs2004_public.sas7bdat")
chs2003 <- read_sas("chs2003_public.sas7bdat")
# store the dfs in list for cleaning
chs_datalist <- list(chs2014, chs2013, chs2012, chs2011, chs2010, chs2009, chs2008, chs2007, chs2006, chs2005, chs2004, chs2003)
names(chs_datalist) <- c("chs2014", "chs2013", "chs2012", "chs2011", "chs2010", "chs2009", "chs2008", "chs2007", "chs2006", "chs2005", "chs2004", "chs2003")

# create year variable for each df
year <- c(2014:2003)
chs_datalist2 <- Map(cbind, chs_datalist, year = year)
chs_datalist2 <- lapply(chs_datalist2, function(x) {colnames(x) <- tolower(colnames(x)); x})

# subset dfs by relevant vars
library(dplyr)
chs_datalist2 <- lapply(chs_datalist2, dplyr::select, 
                        one_of("year", "agegroup", "sex", "newrace", "bthregion2", 
                               "usborn", "education", "demog17", "hhsize"), 
                        contains("strata", ignore.case = TRUE), 
                        starts_with("wt", ignore.case = TRUE), 
                        starts_with("neighpovgroup", ignore.case = TRUE), 
                        starts_with("athomelanguage", ignore.case = TRUE), 
                        starts_with("employment", ignore.case = TRUE))

# Remove numbers from suffix of var names for merging dfs later
chs_datalist2 <- lapply(chs_datalist2, function(x) {colnames(x) <- gsub("[[:digit:]]", "", colnames(x)); x})

# Remove suffix "_dual" from weight vars and "_" from vars suffixes
chs_datalist2 <- lapply(chs_datalist2, function(x) {colnames(x) <- gsub("_dual", "", colnames(x)); x})
chs_datalist2 <- lapply(chs_datalist2, function(x) {colnames(x) <- gsub("_", "", colnames(x)); x})

# check that var names match across all dfs
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[2]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[3]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[4]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[5]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[6]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[7]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[8]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[9]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[10]]))
setdiff(colnames(chs_datalist2[[1]]), colnames(chs_datalist2[[11]]))

# aggregate to the strata (neighborhood) level


# chs_data_all <- Reduce(function(...) merge(..., all = TRUE), chs_datalist2)



############# MAKE UHF NEIGHBORHOOD AND ZIP CODE MERGE FILE ############# 

library(dplyr)
# Bronx
"Crotona - Tremont" <- c(10453, 10457, 10460)
"Fordham - Bronx Park" <- c(10458, 10467, 10468)
"High Bridge - Morrisania" <- c(10451, 10452, 10456)
"Hunts Point - Mott Haven" <- c(10454, 10455, 10459, 10474)
"Kingsbridge - Riverdale" <- c(10463, 10471)
"Northeast Bronx" <- c(10466, 10469, 10470, 10475)
"Pelhem - Throgs Neck" <- c(10461, 10462,10464, 10465, 10472, 10473)
bronx_nh <- list(
  Crotona_Tremont = `Crotona - Tremont`, 
  Fordham_Bronx_Park = `Fordham - Bronx Park`, 
  High_Bridge_Morrisania = `High Bridge - Morrisania`, 
  Hunts_Point_Mott_Haven = `Hunts Point - Mott Haven`, 
  Kingsbridge_Riverdale = `Kingsbridge - Riverdale`, 
  Northeast_Bronx = `Northeast Bronx`, 
  Pelhem_Throgs_Neck = `Pelhem - Throgs Neck`)
Bronx <- as.data.frame(do.call(rbind, bronx_nh))
Bronx$uhf <- row.names(Bronx)
Bronx$borough <- "Bronx"
Bronx <- gather(Bronx, "uhf", "zip", 1:6)
Bronx[, 3] <- rm()

# Brooklyn
Brooklyn <- c("Bedford Stuyvesant - Crown Heights", "Bensonhurst - Bay Ridge", "Borough Park", "Canarsie - Flatlands", "Coney Island - Sheepshead Bay", "Downtown - Heights - Slope", "East Flatbush - Flatbush", "East New York", "Greenpoint", "Sunset Park", "Williamsburg - Bushwick")
"Bedford Stuyvesant - Crown Heights" <- c(11212, 11213, 11216, 11233, 11238)
"Bensonhurst - Bay Ridge" <- c(11209, 11214, 11228)
"Borough Park" <- c(11204, 11218, 11219, 11230)
"Canarsie - Flatlands" <- c(11234, 11236, 11239)
"Coney Island - Sheepshead Bay" <- c(11223, 11224, 11229, 11235)
"Downtown - Heights - Slope" <- c(11201, 11205, 11215, 11217, 11231)
"East Flatbush - Flatbush" <- c(11203, 11210, 11225, 11226)
"East New York" <- c(11207, 11208)
"Greenpoint" <- c(11211, 11222)
"Sunset Park" <- c(11220, 11232)
"Williamsburg - Bushwick" <- c(11206, 11221, 11237)

brooklyn_nh <- list(
  Bedford_Stuyvesant_Crown_Heights = `Bedford Stuyvesant - Crown Heights`,
  Bensonhurst_Bay_Ridge 			= `Bensonhurst - Bay Ridge`,
  Borough_Park 						= `Borough Park`,
  Canarsie_Flatlands 				= `Canarsie - Flatlands`,
  Coney_Island_Sheepshead_Bay 	= `Coney Island - Sheepshead Bay`,
  Downtown_Heights_Slope 		= `Downtown - Heights - Slope`,
  East_Flatbush_Flatbush 			= `East Flatbush - Flatbush`,
  East_New_York 					= `East New York`,
  Greenpoint 					= `Greenpoint`,
  Sunset_Park 						= `Sunset Park`,
  Williamsburg_Bushwick 			 = `Williamsburg - Bushwick`)

Brooklyn <- as.data.frame(do.call(rbind, brooklyn_nh))
Brooklyn$uhf <- row.names(Brooklyn)
Brooklyn$borough <- "Brooklyn"
Brooklyn <- gather(Brooklyn, "uhf", "zip", 1:5)
Brooklyn[, 3] <- rm()
Brooklyn <- Brooklyn[!duplicated(Brooklyn), ]

# Manhattan
"Central Harlem - Morningside Heights" <- c(10026, 10027, 10030, 10037, 10039)
"Chelsea - Clinton" <- c(10001, 10011, 10018, 10019, 10020, 10036)
"East Harlem" <- c(10029, 10035)
"Gramercy Park - Murray Hill" <- c(10010, 10016, 10017, 10022)
"Greenwich Village - Soho" <- c(10012, 10013, 10014)
"Lower Manhattan" <- c(10004, 10005, 10006, 10007, 10038, 10280)
"Union Square - Lower East Side" <- c(10002, 10003, 10009)
"Upper East Side" <- c(10021, 10028, 10044, 10128)
"Upper West Side" <- c(10023, 10024, 10025)
"Washington Heights - Inwood" <- c(10031, 10032, 10033, 10034, 10040)

Manhattan_nh <- list(
  Central_Harlem_Morningside_Height = `Central Harlem - Morningside Heights`,
  Chelsea_Clinton = `Chelsea - Clinton`,
  East_Harlem = `East Harlem`,
  Gramercy_Park_Murray_Hill = `Gramercy Park - Murray Hill`,
  Greenwich_Village_Soho = `Greenwich Village - Soho`,
  Lower_Manhattan = `Lower Manhattan`,
  Union_Square_Lower_East_Side = `Union Square - Lower East Side`,
  Upper_East_Side = `Upper East Side`,
  Upper_West_Side = `Upper West Side`,
  Washington_Heights_Inwood = `Washington Heights - Inwood`)

Manhattan <- as.data.frame(do.call(rbind, Manhattan_nh))
Manhattan$uhf <- row.names(Manhattan)
Manhattan$borough <- "Manhattan"
Manhattan <- gather(Manhattan, "uhf", "zip", 1:6)
Manhattan[, 3] <- rm()
Manhattan <- Manhattan[!duplicated(Manhattan), ]

# Queens
"Bayside - Little Neck" 	 <- c(11361, 11362, 11363, 11364)
"Flushing - Clearview" 	 	 <- c(11354, 11355, 11356, 11357, 11358, 11359, 11360)
"Fresh Meadows" 			 <- c(11365, 11366, 11367)
"Jamaica" 					 <- c(11412, 11423, 11432, 11433, 11434, 11435, 11436)
"Long Island City - Astoria" <- c(11101, 11102, 11103, 11104, 11105, 11106)
"Ridgewood - Forest Hills" 	 <- c(11374, 11375, 11379, 11385)
"Rockaway" 					 <- c(11691, 11692, 11693, 11694, 11695, 11697)
"Southeast Queens" 			 <- c(11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429)
"Southwest Queens"			 <- c(11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421)
"West Queens"				 <- c(11368, 11369, 11370, 11372, 11373, 11377, 11378)

Queens_nh <- list(
  Bayside_Little_Neck 	 = `Bayside - Little Neck`, 	
  Flushing_Clearview 	 	 = `Flushing - Clearview`, 	 	
  Fresh_Meadows 			 = `Fresh Meadows`, 			
  Jamaica 					 = `Jamaica`, 					
  Long_Island_City_Astoria = `Long Island City - Astoria`,
  Ridgewood_Forest_Hills 	 = `Ridgewood - Forest Hills`, 	
  Rockaway 					 = `Rockaway`, 					
  Southeast_Queens 			 = `Southeast Queens`, 			
  Southwest_Queens			 = `Southwest Queens`,			
  West_Queens				 = `West Queens`)		

Queens <- as.data.frame(do.call(rbind, Queens_nh))
Queens$uhf <- row.names(Queens)
Queens$borough <- "Queens"
Queens <- gather(Queens, "uhf", "zip", 1:9)
Queens[, 3] <- rm()
Queens <- Queens[!duplicated(Queens), ]

# Staten Island
"Port Richmond" <- c(10302, 10303, 10310)
"South Beach - Tottenville" <- c(10306, 10307, 10308, 10309, 10312)
"Stapleton - St. George" <- c(10301, 10304, 10305)
"Willowbrook" <- c(10314)

Staten_nh <- list(
  Port_Richmond = `Port Richmond`,
  South_Beach_Tottenville = `South Beach - Tottenville`,
  Stapleton_St_George = `Stapleton - St. George`,
  Willowbrook = `Willowbrook`)

Staten <- as.data.frame(do.call(rbind, Staten_nh))
Staten$uhf <- row.names(Staten)
Staten$borough <- "Staten"
Staten <- gather(Staten, "uhf", "zip", 1:5)
Staten[, 3] <- rm()
Staten <- Staten[!duplicated(Staten), ]

# Merge all boroughs zip code and UHF neighbrhood data
uhf_zips <- rbind(Bronx, Manhattan, Queens, Brooklyn, Staten)

############# MERGE SALES DATA ############# 

# ruby script already changed .xls files to .csv and merged for each year
library(readr)
data_sales <- read_csv("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales/sum-data-neighborhood.csv")


############# MERGE CRIME DATA ############# 






############# MERGE ALL DATA ############# 



############# IMPORTANT DATA NOTES ############# 


# 2002-04:
#   neighpovgroup4_2000
# 2005-09:
#   neighpovgroup4_0711
# 2010:
#   neighpovgroup4_0711
# 2011-2012:
#   neighpovgroup4_0711
# 2013:
#   neighpovgroup4_0812

## year prefix varies for employment var
# employment14 changes for each year
# 
#   2002-04: employment
#   2005: employment05
#   2006: employment06
#   2007: employment07
#   2008: employment08
#   2009: employment09
#   2010: employment10
#   2011: employment11
#   2012: employment12
#   2013: employment13
#   
#   The response order for Q8.13 differs from the order of responses in formatted frequencies.Employment12 was renumbered so that
#   response options would be consistent with order in prior years
#   
#   1=Employed for wages/salary
#   2=Self-employed
#   3=Unempl for 1+yrs
#   4=Unempl for <1 yr
#   5=Homemaker
#   6=Student
#   7=Retired
#   8=Unable to work
#   .d=Don’t know
#   .r=Refused
