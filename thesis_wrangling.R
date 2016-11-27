# Code to pre-process data for thesis
# Jessica Kim
# Fall 2016

############# MERGE CHS DATA ############# 
setwd("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_chs")
setwd("/Users/jsk2225/Downloads")

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


############# MAKE UHF NEIGHBORHOOD AND ZIP CODE MERGE FILE ############# 

library(dplyr)
library(tidyr)
# Bronx
"South Bronx" <- c(10451, 10452, 10453, 10454, 10455, 10456, 10457, 10459, 10460, 10474)
"Fordham-Bronx P" <- c(10458, 10467, 10468)
"Kingsbridge - Riverdale" <- c(10463, 10471)
"NE Bronx" <- c(10466, 10469, 10470, 10475)
"Pelham-Throgs Neck" <- c(10461, 10462,10464, 10465, 10472, 10473)
bronx_nh <- list(
  `South Bronx` = `South Bronx`, 
  `Fordham-Bronx Pk` = `Fordham-Bronx P`, 
  `Kingsbridge` = `Kingsbridge - Riverdale`, 
  `NE Bronx` = `NE Bronx`, 
  `Pelham-Throgs Neck` = `Pelham-Throgs Neck`)
Bronx <- as.data.frame(do.call(rbind, bronx_nh))
Bronx$uhf <- row.names(Bronx)
Bronx$borough <- "Bronx"
Bronx <- gather(Bronx, "uhf", "zip", 1:10)
Bronx[, 3] <- rm()

# Brooklyn
Brooklyn <- c("Bedford Stuyvesant - Crown Heights", "Bensonhurst - Bay Ridge", "Borough Park", "Canarsie - Flatlands", "Coney Island - Sheepshead Bay", "Downtown - Heights - Slope", "East Flatbush - Flatbush", "East New York", "Greenpoint", "Sunset Park", "Williamsburg - Bushwick")
"Bedford Stuyvesant - Crown Heights" <- c(11212, 11213, 11216, 11233, 11238)
"Bensonhurst - Bay Ridge" <- c(11209, 11214, 11228)
"Borough Park" <- c(11204, 11218, 11219, 11230)
"Flatbush-Canarsie" <- c(11234, 11236, 11239, 11203, 11210, 11225, 11226)
"Coney Island - Sheepshead Bay" <- c(11223, 11224, 11229, 11235)
"Downtown - Heights - Slope" <- c(11201, 11205, 11215, 11217, 11231)
"East New York" <- c(11207, 11208)
"Greenpoint" <- c(11211, 11222)
"Sunset Park" <- c(11220, 11232)
"Williamsburg - Bushwick" <- c(11206, 11221, 11237)

brooklyn_nh <- list(
  `Bed Stuy-Crown Heights` = `Bedford Stuyvesant - Crown Heights`,
  `Bensonhurst`	= `Bensonhurst - Bay Ridge`,
  `Borough Park` = `Borough Park`,
  `Flatbush-Canarsie` = `Flatbush-Canarsie`,
  `Coney Island` = `Coney Island - Sheepshead Bay`,
  `Downtown-Heights-Slope` = `Downtown - Heights - Slope`,
  `East New York`	= `East New York`,
  `Greenpoint` = `Greenpoint`,
  `Sunset Park`	= `Sunset Park`,
  `Williamsburg-Bushwk` = `Williamsburg - Bushwick`)

Brooklyn <- as.data.frame(do.call(rbind, brooklyn_nh))
Brooklyn$uhf <- row.names(Brooklyn)
Brooklyn$borough <- "Brooklyn"
Brooklyn <- gather(Brooklyn, "uhf", "zip", 1:7)
Brooklyn[, 3] <- rm()
Brooklyn <- Brooklyn[!duplicated(Brooklyn), ]

# Manhattan
"Central Harlem - Morningside Heights" <- c(10026, 10027, 10030, 10037, 10039)
"Chelsea - Clinton" <- c(10001, 10011, 10012, 10013, 10014, 10018, 10019, 10020, 10036)
"East Harlem" <- c(10029, 10035)
"Gramercy Park - Murray Hill" <- c(10010, 10016, 10017, 10021, 10022, 10028, 10044, 
                                   10065, 10075, 10128, 10162, 10165, 10170, 10171)
"Lower Manhattan" <- c(10002, 10003, 10004, 10005, 10006, 10007, 10009, 10038, 10048, 10280, 10282)
"Upper West Side" <- c(10023, 10024, 10025, 10069)
"Washington Heights - Inwood" <- c(10031, 10032, 10033, 10034, 10040)

Manhattan_nh <- list(
  `Central Harlem` = `Central Harlem - Morningside Heights`,
  `Chelsea-Village` = `Chelsea - Clinton`,
  `East Harlem` = `East Harlem`,
  `Upper East Side-Gramercy ` = `Gramercy Park - Murray Hill`,
  `Lower Manhatten` = `Lower Manhattan`,
  `Upper West Side` = `Upper West Side`,
  `Wash Heights-Inwood` = `Washington Heights - Inwood`)

Manhattan <- as.data.frame(do.call(rbind, Manhattan_nh))
Manhattan$uhf <- row.names(Manhattan)
Manhattan$borough <- "Manhattan"
Manhattan <- gather(Manhattan, "uhf", "zip", 1:14)
Manhattan[, 3] <- rm()
Manhattan <- Manhattan[!duplicated(Manhattan), ]

# Queens
"Bayside - Little Neck" 	 <- c(11361, 11362, 11363, 11364, 11365, 11366, 11367)
"Flushing - Clearview" 	 	 <- c(11354, 11355, 11356, 11357, 11358, 11359, 11360)
"Jamaica" 					 <- c(11412, 11423, 11432, 11433, 11434, 11435, 11436)
"Long Island City - Astoria" <- c(11101, 11102, 11103, 11104, 11105, 11106, 
                                  11368, 11369, 11370, 11372, 11373, 11377, 11378)
"Ridgewood - Forest Hills" 	 <- c(11374, 11375, 11379, 11385)
"Rockaway" 					 <- c(11691, 11692, 11693, 11694, 11695, 11697)
"Southeast Queens" 			 <- c(11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429)
"Southwest Queens"			 <- c(11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421)

Queens_nh <- list(
  `Bayside-Meadows` = `Bayside - Little Neck`, 	
  `Flushing` = `Flushing - Clearview`, 	 	
  `Jamaica` = `Jamaica`, 					
  `W Queens-Long Island City` = `Long Island City - Astoria`,
  `Ridgewood` = `Ridgewood - Forest Hills`, 	
  `Rockaway` = `Rockaway`, 					
  `SE Queens` = `Southeast Queens`, 			
  `SW Queens` = `Southwest Queens`)

Queens <- as.data.frame(do.call(rbind, Queens_nh))
Queens$uhf <- row.names(Queens)
Queens$borough <- "Queens"
Queens <- gather(Queens, "uhf", "zip", 1:13)
Queens[, 3] <- rm()
Queens <- Queens[!duplicated(Queens), ]

# Staten Island
"Port Richmond" <- c(10301, 10302, 10303, 10304, 10305, 10310 )
"South Beach - Tottenville" <- c(10306, 10307, 10308, 10309, 10312, 10314)

Staten_nh <- list(
  `Northern SI` = `Port Richmond`,
  `Southern SI` = `South Beach - Tottenville`)

Staten <- as.data.frame(do.call(rbind, Staten_nh))
Staten$uhf <- row.names(Staten)
Staten$borough <- "Staten"
Staten <- gather(Staten, "uhf", "zip", 1:6)
Staten[, 3] <- rm()
Staten <- Staten[!duplicated(Staten), ]

# Merge all boroughs zip code and UHF neighbrhood data
uhf_zips <- rbind(Bronx, Manhattan, Queens, Brooklyn, Staten)
write.csv(uhf_zips, file = "uhf_zips.csv")

############# PREP SALES DATA ############# 

# ruby script already changed .xls files to .csv and merged for each year
library(readr) 
data_sales_zips <- read_csv("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_wrangling/sum-data-zip.csv")
uhf_zips <- read_csv("uhf_zips.csv")
colnames(data_sales_zips) <- c("year", "zip", "sale_amount")
data_sales_zips$zip <- as.integer(data_sales_zips$zip)
table(duplicated(data_sales_zips)) # check for duplicates
data_sales <- left_join(data_sales_zips, uhf_zips[, -1], by = c("zip" = "zip"))

# aggregate sales to uhf level
data_sales_uhf <- data_sales %>%
  group_by(year, uhf) %>% 
  summarize(n_sales = n(),
            total_sales_uhf = sum(sale_amount))
save(data_sales_uhf, file = "data_sales_uhf.RData")

### For some reason, 2003 data wasn't included...for prelim analysis, removing 2003
chs_datalist3 <- chs_datalist2[-12]
save(chs_datalist3, file = "chs_datalist3.RData")
load("chs_datalist3.RData")

# For each df in chs datalist, get proportions at the strata (neighborhood level)
# APPLY WEIGHTS BEFORE CALCULATING THE FOLLOWING PROPORTIONS BY STRATA
# 34 strata pre-2008 and 35 strata post-2008?
chs_uhf_level <- lapply(chs_datalist3, function(x) {
  x %>%
    group_by(strata) %>%
    mutate(males = ifelse(sex == 1, 1 * wt, 0),
           blacks = ifelse(newrace == 2, 1 * wt, 0),
           foreigns = ifelse(usborn == 2, 1 * wt, 0),
           low_educs = ifelse(education == 1, 1 * wt, 0),
           unemploy = ifelse(employment == 3, 1 * wt, 0)) %>%
    summarize(n = sum(wt),
              year = mean(year),
              n_male = sum(males, na.rm = TRUE),
              prop_male = 100 * (sum(males, na.rm = TRUE) / n),
              n_black = sum(blacks, na.rm = TRUE),
              prop_black = 100 * (sum(blacks, na.rm = TRUE) / n),
              n_foreigns = sum(foreigns, na.rm = TRUE),
              prop_foreign = 100 * (sum(foreigns, na.rm = TRUE) / n),
              n_low_educ = sum(low_educs, na.rm = TRUE),
              prop_less_hs = 100 * (sum(low_educs, na.rm = TRUE) / n),
              n_unemployed = sum(unemploy, na.rm = TRUE),
              prop_unemployed = 100 * (sum(unemploy, na.rm = TRUE) / n))
})
summary(chs_uhf_level$chs2014)

# Read in uhf neighborhood names
uhf_names <- read_csv("uhf_strata_names_merge.csv")
uhf_names$X4 <- NULL
chs_uhf_level <- lapply(chs_uhf_level, function(x) merge(x, uhf_names, by.x = "strata", by.y = "strata"))
lapply(chs_uhf_level, head) # check that numbers are right, first 6 rows
lapply(chs_uhf_level, tail) # check numbers are right, last 6 rows
lapply(chs_uhf_level, NROW) # check that number of stratas are consistent
save(chs_uhf_level, file = "chs_uhf_level.RData")

############# MERGE CRIME DATA ############# 
# First need to get crimes at UHF neighborhood level
felonies_df <- read_csv("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/historical_citywide_crime_complaint_data_by_precinct_2000_2015/NYPD_7_Major_Felony_Incidents.csv")
felonies_2002_to_2014 <- felonies_df %>%
  select(-`Jurisdiction`, -starts_with("Comp")) %>%
  filter(`Occurrence Year` > 2001 & `Occurrence Year` < 2015)
write.csv(felonies_2002_to_2014, file = "/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/historical_citywide_crime_complaint_data_by_precinct_2000_2015/felonies_2002_to_2014.csv")
# Loaded in felonies table into QGIS and did location merge with UHF neighborhood shape file to add neighborhoods to felonies table
# to read in only last 2 columns: read.csv(file="result1", sep=" ", colClasses=c("NULL", NA, NA))

felonies_with_uhf <- read_csv("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/historical_citywide_crime_complaint_data_by_precinct_2000_2015/felonies_with_uhf.csv")

# Aggregate felonies up to uhf level
felonies_uhf_level <- felonies_with_uhf %>%
  select(Occurren_3, Offense, UHF_CODE) %>%
  rename(year = Occurren_3) %>%
  group_by(UHF_CODE, year) %>%
  mutate(n_felonies_uhf = n()) %>%
  group_by(UHF_CODE, Offense, year) %>%
  mutate(n_offense_uhf = n(),
         prop_offense_uhf = 100 * (n_offense_uhf / n_felonies_uhf)) # create rates of each offense type by strata

felonies_uhf_level <- felonies_uhf_level[!duplicated(felonies_uhf_level), ]
felonies_uhf_level <- felonies_uhf_level[order(felonies_uhf_level$year, felonies_uhf_level$UHF_CODE, felonies_uhf_level$Offense), ]

############# MERGE ALL DATA ############# 
# Combine all chs dfs
load("chs_uhf_level.RData")
chs_data_all <- Reduce(function(...) merge(..., all = TRUE), chs_uhf_level)
chs_data_all <- chs_data_all[order(chs_data_all$strata, chs_data_all$year), ]

head(felonies_uhf_level)
felonies_chs_df <- right_join(felonies_uhf_level, chs_data_all, 
                              by = c("UHF_CODE" = "uhf_code", "year" = "year"))
felonies_chs_df <- felonies_chs_df[order(felonies_chs_df$year, felonies_chs_df$UHF_CODE), ]
setdiff(felonies_chs_df$neighborhood_name, data_sales_uhf$uhf) # check if neighborhood names match (need to rename union square to lower mnhtn)
felonies_chs_df$neighborhood_name[felonies_chs_df$neighborhood_name == "Union Square"] <- "Lower Manhatten"
felonies_chs_df$UHF_CODE[felonies_chs_df$neighborhood_name == "Lower Manhatten"] <- 309310
View(felonies_chs_df)
save(felonies_chs_df, file = "felonies_chs_df.RData")
# I think I need to make all NA for crime data as 0, assuming that if there is no data then there was no crime
felonies_chs_sales <- left_join(felonies_chs_df, data_sales_uhf, by = c("year" = "year", "neighborhood_name" = "uhf"))
View(felonies_chs_sales)
save(felonies_chs_sales, file = "felonies_chs_sales.RData")
load("felonies_chs_sales.RData")


############# RELEVANT CODEBOOK EXTRACTS ############# 

# get proportion of each sex
## 1=male; 2=female
# get proportion of each race
## 1=White Non-Hispanic ; 2=Black Non-Hispanic ; 3=Hispanic ; 4=Asian/PI Non-Hispanic ; 5=Other Non-Hispanic
# get proportion of foreign born
## 1 = usborn; 2 = foreign born
# get proportions of each education levels
## 1=Less than HS ; 2=High school grad ; 3=Some college ; 4=College graduate
# get proportions of poverty group
# Neighborhood poverty;
# percent of zip code
# population living
# below 100% FPL per
# American Community
# Survey, 2008-2012
# 1= 0 - <10% (low pov) 1837 2=10 - <20% 2699 3=20 - <30% 2095 4=30 - <100% (very hi)
# get proportions of age
## 1=18-24yrs ; 2=25-44 yrs ; 3=45-64 yrs ; 4=65+ yrs

# get proportions of at home language? REMOVE THIS var
# remove demog - it actually is the age var
# Employment

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
#   The response order for Q8.13 differs from the order of responses in formatted frequencies.Employment12 was renumbered so that response options would be consistent with order in prior years
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
