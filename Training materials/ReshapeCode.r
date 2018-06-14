## To reshape MER Structured Dataset from wide to long, using Site x IM training dataset
#Load much needed libraries

library(tidyr)
library(plyr)
library(stringr)
library(dplyr)

#import training dataset directly into R; change file name and update working directory as needed
#File location on sharepoint: https://www.pepfar.net/OGAC-HQ/icpi/Products/ICPI%20Data%20Store/MER/Testing/ICPI_MER_Structured_Dataset_SITE_IM_Training_20180614.zip
df <- readr::read_tsv("ICPI_MER_Structured_Dataset_SITE_IM_Training_20180614.txt")
View(df)

#reshape training dataset from wide to long, putting all results/targets into one column 
long.df <- gather(df, key= "period", "value", FY2017_TARGETS:FY2018Q2, na.rm=TRUE)

#Create separate column for labeling targets and results
longplus.df <- long.df %>% mutate(Valuetype=ifelse(str_detect(period, "TARGETS"),"Targets", "Results"))

#subset data without NULL values
sub.df <- select(filter(longplus.df, value != "NULL"),c(orgUnitUID:Valuetype))

#check Nulls + subset data = total of gather3
gathernulls <- select(filter(longplus.df, value == "NULL"))

#Export dataset to csv or txt or xls for reading into Tableau
write.csv(longplus.df,"trainingdatasetsiteimv2.csv", row.names=FALSE)
