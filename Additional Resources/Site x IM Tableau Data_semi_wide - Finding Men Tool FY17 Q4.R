##   FactView Data to be used in Tableau; Time-Long / Indicators-Wide
##   Site by IM files
##   PSNU/IM file created by Noah Bartlett
##   Date: October 19, 2017
##   Modified for site-level by Kristy Schlenker
##   Updated: Feb 3, 2018


### NOTES

#   The first part of this code transforms time-related data from wide to long to optimize use in Tableau
#   The last part of this code transforms key indicators/modalities from long to wide...
#   This is to avoid having to create a ton of new measures in Tableau. 
#   This limits what you visualize in some regards, but depending on your visualization needs...
#   it may be easier to use a semi-wide format  

#   - Data source: 
#     - ICPI_Fact_View_PSNU_IM
#   If you get an error message which says "Error: cannot allocate vector of size..." then
#     try deleting unneeded files. Or close and restart R. 

######################################################################################
# Packages to be loaded

#######################################################################library(tidyverse)
library(readr)
#library(eply)

# Set-up working directory (Change as needed)
setwd("C:/Kristy/Tableau Tools/Data for R")

# Import data (Change date as needed)
#mydata=read.csv("ICPI_FactView_PSNU_IM_20171222_v2_1.txt",sep="\t",header = T)
##data=read.csv("file:///C:/Kristy/Tableau Tools/Data for R/ICPI_FactView_Site_IM_Uganda_20171222_v2_2.txt",sep="\t",header = T, col_types = "cccccccccccccccccccccccccccccccccccccccccc")

data <- read_tsv("ICPI_FactView_Site_IM_Haiti_20171222_v2_2.txt", na = character(),
              col_types = cols(MechanismID = "c",
                 FY2015Q2 = "d",      
                 FY2015Q3 = "d",      
                 FY2015Q4 = "d",      
                 FY2015APR = "d",     
                 FY2016_TARGETS = "d",
                 FY2016Q1 = "d",      
                 FY2016Q2 = "d",      
                 FY2016Q3 = "d",      
                 FY2016Q4 = "d",      
                 FY2016APR = "d",     
                 FY2017_TARGETS = "d",
                 FY2017Q1 = "d",      
                 FY2017Q2 = "d",      
                 FY2017Q3 = "d",      
                 FY2017Q4 = "d",      
                 FY2017APR = "d",
                 FY2017_TARGETS = "d",
                 FY2018_TARGETS = "d"))

###############
#Drop all but HTS_TST, HTS_TST_POS, HTS_TST_NEG & TX
data <- data %>% 
  filter(
    (indicator %in% c("HTS_TST", "HTS_TST_POS","HTS_TST_NEG", "TX_CURR", "TX_NEW") & 
              standardizedDisaggregate %in% c("Modality/MostCompleteAgeDisagg","MostCompleteAgeDisagg","Total Numerator")) 
     |(indicator %in% c("TX_RET") & standardizedDisaggregate %in% c("AgeAboveTen/Sex", "AgeLessThanTen", "Aggregated Age/Sex", "Total Numerator"))
     |(indicator %in% c("TX_PVLS") & standardizedDisaggregate %in% c("AgeAboveTen/Sex/Indication", "AgeLessThanTen/Indication", "Aggregated Age/Sex", "Aggregated Age/Sex/Indication", "Total Numerator", "Total Denominator"))
          )

##Note - did some completeness analyses for Uganda and it would be ok (at least for FY17) to sum Coarse/Fine for TX_RET & TX_PVLS
## TX_RET disasggs = AgeAboveTen/Sex; AgeLessThanTen; Aggregated Age/Sex; Total Numerator; Total Denominator
## TX_PVLS disaggs = AgeAboveTen/Sex/Indication;	AgeLessThanTen/Indication;	Aggregated Age/Sex;	Aggregated Age/Sex/Indication; Total Numerator;	Total Denominator
## For FY15/16 there is no TX_PVLS and there is only the Age/Sex disagg

#Renames first column header from ï..orgUnitUID to orgUnitUID [no longer needed bc changed type of import]
#data <- data %>% 
#  rename("orgUnitUID" = "ï..orgUnitUID")


################################# 
# Create a single site name from facility, community, or _mil
################################# 

data <- data %>% 
  mutate(
    site_type = 
      case_when(
        orgUnitUID==FacilityUID   ~ "Facility",
        orgUnitUID==CommunityUID  ~"Community",
        typeMilitary=="Y"         ~"Military",
        TRUE                      ~ "") ) 

# Creating the site name variable
data$site_name = with(data, 
                       ifelse(site_type=="Facility", Facility,
                              ifelse(site_type=="Community", Community,
                                     ifelse(site_type=="Military", PSNU, ""))))

######################################################################################

###################################
# Add DREAMS Districts to file
###################################

# List of DREAMS Districts
DREAMS<-c(
  # Kenya
  "Homa Bay", "Kisumu", "Nairobi County", "Siaya",
  # Lesotho
  "Berea", "Maseru", 
  # Malawi
  "Machinga District", "Zomba District",
  # Mozambique
  "Chokwe", "Cidade Da Beira", "Cidade De Quelimane", "Cidade De Xai-Xai","Xai-Xai",
  # South Africa
  "gp City of Johannesburg Metropolitan Municipality", "gp Ekurhuleni Metropolitan Municipality",
  "kz eThekwini Metropolitan Municipality", "kz uMgungundlovu District Municipality",
  "kz Umkhanyakude District Municipality",
  # Swaziland
  "Hhohho", "Lubombo", "Manzini" , "Shiselweni", 
  # Tanzania
  "Kahama DC", "Kahama TC", "Kyela DC","Mbeya CC","Msalala DC","Shinyanga MC","Temeke MC","Ushetu DC",
  # Uganda
  "Bukomansimbi District", "Gomba District", "Gulu District", "Lira District", "Mityana District",
  "Mubende District", "Mukono District", "Oyam District", "Rakai District", "Sembabule District",
  # Zambia
  "Chingola District", "Lusaka Urban District", "Ndola District", 
  # Zimbabwe
  "Bulawayo", "Chipinge", "Gweru" , "Makoni", "Mazowe", "Mutare")


# Creates a TRUE/FALSE column if PSNU is listed above as a DREAMS district
data$DREAMS <- data$PSNU %in% DREAMS


#########################
# Add TX_NET_NEW to file
#########################

# Create new dataframe with just TX_CURR
net_new= data %>%
  filter(indicator=="TX_CURR")                    
#           
# # Calculate TX_NET_NEW and creates new columns for each Q and/or FY
net_new<-     
  net_new %>% 
  mutate_at(vars(starts_with("FY2")),funs(ifelse(is.na(.),0,.))) %>%   
  mutate(indicator="TX_NET_NEW",
         Y2015Q2=FY2015Q2,
         Y2015Q3=0,
         Y2015Q4=FY2015Q4-FY2015Q2,
         Y2015APR=FY2015Q4,
         Y2016_TARGETS=FY2016_TARGETS-FY2015APR,
         Y2016Q1=0,
         Y2016Q2=FY2016Q2-FY2015APR,
         Y2016Q3=0,
         Y2016Q4=FY2016Q4-FY2016Q2,
         Y2016APR=FY2016Q4-FY2015APR,
         Y2017_TARGETS=FY2017_TARGETS-FY2016APR,
         Y2017Q1=FY2017Q1-FY2016Q4,
         Y2017Q2=FY2017Q2-FY2017Q1,
         Y2017Q3=FY2017Q3-FY2017Q2,
         Y2017Q4=FY2017Q4-FY2017Q3,
         Y2017APR=FY2017Q4-FY2016APR)  # <- ADD NECESSARY VARIABLES EACH QUARTER #
# 
# 
# # Delete old columns
net_new=
  net_new %>% 
  select(-starts_with("FY20"))
# 
# 
# # Rename new columns with correct names (i.e. FY2016Q1)
names(net_new) <- gsub("Y2", "FY2", names(net_new))
# 
# # Corrects for an error involving vectors in R
data$indicator <- as.character(data$indicator)
# 
# # Adds TX_NET_NEW dataframe to FactView dataframe
data.netnew=bind_rows(data,net_new)

######################################################################################


################################# 
# Reshape data for use in Tableau
################################# 

# Create three dataframes - results, targets and APR - to be combined.

# These are the columns which will be included in Tableau
# This list can be modified as needed.
TableauColumns<-c("site_type", "site_name",
                  "orgUnitUID", "OperatingUnit", "CountryName", "SNU1", "PSNU", "CurrentSNUPrioritization", 
                  "DREAMS", "PrimePartner", "FundingAgency","ImplementingMechanismName","MechanismID",
                  "CommunityUID", "Community", "CurrentCommunityPrioritization", "typeCommunity",
                  "FacilityUID", "Facility", "CurrentFacilityPrioritization", "typeFacility",
                  "indicator","numeratorDenom", "indicatorType","standardizedDisaggregate", 
                  "Age","Sex","resultStatus","otherDisaggregate","modality")

# Create results dataframe. Only collects quarterly data starting in FY2015Q3
results<- data.netnew %>%
  select(-FY2015Q2, -contains("TARGETS"), -contains("APR")) %>% 
  
  # Columns that will be used in Tableau.
  group_by_at(TableauColumns) %>%
  
  # Creates one Values column and one period column (e.g. FY2017Q3)
  summarize_at(vars(starts_with("FY2")), funs(sum(., na.rm=TRUE))) %>% # <-ADD NEW QUARTER
  ungroup %>%
  gather(period, values, starts_with("FY2")) %>% 
  filter(values !=0)


# Create targets dataframe for FY16 and FY17 targets
targets<- data.netnew %>%
  select(-contains("APR"),-contains("Q")) %>% 
  
  # Columns that will be used in Tableau.
  group_by_at(TableauColumns) %>%
  
  # Creates one Values column and one period column (e.g. FY2016_TARGETS)
  summarize_at(vars(starts_with("FY2")), funs(sum(., na.rm=TRUE))) %>%
  ungroup %>%
  gather(period, values, starts_with("FY2")) %>%
  filter(values !=0)

# Create APR dataframe for FY15, FY16, FY17 APR results
APR<- data.netnew %>%
  select(-contains("TARGETS"),-contains("Q") ) %>% 
  
  # Columns that will be used in Tableau.
  group_by_at(TableauColumns) %>%
  
  # Creates one Values column and one period column (e.g. FY2016APR)
  summarize_at(vars(starts_with("FY2")), funs(sum(., na.rm=TRUE))) %>%
  ungroup %>%
  gather(period, values, starts_with("FY2")) %>%
  filter(values !=0)

# Creates a column in each dataframe to label values either Results or Targets
results$ResultsOrTargets<-"Quarterly Results"
targets$ResultsOrTargets<-"Targets"
APR$ResultsOrTargets<-"Annual Results"


# Changes quarters into dates - PART 1 
#     Will change back to quarters in Tableau
#     Why do we have to do this? Because Tableau assumes that Q1 starts in January. 
#     Although you can set Tableau to an October start, by that time, Tableau has already 
#     assigned the quarterly data to have a January start.

results$period<- gsub("FY20", "",results$period)
targets$period<- gsub("FY20", "",targets$period)
APR$period<- gsub("FY20", "",APR$period)
targets$period<- gsub("_TARGETS", "Q1",targets$period)
APR$period<- gsub("APR", "Q4",APR$period)


# Combines all three dataframes into one
finaldata=bind_rows(results,targets, APR)

# Changes quarters into dates - PART 2
#     YES - I know there are better ways to do this. But this works. And frankly, finding
#     another solution was harder than it should have been. 
finaldata$period[finaldata$period=="15Q1"] <- "10/1/2014"
finaldata$period[finaldata$period=="15Q2"] <- "1/1/2015"
finaldata$period[finaldata$period=="15Q3"] <- "4/1/2015"
finaldata$period[finaldata$period=="15Q4"] <- "7/1/2015"
finaldata$period[finaldata$period=="16Q1"] <- "10/1/2015"
finaldata$period[finaldata$period=="16Q2"] <- "1/1/2016"
finaldata$period[finaldata$period=="16Q3"] <- "4/1/2016"
finaldata$period[finaldata$period=="16Q4"] <- "7/1/2016"
finaldata$period[finaldata$period=="17Q1"] <- "10/1/2016"
finaldata$period[finaldata$period=="17Q2"] <- "1/1/2017"
finaldata$period[finaldata$period=="17Q3"] <- "4/1/2017"
finaldata$period[finaldata$period=="17Q4"] <- "7/1/2017"
finaldata$period[finaldata$period=="18Q1"] <- "10/1/2017"

#############################

#This list matches TableauColumns minus indicator, standardizedDisaggregate, Age, Sex, Results Status
##plus period and ResultsOrTargets
#is the list of variables that is going to be used to summarize/aggregate the new wider data once transformed below
SummaryCols <- c("site_type", "site_name",
                  "orgUnitUID", "OperatingUnit", "CountryName", "SNU1", "PSNU", "CurrentSNUPrioritization", "DREAMS",
                 "PrimePartner", "FundingAgency","ImplementingMechanismName","MechanismID",
                 "CommunityUID", "Community", "CurrentCommunityPrioritization", "typeCommunity",
                 "FacilityUID", "Facility", "CurrentFacilityPrioritization", "typeFacility",
                 "numeratorDenom", "indicatorType","otherDisaggregate","modality","period",
                 "ResultsOrTargets")

##Creates a new concatenated column with Indicator, Age, Sex
##Then spreads the indicator/age/sex into wide columns so that you don't have to create as many measures in Tableau
##Then drops the indicator, standardized disag, age, sex, result status, and values columns because they're already in wide format
##Then summarizes/aggregates on the SummaryCols list defined above so that all is collapsed onto single lines


####NEED TO REPLACE NA WITH "" for AGE, SEX, RESULTSTATUS
finaldatawide <- finaldata %>%
  mutate(concated = paste(indicator, Age, Sex, sep = ' ')) %>% 
  spread(key = concated, value = values) 

finaldatawide <- finaldatawide  %>% 
  select(-c(indicator, standardizedDisaggregate, Age, Sex, resultStatus)) %>% 
  group_by_at(SummaryCols) %>% 
  summarise_all(., funs(sum(., na.rm=TRUE)) ) 


# Saves dataframe as .txt file
write_tsv(finaldatawide, "FY17Q4_Site_IM_WIDE_Haiti.txt")
