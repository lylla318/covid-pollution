# Load ggplot into current session
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggpubr)
library(readxl)
library(stringi)
install.packages("tidycensus")
library(tidycensus)
library(readr)
library(tidyr)

#set our working directory 
setwd("C:/Users/Mike Petroni/Documents/Rproj/Covid")

#grab our data 
########
#########################################
########

covid_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

#census data 
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69")
#what variables do we want?
v17 <- load_variables(2009, "acs5", cache = TRUE)
View(v17)
unique(v17$concept)
write.csv(v17,"ACSvars.csv")


Years <- c(1999:2018)
Year <- 2018
censusGrab <- function(Year) {
  #this will grab all of our ACS data by year
  acsYear <- get_acs(geography = "county", 
                variables = c(medincome = "B19013_001",
                              population = "B01001_001",
                              black_pop = "B01001B_001",
                              hispanic_pop = "B01001I_001",
                              poverty_status_total = "B17001_001",
                              poverty_below_total = "B17001_002",
                              median_house_value = "B25077_001",
                              Total_Education = "B15003_001",
                              overHS1 = "B15003_017",
                              overHS2 = "B15003_018",
                              overHS3 = "B15003_019",
                              overHS4 = "B15003_020",
                              overHS5 = "B15003_021",
                              overHS6 = "B15003_022",
                              overHS7 = "B15003_023",
                              overHS8 = "B15003_024",
                              overHS9 = "B15003_025",
                              housing_units = "B25001_001",
                              owner_occupied = "B25002_002",
                              Male_age_65_66 = "B01001_020",
                              Male_age_67_69 = "B01001_021",
                              Male_age_70_74 = "B01001_022",
                              Male_age_75_79 = "B01001_023",
                              Male_age_80_84 = "B01001_024",
                              Male_age_85_plus = "B01001_025",
                              F_age_65_66 = "B01001_044",
                              F_age_67_69 = "B01001_045",
                              F_age_70_74 = "B01001_046",
                              F_age_75_79 = "B01001_047",
                              F_age_80_84 = "B01001_048",
                              F_age_85_plus = "B01001_049"),
                year = Year)
  
  # spead em buster!
  acsYear1 <- acsYear %>% select(-moe) %>% spread(variable, estimate) %>% distinct()
  
  # add year var
  acsYear1$year <- rep(Year, nrow(acsYear1))
  
  # get our percentage variables 
  acsYear2 <- acsYear1 %>% mutate(Black_Per = black_pop/population,
                                  Hispanic_Per = hispanic_pop/population,
                                  Poverty_Per = poverty_below_total/poverty_status_total,
                                  Under_HS_Per = 1-((overHS1 + overHS2 + overHS3 + overHS4 + overHS5 + overHS6 +
                                    overHS7 + overHS8 +overHS9)/Total_Education),
                                  Owner_Occ_Per = owner_occupied/housing_units,
                                  Over64_M_Per = (Male_age_65_66 + Male_age_67_69 +Male_age_70_74 +
                                    Male_age_75_79 + Male_age_80_84 + Male_age_85_plus)/population,
                                  Over64_F_Per = (F_age_65_66 + F_age_67_69 + F_age_70_74 +
                                                    F_age_75_79 + F_age_80_84 + F_age_85_plus)/population)
  # cut it to what we want
  acsYear3 <- acsYear2 %>% select(GEOID, NAME, year, median_house_value, medincome, Black_Per,
                                  Hispanic_Per, population, Poverty_Per, Under_HS_Per,
                                  Owner_Occ_Per, Over64_F_Per, Over64_M_Per)
  
  return(acsYear3)
}

#looks like we need to start in 2012 due to some of our variables no being available prior to that 
founddat <- lapply(2012:2018, function (x)  censusGrab(x))
l25 <- founddat[[1]]
for (i in 2:length(founddat)) l25 <- bind_rows(l25, founddat[[i]])

#average them out
acsAvs <- l25 %>% group_by(GEOID, NAME) %>% summarise(median_house_value_mean_12to18 = mean(median_house_value, na.rm = T),
                                                      median_income_mean_12to18 = mean(medincome, na.rm = T),
                                                      Black_Per_mean_12to18 = mean(Black_Per, na.rm = T),
                                                      Hispanic_Per_mean_12to18 = mean(Hispanic_Per, na.rm =T),
                                                      population_mean_12to18 = mean(population, na.rm =T),
                                                      Poverty_Per_mean_12to18 = mean(Poverty_Per, na.rm = T),
                                                      Under_HS_Per_mean_12to18 = mean(Under_HS_Per, na.rm = T),
                                                      Owner_Occ_Per_mean_12to18 = mean(Owner_Occ_Per, na.rm = T),
                                                      Over64_F_Per_mean_12to18 = mean(Over64_F_Per, na.rm =T),
                                                      Over64_M_Per_mean_12to18 = mean(Over64_M_Per, na.rm = T))
                                                      
#ACS data complete! score



#lets explore other county level data.....

#this is a great resource countyhealthrankings.org
# here is the documentation https://www.countyhealthrankings.org/sites/default/files/media/document/Trends%20documentation%202020.pdf
CHRtrends <- read_csv("https://www.countyhealthrankings.org/sites/default/files/media/document/CHR_trends_csv_2020.csv")

unique(CHRtrends$measurename)
unique(CHRtrends$yearspan)

# lets grab air pollution, aldult obesity, premature death, physical inactivity 

CHRadder <- CHRtrends %>% filter(!yearspan %in% c("1997-1999", "1998-2000", "1999-2001", "2000-2002", "2001-2003", "2002-2004",
                                                  "2003-2005", "2004-2006", "2005-2007", "2006-2008", "2007-2009", "2008-2010",
                                                  "2009-2011", "2008", "2009","2010", "2011"))

CHRadder <- CHRadder %>% select(statecode, countycode, county, measurename, rawvalue)

#make the fips code
CHRadder <- CHRadder %>% mutate(GEOID =  )

#spread it!
acsYear1 <- acsYear %>% select(-moe) %>% spread(variable, estimate) %>% distinct()

#smokeing rates by county 
# so the trends data does not have smoking, lets grab that for our years 
# https://www.countyhealthrankings.org/sites/default/files/DataDictionary_2014.pdf

CHR14 <- read_csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2014.csv", skip = 1)
   

# hospital beds 

hospitals <- read_csv("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
View(hospitals)

# here is the base data for most of the CHR indicators, but its a bit more raw than we want

download.file("http://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011XPT.ZIP",
              destfile = "LLCP2011XPT.ZIP")
unzip("LLCP2011XPT.ZIP")

#you may need to manually download and unzip this thing
install.packages("Hmisc")
library(Hmisc)
LLCP <- sasxport.get("LLCP2011.XPT")

names(LLCP)
View(head(LLCP))


download.file("http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.zip",
              destfile = "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.zip")

unzip("IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.zip")









nata2014v2_national_resphi_by_tract_poll_1_  <- read_excel("/Users/lyllayounes/Documents/lrn_github/covid-pollution/data/nata2014v2_national_resphi_by_tract_poll.xlsx")
View(head(nata2014v2_national_resphi_by_tract_poll_1_))
nata2 <- nata2014v2_national_resphi_by_tract_poll_1_

## Reformat NATA data to be by county ##
nata2$FIPS  = as.character(nata2$FIPS)
nata2$Tract = as.character(nata2$Tract)
nata2$FIPS_TRACT = stri_pad_right(nata2$FIPS, 11, 0)
nata2  <- nata2 [which(nata2$Tract == nata2$FIPS_TRACT ),]
nata2$Population <- as.numeric(as.character( gsub(",", "", nata2$Population) ))

## Cut out LA and TX ##
nata_la_tx <- nata2 %>% filter(nata2$State == "LA" | nata2$State == "TX")
nata_la_tx <- nata_la_tx %>% filter(nata_la_tx$County != "Entire State" &nata_la_tx$County != "Entire US")
nata_la_tx <- nata_la_tx %>% rename(hazard_quotient = `Total Respiratory (hazard quotient)`)
nata_la_tx <- nata_la_tx %>% rename(Population.x = `Population`)

names(nata_la_tx)
View(nata_la_tx)

## Reformat covid data ##
covid <- read.csv(file="/Users/lyllayounes/Documents/lrn_github/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", head=TRUE,sep=",")
names(covid)

covid <- covid %>% filter(Province_State == "Louisiana" | Province_State == "Texas")
covid <- covid %>% filter(substr(FIPS,0,2) == "48" | substr(FIPS,0,2) == "22")
keeps <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key", "Population", "X4.7.20")
covid <- covid[keeps]
covid$deaths <- ifelse(is.na(covid$X4.7.20),0,covid$X4.7.20) 
View(covid)

df <- merge(nata_la_tx, covid, by.x="FIPS", by.y="FIPS")
View(df)

reg = lm(deaths ~ Population.x + hazard_quotient, data=df)
summary(reg)


# ggscatter(df, x = "hazard_quotient", y = "deaths", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "spearman",
#           xlab = "NATA RESPIRATORY HAZARD", ylab = "COVID-19 DEATHS")







