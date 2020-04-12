## Load libraries ##
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggpubr)
library(readxl)
library(stringi)

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
covid <- read.csv(file="/Users/lyllayounes/Documents/lrn_github/covid-reporting/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", head=TRUE,sep=",")
names(covid)

covid <- covid %>% filter(Province_State == "Louisiana" | Province_State == "Texas") 
covid <- covid %>% filter(substr(FIPS,0,2) == "22" | substr(FIPS,0,2) == "48" ) #
keeps <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key", "Population", "X4.7.20")
covid <- covid[keeps]
covid$deaths <- ifelse(is.na(covid$X4.7.20),0,covid$X4.7.20) 
View(covid)

df <- merge(nata_la_tx, covid, by.x="FIPS", by.y="FIPS")
View(df)

reg = lm(deaths ~ Population.x + hazard_quotient, data=df)
summary(reg)

reg = lm( (deaths/Population.x) ~ hazard_quotient, data=df)
summary(reg)

ggscatter(df, x = "hazard_quotient", y = "deaths",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "NATA RESPIRATORY HAZARD", ylab = "COVID-19 DEATHS")


## Analyze every chemical ##
d <- df[ -c(1:5, 51:63) ]
names(d)

d$deaths_per_capita <- d$deaths / d$Population.x
covmod <- lm(deaths ~ ., data = d)
summary(covmod)


## Account for population density ##
census_county_interpolated <- read_csv("/Users/lyllayounes/Documents/lrn_github/covid-reporting/PM_COVID/Data/census_county_interpolated.csv")
census_county_interpolated$fips <- as.character(census_county_interpolated$fips)

censusAvg <- census_county_interpolated %>% 
  group_by(fips) %>% 
  summarise(popdensity = mean(popdensity), poverty = mean(poverty)) %>% 
  ungroup()

df2 <- left_join(df, censusAvg, by = c("FIPS" = "fips"))

reg = lm(deaths ~ Population.x + hazard_quotient + popdensity + poverty, data=df2)
summary(reg)

df2$deaths_per_capita <- df2$deaths / df2$Population.x

reg = lm( deaths_per_capita ~ hazard_quotient + popdensity + poverty, data=df2)
summary(reg)



## Pull TRI Data into Session ##

TRI_2018_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2018/fname/TRI_2018_US.csv/CSV")
TRI_2017_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2017/fname/TRI_2017_US.csv/CSV")
TRI_2016_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2016/fname/TRI_2016_US.csv/CSV")
TRI_2015_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2015/fname/TRI_2015_US.csv/CSV")
TRI_2014_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2014/fname/TRI_2014_US.csv/CSV")
TRI_2013_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2013/fname/TRI_2013_US.csv/CSV")
TRI_2012_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2012/fname/TRI_2012_US.csv/CSV")
TRI_2011_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2011/fname/TRI_2011_US.csv/CSV")
TRI_2010_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2010/fname/TRI_2010_US.csv/CSV")
TRI_2009_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2009/fname/TRI_2009_US.csv/CSV")
TRI_2008_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2008/fname/TRI_2008_US.csv/CSV")
TRI_2007_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2007/fname/TRI_2007_US.csv/CSV")
TRI_2006_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2006/fname/TRI_2006_US.csv/CSV")
TRI_2005_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2005/fname/TRI_2005_US.csv/CSV")
TRI_2004_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2004/fname/TRI_2004_US.csv/CSV")
TRI_2003_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2003/fname/TRI_2003_US.csv/CSV")
TRI_2002_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2002/fname/TRI_2002_US.csv/CSV")
TRI_2001_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2001/fname/TRI_2001_US.csv/CSV")
TRI_2000_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/2000/fname/TRI_2000_US.csv/CSV")
TRI_1999_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1999/fname/TRI_1999_US.csv/CSV")
TRI_1998_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1998/fname/TRI_1998_US.csv/CSV")
TRI_1997_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1997/fname/TRI_1997_US.csv/CSV")
TRI_1996_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1996/fname/TRI_1996_US.csv/CSV")
TRI_1995_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1995/fname/TRI_1995_US.csv/CSV")
TRI_1994_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1994/fname/TRI_1994_US.csv/CSV")
TRI_1993_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1993/fname/TRI_1993_US.csv/CSV")
TRI_1992_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1992/fname/TRI_1992_US.csv/CSV")
TRI_1991_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1991/fname/TRI_1991_US.csv/CSV")
TRI_1990_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1990/fname/TRI_1990_US.csv/CSV")
TRI_1989_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1989/fname/TRI_1989_US.csv/CSV")
TRI_1988_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1988/fname/TRI_1988_US.csv/CSV")
TRI_1987_US <- read_csv("https://data.epa.gov/efservice/MV_TRI_BASIC_DOWNLOAD/year/=/1987/fname/TRI_1987_US.csv/CSV")

tridat1 <- rbind(TRI_2018_US,
                 TRI_2017_US,
                 TRI_2016_US,
                 TRI_2015_US,
                 TRI_2014_US,
                 TRI_2013_US,
                 TRI_2012_US,
                 TRI_2011_US,
                 TRI_2010_US,
                 TRI_2009_US,
                 TRI_2008_US,
                 TRI_2007_US,
                 TRI_2006_US,
                 TRI_2005_US,
                 TRI_2004_US,
                 TRI_2003_US,
                 TRI_2002_US,
                 TRI_2001_US,
                 TRI_2000_US,
                 TRI_1999_US,
                 TRI_1998_US,
                 TRI_1997_US,
                 TRI_1996_US,
                 TRI_1995_US,
                 TRI_1994_US,
                 TRI_1993_US,
                 TRI_1992_US,
                 TRI_1991_US,
                 TRI_1990_US,
                 TRI_1989_US,
                 TRI_1988_US,
                 TRI_1987_US)

View(tridat1)
names(tridat1)


