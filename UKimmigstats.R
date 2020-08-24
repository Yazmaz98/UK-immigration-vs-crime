install.packages("naniar")
install.packages("GDAtools")
library("GDAtools")
library(naniar)
library(ggplot2)
library(corrplot)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(caret)
library(factoextra)
library(caTools)
library(randomForest)
library(boot)
library(ggpubr)
library(fitdistrplus)



# 1. Data preparation and cleaning
#1.1. Datasets structures and checking for NAs
crime = read.csv("crime.csv", stringsAsFactors = FALSE, na.strings=c("","NA"," "))
immig = read.csv("borough_only.csv", stringsAsFactors = FALSE, na.strings=c("","NA"," "))

str(crime) #from 2008 to 2018
str(immig) #from 2002 to 2018
#Cheking for NAs
colSums(is.na(crime))
colSums(is.na(immig))

#weve seen that the crime dataset has some NAs. We will take care of that after some feature engineering once we 
#joint the tables and have the final dataset we will work with.

#1.2. Features Engineering
#Taking care of Immig dataset
#let's delete years 2002 - 2007 from immig
immig$X2002 <- NULL
immig$X2003 <- NULL
immig$X2004 <- NULL
immig$X2005 <- NULL
immig$X2006 <- NULL
immig$X2007 <- NULL

#let's set correct variable types 
immig$Area = factor(immig$Area)
immig$Old.Code = factor(immig$Old.Code)

#run the line below to see the new structure of the dataset
#str(immig)
#Taking care of Crime dataset
#let's take care of variable types
crime$Borough = factor(crime$Borough)
crime$Major.Category = factor(crime$Major.Category)
crime$Minor.Category = factor(crime$Minor.Category)
#run the line below to see the new structure of the dataset
#str(crime)

#Crime dataset's area codes are way too many, and they do not designate each borough with a unique area code we fine the same borough with multiple different area codes. Because we want to be able to use this variable as a predictive one in our model, we will set crime's area codes to those of immig's dataset because those are unique and have only 33 different levels.
names(crime)[names(crime) == "LSOA.Code"] <- "area_code"
# here we change area codes according to the borough
crime$area_code[crime$Borough=="City of London"] = immig$New.Code[immig$Area=="City of London"]
crime$area_code[crime$Borough=="Barking and Dagenham"] = immig$New.Code[immig$Area=="Barking and Dagenham"]
crime$area_code[crime$Borough=="Barnet"] = immig$New.Code[immig$Area=="Barnet"]
crime$area_code[crime$Borough=="Bexley"] = immig$New.Code[immig$Area=="Bexley"]
crime$area_code[crime$Borough=="Brent"] = immig$New.Code[immig$Area=="Brent"]
crime$area_code[crime$Borough=="Bromley"] = immig$New.Code[immig$Area=="Bromley"]
crime$area_code[crime$Borough=="Camden"] = immig$New.Code[immig$Area=="Camden"]
crime$area_code[crime$Borough=="Croydon"] = immig$New.Code[immig$Area=="Croydon"]
crime$area_code[crime$Borough=="Ealing"] = immig$New.Code[immig$Area=="Ealing"]
crime$area_code[crime$Borough=="Enfield"] = immig$New.Code[immig$Area=="Enfield"]
crime$area_code[crime$Borough=="Greenwich"] = immig$New.Code[immig$Area=="Greenwich"]
crime$area_code[crime$Borough=="Hackney"] = immig$New.Code[immig$Area=="Hackney"]
crime$area_code[crime$Borough=="Hammersmith and Fulham"] = immig$New.Code[immig$Area=="Hammersmith and Fulham"]
crime$area_code[crime$Borough=="Haringey"] = immig$New.Code[immig$Area=="Haringey"]
crime$area_code[crime$Borough=="Harrow"] = immig$New.Code[immig$Area=="Harrow"]
crime$area_code[crime$Borough=="Havering"] = immig$New.Code[immig$Area=="Havering"]
crime$area_code[crime$Borough=="Hillingdon"] = immig$New.Code[immig$Area=="Hillingdon"]
crime$area_code[crime$Borough=="Hounslow"] = immig$New.Code[immig$Area=="Hounslow"]
crime$area_code[crime$Borough=="Islington"] = immig$New.Code[immig$Area=="Islington"]
crime$area_code[crime$Borough=="Kensington and Chelsea"] = immig$New.Code[immig$Area=="Kensington and Chelsea"]
crime$area_code[crime$Borough=="Kingston upon Thames"] = immig$New.Code[immig$Area=="Kingston upon Thames"]
crime$area_code[crime$Borough=="Lambeth"] = immig$New.Code[immig$Area=="Lambeth"]
crime$area_code[crime$Borough=="Lewisham"] = immig$New.Code[immig$Area=="Lewisham"]
crime$area_code[crime$Borough=="Merton"] = immig$New.Code[immig$Area=="Merton"]
crime$area_code[crime$Borough=="Newham"] = immig$New.Code[immig$Area=="Newham"]
crime$area_code[crime$Borough=="Redbridge"] = immig$New.Code[immig$Area=="Redbridge"]
crime$area_code[crime$Borough=="Richmond upon Thames"] = immig$New.Code[immig$Area=="Richmond upon Thames"]
crime$area_code[crime$Borough=="Southwark"] = immig$New.Code[immig$Area=="Southwark"]
crime$area_code[crime$Borough=="Sutton"] = immig$New.Code[immig$Area=="Sutton"]
crime$area_code[crime$Borough=="Tower Hamlets"] = immig$New.Code[immig$Area=="Tower Hamlets"]
crime$area_code[crime$Borough=="Waltham Forest"] = immig$New.Code[immig$Area=="Waltham Forest"]
crime$area_code[crime$Borough=="Wandsworth"] = immig$New.Code[immig$Area=="Wandsworth"]
crime$area_code[crime$Borough=="Westminster"] = immig$New.Code[immig$Area=="Westminster"]

#let's make this area code variable a factor (because it is a categorical variable)
crime$area_code = factor(crime$area_code )
# look how now we have as many area codes as boroughs in our dataset, it already looks a lot clearner
#str(crime)

#Joining Datasets
#Crime's dataset has 122k observations where each lign is a type of criminal offence in a specific borough and the number of its occurences throughout the 12 months of each year from 2008 to 2018. The nino dataset has 33 observations where each line is a borough and the number of entering immigrants to that specific borough each year from 2008 to 2018.
#We will add 11 variables to the Crime dataset called "NbImmig2008", "NbImmig2009", ...,"NbImmig2018". To each observation in Crime where the Area is equal to one of the 33 Boroughs in the Nino dataset, we will add the number of immigrants for each year from 2008 to 2018.
#Now of course the number of immigrants during the first month of 2007 could be a lot less than the number of immigrants entering the borough on month 12 of 2007, but we are doing for the sake of having more predictive variables.

crime$NbImmig2008[crime$Borough=="City of London"] = immig$X2008[immig$Area == "City of London"]
crime$NbImmig2009[crime$Borough=="City of London"] = immig$X2009[immig$Area == "City of London"]
crime$NbImmig2010[crime$Borough=="City of London"] = immig$X2010[immig$Area == "City of London"]
crime$NbImmig2011[crime$Borough=="City of London"] = immig$X2011[immig$Area == "City of London"]
crime$NbImmig2012[crime$Borough=="City of London"] = immig$X2012[immig$Area == "City of London"]
crime$NbImmig2013[crime$Borough=="City of London"] = immig$X2013[immig$Area == "City of London"]
crime$NbImmig2014[crime$Borough=="City of London"] = immig$X2014[immig$Area == "City of London"]
crime$NbImmig2015[crime$Borough=="City of London"] = immig$X2015[immig$Area == "City of London"]
crime$NbImmig2016[crime$Borough=="City of London"] = immig$X2016[immig$Area == "City of London"]
crime$NbImmig2017[crime$Borough=="City of London"] = immig$X2017[immig$Area == "City of London"]
crime$NbImmig2018[crime$Borough=="City of London"] = immig$X2018[immig$Area == "City of London"]

crime$NbImmig2008[crime$Borough=="Barking and Dagenham"] = immig$X2008[immig$Area == "Barking and Dagenham"]
crime$NbImmig2009[crime$Borough=="Barking and Dagenham"] = immig$X2009[immig$Area == "Barking and Dagenham"]
crime$NbImmig2010[crime$Borough=="Barking and Dagenham"] = immig$X2010[immig$Area == "Barking and Dagenham"]
crime$NbImmig2011[crime$Borough=="Barking and Dagenham"] = immig$X2011[immig$Area == "Barking and Dagenham"]
crime$NbImmig2012[crime$Borough=="Barking and Dagenham"] = immig$X2012[immig$Area == "Barking and Dagenham"]
crime$NbImmig2013[crime$Borough=="Barking and Dagenham"] = immig$X2013[immig$Area == "Barking and Dagenham"]
crime$NbImmig2014[crime$Borough=="Barking and Dagenham"] = immig$X2014[immig$Area == "Barking and Dagenham"]
crime$NbImmig2015[crime$Borough=="Barking and Dagenham"] = immig$X2015[immig$Area == "Barking and Dagenham"]
crime$NbImmig2016[crime$Borough=="Barking and Dagenham"] = immig$X2016[immig$Area == "Barking and Dagenham"]
crime$NbImmig2017[crime$Borough=="Barking and Dagenham"] = immig$X2017[immig$Area == "Barking and Dagenham"]
crime$NbImmig2018[crime$Borough=="Barking and Dagenham"] = immig$X2018[immig$Area == "Barking and Dagenham"]

crime$NbImmig2008[crime$Borough=="Barnet"] = immig$X2008[immig$Area == "Barnet"]
crime$NbImmig2009[crime$Borough=="Barnet"] = immig$X2009[immig$Area == "Barnet"]
crime$NbImmig2010[crime$Borough=="Barnet"] = immig$X2010[immig$Area == "Barnet"]
crime$NbImmig2011[crime$Borough=="Barnet"] = immig$X2011[immig$Area == "Barnet"]
crime$NbImmig2012[crime$Borough=="Barnet"] = immig$X2012[immig$Area == "Barnet"]
crime$NbImmig2013[crime$Borough=="Barnet"] = immig$X2013[immig$Area == "Barnet"]
crime$NbImmig2014[crime$Borough=="Barnet"] = immig$X2014[immig$Area == "Barnet"]
crime$NbImmig2015[crime$Borough=="Barnet"] = immig$X2015[immig$Area == "Barnet"]
crime$NbImmig2016[crime$Borough=="Barnet"] = immig$X2016[immig$Area == "Barnet"]
crime$NbImmig2017[crime$Borough=="Barnet"] = immig$X2017[immig$Area == "Barnet"]
crime$NbImmig2018[crime$Borough=="Barnet"] = immig$X2018[immig$Area == "Barnet"]

crime$NbImmig2008[crime$Borough=="Bexley"] = immig$X2008[immig$Area == "Bexley"]
crime$NbImmig2009[crime$Borough=="Bexley"] = immig$X2009[immig$Area == "Bexley"]
crime$NbImmig2010[crime$Borough=="Bexley"] = immig$X2010[immig$Area == "Bexley"]
crime$NbImmig2011[crime$Borough=="Bexley"] = immig$X2011[immig$Area == "Bexley"]
crime$NbImmig2012[crime$Borough=="Bexley"] = immig$X2012[immig$Area == "Bexley"]
crime$NbImmig2013[crime$Borough=="Bexley"] = immig$X2013[immig$Area == "Bexley"]
crime$NbImmig2014[crime$Borough=="Bexley"] = immig$X2014[immig$Area == "Bexley"]
crime$NbImmig2015[crime$Borough=="Bexley"] = immig$X2015[immig$Area == "Bexley"]
crime$NbImmig2016[crime$Borough=="Bexley"] = immig$X2016[immig$Area == "Bexley"]
crime$NbImmig2017[crime$Borough=="Bexley"] = immig$X2017[immig$Area == "Bexley"]
crime$NbImmig2018[crime$Borough=="Bexley"] = immig$X2018[immig$Area == "Bexley"]

crime$NbImmig2008[crime$Borough=="Brent"] = immig$X2008[immig$Area == "Brent"]
crime$NbImmig2009[crime$Borough=="Brent"] = immig$X2009[immig$Area == "Brent"]
crime$NbImmig2010[crime$Borough=="Brent"] = immig$X2010[immig$Area == "Brent"]
crime$NbImmig2011[crime$Borough=="Brent"] = immig$X2011[immig$Area == "Brent"]
crime$NbImmig2012[crime$Borough=="Brent"] = immig$X2012[immig$Area == "Brent"]
crime$NbImmig2013[crime$Borough=="Brent"] = immig$X2013[immig$Area == "Brent"]
crime$NbImmig2014[crime$Borough=="Brent"] = immig$X2014[immig$Area == "Brent"]
crime$NbImmig2015[crime$Borough=="Brent"] = immig$X2015[immig$Area == "Brent"]
crime$NbImmig2016[crime$Borough=="Brent"] = immig$X2016[immig$Area == "Brent"]
crime$NbImmig2017[crime$Borough=="Brent"] = immig$X2017[immig$Area == "Brent"]
crime$NbImmig2018[crime$Borough=="Brent"] = immig$X2018[immig$Area == "Brent"]

crime$NbImmig2008[crime$Borough=="Bromley"] = immig$X2008[immig$Area == "Bromley"]
crime$NbImmig2009[crime$Borough=="Bromley"] = immig$X2009[immig$Area == "Bromley"]
crime$NbImmig2010[crime$Borough=="Bromley"] = immig$X2010[immig$Area == "Bromley"]
crime$NbImmig2011[crime$Borough=="Bromley"] = immig$X2011[immig$Area == "Bromley"]
crime$NbImmig2012[crime$Borough=="Bromley"] = immig$X2012[immig$Area == "Bromley"]
crime$NbImmig2013[crime$Borough=="Bromley"] = immig$X2013[immig$Area == "Bromley"]
crime$NbImmig2014[crime$Borough=="Bromley"] = immig$X2014[immig$Area == "Bromley"]
crime$NbImmig2015[crime$Borough=="Bromley"] = immig$X2015[immig$Area == "Bromley"]
crime$NbImmig2016[crime$Borough=="Bromley"] = immig$X2016[immig$Area == "Bromley"]
crime$NbImmig2017[crime$Borough=="Bromley"] = immig$X2017[immig$Area == "Bromley"]
crime$NbImmig2018[crime$Borough=="Bromley"] = immig$X2018[immig$Area == "Bromley"]

crime$NbImmig2008[crime$Borough=="Camden"] = immig$X2008[immig$Area == "Camden"]
crime$NbImmig2009[crime$Borough=="Camden"] = immig$X2009[immig$Area == "Camden"]
crime$NbImmig2010[crime$Borough=="Camden"] = immig$X2010[immig$Area == "Camden"]
crime$NbImmig2011[crime$Borough=="Camden"] = immig$X2011[immig$Area == "Camden"]
crime$NbImmig2012[crime$Borough=="Camden"] = immig$X2012[immig$Area == "Camden"]
crime$NbImmig2013[crime$Borough=="Camden"] = immig$X2013[immig$Area == "Camden"]
crime$NbImmig2014[crime$Borough=="Camden"] = immig$X2014[immig$Area == "Camden"]
crime$NbImmig2015[crime$Borough=="Camden"] = immig$X2015[immig$Area == "Camden"]
crime$NbImmig2016[crime$Borough=="Camden"] = immig$X2016[immig$Area == "Camden"]
crime$NbImmig2017[crime$Borough=="Camden"] = immig$X2017[immig$Area == "Camden"]
crime$NbImmig2018[crime$Borough=="Camden"] = immig$X2018[immig$Area == "Camden"]

crime$NbImmig2008[crime$Borough=="Croydon"] = immig$X2008[immig$Area == "Croydon"]
crime$NbImmig2009[crime$Borough=="Croydon"] = immig$X2009[immig$Area == "Croydon"]
crime$NbImmig2010[crime$Borough=="Croydon"] = immig$X2010[immig$Area == "Croydon"]
crime$NbImmig2011[crime$Borough=="Croydon"] = immig$X2011[immig$Area == "Croydon"]
crime$NbImmig2012[crime$Borough=="Croydon"] = immig$X2012[immig$Area == "Croydon"]
crime$NbImmig2013[crime$Borough=="Croydon"] = immig$X2013[immig$Area == "Croydon"]
crime$NbImmig2014[crime$Borough=="Croydon"] = immig$X2014[immig$Area == "Croydon"]
crime$NbImmig2015[crime$Borough=="Croydon"] = immig$X2015[immig$Area == "Croydon"]
crime$NbImmig2016[crime$Borough=="Croydon"] = immig$X2016[immig$Area == "Croydon"]
crime$NbImmig2017[crime$Borough=="Croydon"] = immig$X2017[immig$Area == "Croydon"]
crime$NbImmig2018[crime$Borough=="Croydon"] = immig$X2018[immig$Area == "Croydon"]

crime$NbImmig2008[crime$Borough=="Ealing"] = immig$X2008[immig$Area == "Ealing"]
crime$NbImmig2009[crime$Borough=="Ealing"] = immig$X2009[immig$Area == "Ealing"]
crime$NbImmig2010[crime$Borough=="Ealing"] = immig$X2010[immig$Area == "Ealing"]
crime$NbImmig2011[crime$Borough=="Ealing"] = immig$X2011[immig$Area == "Ealing"]
crime$NbImmig2012[crime$Borough=="Ealing"] = immig$X2012[immig$Area == "Ealing"]
crime$NbImmig2013[crime$Borough=="Ealing"] = immig$X2013[immig$Area == "Ealing"]
crime$NbImmig2014[crime$Borough=="Ealing"] = immig$X2014[immig$Area == "Ealing"]
crime$NbImmig2015[crime$Borough=="Ealing"] = immig$X2015[immig$Area == "Ealing"]
crime$NbImmig2016[crime$Borough=="Ealing"] = immig$X2016[immig$Area == "Ealing"]
crime$NbImmig2017[crime$Borough=="Ealing"] = immig$X2017[immig$Area == "Ealing"]
crime$NbImmig2018[crime$Borough=="Ealing"] = immig$X2018[immig$Area == "Ealing"]

crime$NbImmig2008[crime$Borough=="Enfield"] = immig$X2008[immig$Area == "Enfield"]
crime$NbImmig2009[crime$Borough=="Enfield"] = immig$X2009[immig$Area == "Enfield"]
crime$NbImmig2010[crime$Borough=="Enfield"] = immig$X2010[immig$Area == "Enfield"]
crime$NbImmig2011[crime$Borough=="Enfield"] = immig$X2011[immig$Area == "Enfield"]
crime$NbImmig2012[crime$Borough=="Enfield"] = immig$X2012[immig$Area == "Enfield"]
crime$NbImmig2013[crime$Borough=="Enfield"] = immig$X2013[immig$Area == "Enfield"]
crime$NbImmig2014[crime$Borough=="Enfield"] = immig$X2014[immig$Area == "Enfield"]
crime$NbImmig2015[crime$Borough=="Enfield"] = immig$X2015[immig$Area == "Enfield"]
crime$NbImmig2016[crime$Borough=="Enfield"] = immig$X2016[immig$Area == "Enfield"]
crime$NbImmig2017[crime$Borough=="Enfield"] = immig$X2017[immig$Area == "Enfield"]
crime$NbImmig2018[crime$Borough=="Enfield"] = immig$X2018[immig$Area == "Enfield"]

crime$NbImmig2008[crime$Borough=="Greenwich"] = immig$X2008[immig$Area == "Greenwich"]
crime$NbImmig2009[crime$Borough=="Greenwich"] = immig$X2009[immig$Area == "Greenwich"]
crime$NbImmig2010[crime$Borough=="Greenwich"] = immig$X2010[immig$Area == "Greenwich"]
crime$NbImmig2011[crime$Borough=="Greenwich"] = immig$X2011[immig$Area == "Greenwich"]
crime$NbImmig2012[crime$Borough=="Greenwich"] = immig$X2012[immig$Area == "Greenwich"]
crime$NbImmig2013[crime$Borough=="Greenwich"] = immig$X2013[immig$Area == "Greenwich"]
crime$NbImmig2014[crime$Borough=="Greenwich"] = immig$X2014[immig$Area == "Greenwich"]
crime$NbImmig2015[crime$Borough=="Greenwich"] = immig$X2015[immig$Area == "Greenwich"]
crime$NbImmig2016[crime$Borough=="Greenwich"] = immig$X2016[immig$Area == "Greenwich"]
crime$NbImmig2017[crime$Borough=="Greenwich"] = immig$X2017[immig$Area == "Greenwich"]
crime$NbImmig2018[crime$Borough=="Greenwich"] = immig$X2018[immig$Area == "Greenwich"]

crime$NbImmig2008[crime$Borough=="Hackney"] = immig$X2008[immig$Area == "Hackney"]
crime$NbImmig2009[crime$Borough=="Hackney"] = immig$X2009[immig$Area == "Hackney"]
crime$NbImmig2010[crime$Borough=="Hackney"] = immig$X2010[immig$Area == "Hackney"]
crime$NbImmig2011[crime$Borough=="Hackney"] = immig$X2011[immig$Area == "Hackney"]
crime$NbImmig2012[crime$Borough=="Hackney"] = immig$X2012[immig$Area == "Hackney"]
crime$NbImmig2013[crime$Borough=="Hackney"] = immig$X2013[immig$Area == "Hackney"]
crime$NbImmig2014[crime$Borough=="Hackney"] = immig$X2014[immig$Area == "Hackney"]
crime$NbImmig2015[crime$Borough=="Hackney"] = immig$X2015[immig$Area == "Hackney"]
crime$NbImmig2016[crime$Borough=="Hackney"] = immig$X2016[immig$Area == "Hackney"]
crime$NbImmig2017[crime$Borough=="Hackney"] = immig$X2017[immig$Area == "Hackney"]
crime$NbImmig2018[crime$Borough=="Hackney"] = immig$X2018[immig$Area == "Hackney"]

crime$NbImmig2008[crime$Borough=="Hammersmith and Fulham"] = immig$X2008[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2009[crime$Borough=="Hammersmith and Fulham"] = immig$X2009[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2010[crime$Borough=="Hammersmith and Fulham"] = immig$X2010[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2011[crime$Borough=="Hammersmith and Fulham"] = immig$X2011[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2012[crime$Borough=="Hammersmith and Fulham"] = immig$X2012[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2013[crime$Borough=="Hammersmith and Fulham"] = immig$X2013[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2014[crime$Borough=="Hammersmith and Fulham"] = immig$X2014[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2015[crime$Borough=="Hammersmith and Fulham"] = immig$X2015[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2016[crime$Borough=="Hammersmith and Fulham"] = immig$X2016[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2017[crime$Borough=="Hammersmith and Fulham"] = immig$X2017[immig$Area == "Hammersmith and Fulham"]
crime$NbImmig2018[crime$Borough=="Hammersmith and Fulham"] = immig$X2018[immig$Area == "Hammersmith and Fulham"]

crime$NbImmig2008[crime$Borough=="Haringey"] = immig$X2008[immig$Area == "Haringey"]
crime$NbImmig2009[crime$Borough=="Haringey"] = immig$X2009[immig$Area == "Haringey"]
crime$NbImmig2010[crime$Borough=="Haringey"] = immig$X2010[immig$Area == "Haringey"]
crime$NbImmig2011[crime$Borough=="Haringey"] = immig$X2011[immig$Area == "Haringey"]
crime$NbImmig2012[crime$Borough=="Haringey"] = immig$X2012[immig$Area == "Haringey"]
crime$NbImmig2013[crime$Borough=="Haringey"] = immig$X2013[immig$Area == "Haringey"]
crime$NbImmig2014[crime$Borough=="Haringey"] = immig$X2014[immig$Area == "Haringey"]
crime$NbImmig2015[crime$Borough=="Haringey"] = immig$X2015[immig$Area == "Haringey"]
crime$NbImmig2016[crime$Borough=="Haringey"] = immig$X2016[immig$Area == "Haringey"]
crime$NbImmig2017[crime$Borough=="Haringey"] = immig$X2017[immig$Area == "Haringey"]
crime$NbImmig2018[crime$Borough=="Haringey"] = immig$X2018[immig$Area == "Haringey"]

crime$NbImmig2008[crime$Borough=="Harrow"] = immig$X2008[immig$Area == "Harrow"]
crime$NbImmig2009[crime$Borough=="Harrow"] = immig$X2009[immig$Area == "Harrow"]
crime$NbImmig2010[crime$Borough=="Harrow"] = immig$X2010[immig$Area == "Harrow"]
crime$NbImmig2011[crime$Borough=="Harrow"] = immig$X2011[immig$Area == "Harrow"]
crime$NbImmig2012[crime$Borough=="Harrow"] = immig$X2012[immig$Area == "Harrow"]
crime$NbImmig2013[crime$Borough=="Harrow"] = immig$X2013[immig$Area == "Harrow"]
crime$NbImmig2014[crime$Borough=="Harrow"] = immig$X2014[immig$Area == "Harrow"]
crime$NbImmig2015[crime$Borough=="Harrow"] = immig$X2015[immig$Area == "Harrow"]
crime$NbImmig2016[crime$Borough=="Harrow"] = immig$X2016[immig$Area == "Harrow"]
crime$NbImmig2017[crime$Borough=="Harrow"] = immig$X2017[immig$Area == "Harrow"]
crime$NbImmig2018[crime$Borough=="Harrow"] = immig$X2018[immig$Area == "Harrow"]

crime$NbImmig2008[crime$Borough=="Havering"] = immig$X2008[immig$Area == "Havering"]
crime$NbImmig2009[crime$Borough=="Havering"] = immig$X2009[immig$Area == "Havering"]
crime$NbImmig2010[crime$Borough=="Havering"] = immig$X2010[immig$Area == "Havering"]
crime$NbImmig2011[crime$Borough=="Havering"] = immig$X2011[immig$Area == "Havering"]
crime$NbImmig2012[crime$Borough=="Havering"] = immig$X2012[immig$Area == "Havering"]
crime$NbImmig2013[crime$Borough=="Havering"] = immig$X2013[immig$Area == "Havering"]
crime$NbImmig2014[crime$Borough=="Havering"] = immig$X2014[immig$Area == "Havering"]
crime$NbImmig2015[crime$Borough=="Havering"] = immig$X2015[immig$Area == "Havering"]
crime$NbImmig2016[crime$Borough=="Havering"] = immig$X2016[immig$Area == "Havering"]
crime$NbImmig2017[crime$Borough=="Havering"] = immig$X2017[immig$Area == "Havering"]
crime$NbImmig2018[crime$Borough=="Havering"] = immig$X2018[immig$Area == "Havering"]

crime$NbImmig2008[crime$Borough=="Hillingdon"] = immig$X2008[immig$Area == "Hillingdon"]
crime$NbImmig2009[crime$Borough=="Hillingdon"] = immig$X2009[immig$Area == "Hillingdon"]
crime$NbImmig2010[crime$Borough=="Hillingdon"] = immig$X2010[immig$Area == "Hillingdon"]
crime$NbImmig2011[crime$Borough=="Hillingdon"] = immig$X2011[immig$Area == "Hillingdon"]
crime$NbImmig2012[crime$Borough=="Hillingdon"] = immig$X2012[immig$Area == "Hillingdon"]
crime$NbImmig2013[crime$Borough=="Hillingdon"] = immig$X2013[immig$Area == "Hillingdon"]
crime$NbImmig2014[crime$Borough=="Hillingdon"] = immig$X2014[immig$Area == "Hillingdon"]
crime$NbImmig2015[crime$Borough=="Hillingdon"] = immig$X2015[immig$Area == "Hillingdon"]
crime$NbImmig2016[crime$Borough=="Hillingdon"] = immig$X2016[immig$Area == "Hillingdon"]
crime$NbImmig2017[crime$Borough=="Hillingdon"] = immig$X2017[immig$Area == "Hillingdon"]
crime$NbImmig2018[crime$Borough=="Hillingdon"] = immig$X2018[immig$Area == "Hillingdon"]

crime$NbImmig2008[crime$Borough=="Hounslow"] = immig$X2008[immig$Area == "Hounslow"]
crime$NbImmig2009[crime$Borough=="Hounslow"] = immig$X2009[immig$Area == "Hounslow"]
crime$NbImmig2010[crime$Borough=="Hounslow"] = immig$X2010[immig$Area == "Hounslow"]
crime$NbImmig2011[crime$Borough=="Hounslow"] = immig$X2011[immig$Area == "Hounslow"]
crime$NbImmig2012[crime$Borough=="Hounslow"] = immig$X2012[immig$Area == "Hounslow"]
crime$NbImmig2013[crime$Borough=="Hounslow"] = immig$X2013[immig$Area == "Hounslow"]
crime$NbImmig2014[crime$Borough=="Hounslow"] = immig$X2014[immig$Area == "Hounslow"]
crime$NbImmig2015[crime$Borough=="Hounslow"] = immig$X2015[immig$Area == "Hounslow"]
crime$NbImmig2016[crime$Borough=="Hounslow"] = immig$X2016[immig$Area == "Hounslow"]
crime$NbImmig2017[crime$Borough=="Hounslow"] = immig$X2017[immig$Area == "Hounslow"]
crime$NbImmig2018[crime$Borough=="Hounslow"] = immig$X2018[immig$Area == "Hounslow"]

crime$NbImmig2008[crime$Borough=="Islington"] = immig$X2008[immig$Area == "Islington"]
crime$NbImmig2009[crime$Borough=="Islington"] = immig$X2009[immig$Area == "Islington"]
crime$NbImmig2010[crime$Borough=="Islington"] = immig$X2010[immig$Area == "Islington"]
crime$NbImmig2011[crime$Borough=="Islington"] = immig$X2011[immig$Area == "Islington"]
crime$NbImmig2012[crime$Borough=="Islington"] = immig$X2012[immig$Area == "Islington"]
crime$NbImmig2013[crime$Borough=="Islington"] = immig$X2013[immig$Area == "Islington"]
crime$NbImmig2014[crime$Borough=="Islington"] = immig$X2014[immig$Area == "Islington"]
crime$NbImmig2015[crime$Borough=="Islington"] = immig$X2015[immig$Area == "Islington"]
crime$NbImmig2016[crime$Borough=="Islington"] = immig$X2016[immig$Area == "Islington"]
crime$NbImmig2017[crime$Borough=="Islington"] = immig$X2017[immig$Area == "Islington"]
crime$NbImmig2018[crime$Borough=="Islington"] = immig$X2018[immig$Area == "Islington"]

crime$NbImmig2008[crime$Borough=="Kensington and Chelsea"] = immig$X2008[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2009[crime$Borough=="Kensington and Chelsea"] = immig$X2009[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2010[crime$Borough=="Kensington and Chelsea"] = immig$X2010[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2011[crime$Borough=="Kensington and Chelsea"] = immig$X2011[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2012[crime$Borough=="Kensington and Chelsea"] = immig$X2012[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2013[crime$Borough=="Kensington and Chelsea"] = immig$X2013[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2014[crime$Borough=="Kensington and Chelsea"] = immig$X2014[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2015[crime$Borough=="Kensington and Chelsea"] = immig$X2015[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2016[crime$Borough=="Kensington and Chelsea"] = immig$X2016[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2017[crime$Borough=="Kensington and Chelsea"] = immig$X2017[immig$Area == "Kensington and Chelsea"]
crime$NbImmig2018[crime$Borough=="Kensington and Chelsea"] = immig$X2018[immig$Area == "Kensington and Chelsea"]

crime$NbImmig2008[crime$Borough=="Kingston upon Thames"] = immig$X2008[immig$Area == "Kingston upon Thames"]
crime$NbImmig2009[crime$Borough=="Kingston upon Thames"] = immig$X2009[immig$Area == "Kingston upon Thames"]
crime$NbImmig2010[crime$Borough=="Kingston upon Thames"] = immig$X2010[immig$Area == "Kingston upon Thames"]
crime$NbImmig2011[crime$Borough=="Kingston upon Thames"] = immig$X2011[immig$Area == "Kingston upon Thames"]
crime$NbImmig2012[crime$Borough=="Kingston upon Thames"] = immig$X2012[immig$Area == "Kingston upon Thames"]
crime$NbImmig2013[crime$Borough=="Kingston upon Thames"] = immig$X2013[immig$Area == "Kingston upon Thames"]
crime$NbImmig2014[crime$Borough=="Kingston upon Thames"] = immig$X2014[immig$Area == "Kingston upon Thames"]
crime$NbImmig2015[crime$Borough=="Kingston upon Thames"] = immig$X2015[immig$Area == "Kingston upon Thames"]
crime$NbImmig2016[crime$Borough=="Kingston upon Thames"] = immig$X2016[immig$Area == "Kingston upon Thames"]
crime$NbImmig2017[crime$Borough=="Kingston upon Thames"] = immig$X2017[immig$Area == "Kingston upon Thames"]
crime$NbImmig2018[crime$Borough=="Kingston upon Thames"] = immig$X2018[immig$Area == "Kingston upon Thames"]

crime$NbImmig2008[crime$Borough=="Lambeth"] = immig$X2008[immig$Area == "Lambeth"]
crime$NbImmig2009[crime$Borough=="Lambeth"] = immig$X2009[immig$Area == "Lambeth"]
crime$NbImmig2010[crime$Borough=="Lambeth"] = immig$X2010[immig$Area == "Lambeth"]
crime$NbImmig2011[crime$Borough=="Lambeth"] = immig$X2011[immig$Area == "Lambeth"]
crime$NbImmig2012[crime$Borough=="Lambeth"] = immig$X2012[immig$Area == "Lambeth"]
crime$NbImmig2013[crime$Borough=="Lambeth"] = immig$X2013[immig$Area == "Lambeth"]
crime$NbImmig2014[crime$Borough=="Lambeth"] = immig$X2014[immig$Area == "Lambeth"]
crime$NbImmig2015[crime$Borough=="Lambeth"] = immig$X2015[immig$Area == "Lambeth"]
crime$NbImmig2016[crime$Borough=="Lambeth"] = immig$X2016[immig$Area == "Lambeth"]
crime$NbImmig2017[crime$Borough=="Lambeth"] = immig$X2017[immig$Area == "Lambeth"]
crime$NbImmig2018[crime$Borough=="Lambeth"] = immig$X2018[immig$Area == "Lambeth"]

crime$NbImmig2008[crime$Borough=="Lewisham"] = immig$X2008[immig$Area == "Lewisham"]
crime$NbImmig2009[crime$Borough=="Lewisham"] = immig$X2009[immig$Area == "Lewisham"]
crime$NbImmig2010[crime$Borough=="Lewisham"] = immig$X2010[immig$Area == "Lewisham"]
crime$NbImmig2011[crime$Borough=="Lewisham"] = immig$X2011[immig$Area == "Lewisham"]
crime$NbImmig2012[crime$Borough=="Lewisham"] = immig$X2012[immig$Area == "Lewisham"]
crime$NbImmig2013[crime$Borough=="Lewisham"] = immig$X2013[immig$Area == "Lewisham"]
crime$NbImmig2014[crime$Borough=="Lewisham"] = immig$X2014[immig$Area == "Lewisham"]
crime$NbImmig2015[crime$Borough=="Lewisham"] = immig$X2015[immig$Area == "Lewisham"]
crime$NbImmig2016[crime$Borough=="Lewisham"] = immig$X2016[immig$Area == "Lewisham"]
crime$NbImmig2017[crime$Borough=="Lewisham"] = immig$X2017[immig$Area == "Lewisham"]
crime$NbImmig2018[crime$Borough=="Lewisham"] = immig$X2018[immig$Area == "Lewisham"]

crime$NbImmig2008[crime$Borough=="Merton"] = immig$X2008[immig$Area == "Merton"]
crime$NbImmig2009[crime$Borough=="Merton"] = immig$X2009[immig$Area == "Merton"]
crime$NbImmig2010[crime$Borough=="Merton"] = immig$X2010[immig$Area == "Merton"]
crime$NbImmig2011[crime$Borough=="Merton"] = immig$X2011[immig$Area == "Merton"]
crime$NbImmig2012[crime$Borough=="Merton"] = immig$X2012[immig$Area == "Merton"]
crime$NbImmig2013[crime$Borough=="Merton"] = immig$X2013[immig$Area == "Merton"]
crime$NbImmig2014[crime$Borough=="Merton"] = immig$X2014[immig$Area == "Merton"]
crime$NbImmig2015[crime$Borough=="Merton"] = immig$X2015[immig$Area == "Merton"]
crime$NbImmig2016[crime$Borough=="Merton"] = immig$X2016[immig$Area == "Merton"]
crime$NbImmig2017[crime$Borough=="Merton"] = immig$X2017[immig$Area == "Merton"]
crime$NbImmig2018[crime$Borough=="Merton"] = immig$X2018[immig$Area == "Merton"]

crime$NbImmig2008[crime$Borough=="Newham"] = immig$X2008[immig$Area == "Newham"]
crime$NbImmig2009[crime$Borough=="Newham"] = immig$X2009[immig$Area == "Newham"]
crime$NbImmig2010[crime$Borough=="Newham"] = immig$X2010[immig$Area == "Newham"]
crime$NbImmig2011[crime$Borough=="Newham"] = immig$X2011[immig$Area == "Newham"]
crime$NbImmig2012[crime$Borough=="Newham"] = immig$X2012[immig$Area == "Newham"]
crime$NbImmig2013[crime$Borough=="Newham"] = immig$X2013[immig$Area == "Newham"]
crime$NbImmig2014[crime$Borough=="Newham"] = immig$X2014[immig$Area == "Newham"]
crime$NbImmig2015[crime$Borough=="Newham"] = immig$X2015[immig$Area == "Newham"]
crime$NbImmig2016[crime$Borough=="Newham"] = immig$X2016[immig$Area == "Newham"]
crime$NbImmig2017[crime$Borough=="Newham"] = immig$X2017[immig$Area == "Newham"]
crime$NbImmig2018[crime$Borough=="Newham"] = immig$X2018[immig$Area == "Newham"]

crime$NbImmig2008[crime$Borough=="Redbridge"] = immig$X2008[immig$Area == "Redbridge"]
crime$NbImmig2009[crime$Borough=="Redbridge"] = immig$X2009[immig$Area == "Redbridge"]
crime$NbImmig2010[crime$Borough=="Redbridge"] = immig$X2010[immig$Area == "Redbridge"]
crime$NbImmig2011[crime$Borough=="Redbridge"] = immig$X2011[immig$Area == "Redbridge"]
crime$NbImmig2012[crime$Borough=="Redbridge"] = immig$X2012[immig$Area == "Redbridge"]
crime$NbImmig2013[crime$Borough=="Redbridge"] = immig$X2013[immig$Area == "Redbridge"]
crime$NbImmig2014[crime$Borough=="Redbridge"] = immig$X2014[immig$Area == "Redbridge"]
crime$NbImmig2015[crime$Borough=="Redbridge"] = immig$X2015[immig$Area == "Redbridge"]
crime$NbImmig2016[crime$Borough=="Redbridge"] = immig$X2016[immig$Area == "Redbridge"]
crime$NbImmig2017[crime$Borough=="Redbridge"] = immig$X2017[immig$Area == "Redbridge"]
crime$NbImmig2018[crime$Borough=="Redbridge"] = immig$X2018[immig$Area == "Redbridge"]

crime$NbImmig2008[crime$Borough=="Richmond upon Thames"] = immig$X2008[immig$Area == "Richmond upon Thames"]
crime$NbImmig2009[crime$Borough=="Richmond upon Thames"] = immig$X2009[immig$Area == "Richmond upon Thames"]
crime$NbImmig2010[crime$Borough=="Richmond upon Thames"] = immig$X2010[immig$Area == "Richmond upon Thames"]
crime$NbImmig2011[crime$Borough=="Richmond upon Thames"] = immig$X2011[immig$Area == "Richmond upon Thames"]
crime$NbImmig2012[crime$Borough=="Richmond upon Thames"] = immig$X2012[immig$Area == "Richmond upon Thames"]
crime$NbImmig2013[crime$Borough=="Richmond upon Thames"] = immig$X2013[immig$Area == "Richmond upon Thames"]
crime$NbImmig2014[crime$Borough=="Richmond upon Thames"] = immig$X2014[immig$Area == "Richmond upon Thames"]
crime$NbImmig2015[crime$Borough=="Richmond upon Thames"] = immig$X2015[immig$Area == "Richmond upon Thames"]
crime$NbImmig2016[crime$Borough=="Richmond upon Thames"] = immig$X2016[immig$Area == "Richmond upon Thames"]
crime$NbImmig2017[crime$Borough=="Richmond upon Thames"] = immig$X2017[immig$Area == "Richmond upon Thames"]
crime$NbImmig2018[crime$Borough=="Richmond upon Thames"] = immig$X2018[immig$Area == "Richmond upon Thames"]

crime$NbImmig2008[crime$Borough=="Southwark"] = immig$X2008[immig$Area == "Southwark"]
crime$NbImmig2009[crime$Borough=="Southwark"] = immig$X2009[immig$Area == "Southwark"]
crime$NbImmig2010[crime$Borough=="Southwark"] = immig$X2010[immig$Area == "Southwark"]
crime$NbImmig2011[crime$Borough=="Southwark"] = immig$X2011[immig$Area == "Southwark"]
crime$NbImmig2012[crime$Borough=="Southwark"] = immig$X2012[immig$Area == "Southwark"]
crime$NbImmig2013[crime$Borough=="Southwark"] = immig$X2013[immig$Area == "Southwark"]
crime$NbImmig2014[crime$Borough=="Southwark"] = immig$X2014[immig$Area == "Southwark"]
crime$NbImmig2015[crime$Borough=="Southwark"] = immig$X2015[immig$Area == "Southwark"]
crime$NbImmig2016[crime$Borough=="Southwark"] = immig$X2016[immig$Area == "Southwark"]
crime$NbImmig2017[crime$Borough=="Southwark"] = immig$X2017[immig$Area == "Southwark"]
crime$NbImmig2018[crime$Borough=="Southwark"] = immig$X2018[immig$Area == "Southwark"]

crime$NbImmig2008[crime$Borough=="Sutton"] = immig$X2008[immig$Area == "Sutton"]
crime$NbImmig2009[crime$Borough=="Sutton"] = immig$X2009[immig$Area == "Sutton"]
crime$NbImmig2010[crime$Borough=="Sutton"] = immig$X2010[immig$Area == "Sutton"]
crime$NbImmig2011[crime$Borough=="Sutton"] = immig$X2011[immig$Area == "Sutton"]
crime$NbImmig2012[crime$Borough=="Sutton"] = immig$X2012[immig$Area == "Sutton"]
crime$NbImmig2013[crime$Borough=="Sutton"] = immig$X2013[immig$Area == "Sutton"]
crime$NbImmig2014[crime$Borough=="Sutton"] = immig$X2014[immig$Area == "Sutton"]
crime$NbImmig2015[crime$Borough=="Sutton"] = immig$X2015[immig$Area == "Sutton"]
crime$NbImmig2016[crime$Borough=="Sutton"] = immig$X2016[immig$Area == "Sutton"]
crime$NbImmig2017[crime$Borough=="Sutton"] = immig$X2017[immig$Area == "Sutton"]
crime$NbImmig2018[crime$Borough=="Sutton"] = immig$X2018[immig$Area == "Sutton"]

crime$NbImmig2008[crime$Borough=="Tower Hamlets"] = immig$X2008[immig$Area == "Tower Hamlets"]
crime$NbImmig2009[crime$Borough=="Tower Hamlets"] = immig$X2009[immig$Area == "Tower Hamlets"]
crime$NbImmig2010[crime$Borough=="Tower Hamlets"] = immig$X2010[immig$Area == "Tower Hamlets"]
crime$NbImmig2011[crime$Borough=="Tower Hamlets"] = immig$X2011[immig$Area == "Tower Hamlets"]
crime$NbImmig2012[crime$Borough=="Tower Hamlets"] = immig$X2012[immig$Area == "Tower Hamlets"]
crime$NbImmig2013[crime$Borough=="Tower Hamlets"] = immig$X2013[immig$Area == "Tower Hamlets"]
crime$NbImmig2014[crime$Borough=="Tower Hamlets"] = immig$X2014[immig$Area == "Tower Hamlets"]
crime$NbImmig2015[crime$Borough=="Tower Hamlets"] = immig$X2015[immig$Area == "Tower Hamlets"]
crime$NbImmig2016[crime$Borough=="Tower Hamlets"] = immig$X2016[immig$Area == "Tower Hamlets"]
crime$NbImmig2017[crime$Borough=="Tower Hamlets"] = immig$X2017[immig$Area == "Tower Hamlets"]
crime$NbImmig2018[crime$Borough=="Tower Hamlets"] = immig$X2018[immig$Area == "Tower Hamlets"]

crime$NbImmig2008[crime$Borough=="Waltham Forest"] = immig$X2008[immig$Area == "Waltham Forest"]
crime$NbImmig2009[crime$Borough=="Waltham Forest"] = immig$X2009[immig$Area == "Waltham Forest"]
crime$NbImmig2010[crime$Borough=="Waltham Forest"] = immig$X2010[immig$Area == "Waltham Forest"]
crime$NbImmig2011[crime$Borough=="Waltham Forest"] = immig$X2011[immig$Area == "Waltham Forest"]
crime$NbImmig2012[crime$Borough=="Waltham Forest"] = immig$X2012[immig$Area == "Waltham Forest"]
crime$NbImmig2013[crime$Borough=="Waltham Forest"] = immig$X2013[immig$Area == "Waltham Forest"]
crime$NbImmig2014[crime$Borough=="Waltham Forest"] = immig$X2014[immig$Area == "Waltham Forest"]
crime$NbImmig2015[crime$Borough=="Waltham Forest"] = immig$X2015[immig$Area == "Waltham Forest"]
crime$NbImmig2016[crime$Borough=="Waltham Forest"] = immig$X2016[immig$Area == "Waltham Forest"]
crime$NbImmig2017[crime$Borough=="Waltham Forest"] = immig$X2017[immig$Area == "Waltham Forest"]
crime$NbImmig2018[crime$Borough=="Waltham Forest"] = immig$X2018[immig$Area == "Waltham Forest"]

crime$NbImmig2008[crime$Borough=="Wandsworth"] = immig$X2008[immig$Area == "Wandsworth"]
crime$NbImmig2009[crime$Borough=="Wandsworth"] = immig$X2009[immig$Area == "Wandsworth"]
crime$NbImmig2010[crime$Borough=="Wandsworth"] = immig$X2010[immig$Area == "Wandsworth"]
crime$NbImmig2011[crime$Borough=="Wandsworth"] = immig$X2011[immig$Area == "Wandsworth"]
crime$NbImmig2012[crime$Borough=="Wandsworth"] = immig$X2012[immig$Area == "Wandsworth"]
crime$NbImmig2013[crime$Borough=="Wandsworth"] = immig$X2013[immig$Area == "Wandsworth"]
crime$NbImmig2014[crime$Borough=="Wandsworth"] = immig$X2014[immig$Area == "Wandsworth"]
crime$NbImmig2015[crime$Borough=="Wandsworth"] = immig$X2015[immig$Area == "Wandsworth"]
crime$NbImmig2016[crime$Borough=="Wandsworth"] = immig$X2016[immig$Area == "Wandsworth"]
crime$NbImmig2017[crime$Borough=="Wandsworth"] = immig$X2017[immig$Area == "Wandsworth"]
crime$NbImmig2018[crime$Borough=="Wandsworth"] = immig$X2018[immig$Area == "Wandsworth"]

crime$NbImmig2008[crime$Borough=="Westminster"] = immig$X2008[immig$Area == "Westminster"]
crime$NbImmig2009[crime$Borough=="Westminster"] = immig$X2009[immig$Area == "Westminster"]
crime$NbImmig2010[crime$Borough=="Westminster"] = immig$X2010[immig$Area == "Westminster"]
crime$NbImmig2011[crime$Borough=="Westminster"] = immig$X2011[immig$Area == "Westminster"]
crime$NbImmig2012[crime$Borough=="Westminster"] = immig$X2012[immig$Area == "Westminster"]
crime$NbImmig2013[crime$Borough=="Westminster"] = immig$X2013[immig$Area == "Westminster"]
crime$NbImmig2014[crime$Borough=="Westminster"] = immig$X2014[immig$Area == "Westminster"]
crime$NbImmig2015[crime$Borough=="Westminster"] = immig$X2015[immig$Area == "Westminster"]
crime$NbImmig2016[crime$Borough=="Westminster"] = immig$X2016[immig$Area == "Westminster"]
crime$NbImmig2017[crime$Borough=="Westminster"] = immig$X2017[immig$Area == "Westminster"]
crime$NbImmig2018[crime$Borough=="Westminster"] = immig$X2018[immig$Area == "Westminster"]

#The size of our dataset is too big.

#It isn't of great use to have the numbers of crime sprawled across each month of the year, we will therefore calculate a total number of crimes per year.

#That way we'll be able to use models later on in the next stages of our study on our dataset.

#The variables from all of the months from years 2017 and 2018 are characters instead of being integers. We must therefore correct their types

crime$X201701 = as.integer(crime$X201701)
crime$X201702 = as.integer(crime$X201702)
crime$X201703 = as.integer(crime$X201703)
crime$X201704 = as.integer(crime$X201704)
crime$X201705 = as.integer(crime$X201705)
crime$X201706 = as.integer(crime$X201706)
crime$X201707 = as.integer(crime$X201707)
crime$X201708 = as.integer(crime$X201708)
crime$X201709 = as.integer(crime$X201709)
crime$X201710 = as.integer(crime$X201710)
crime$X201711 = as.integer(crime$X201711)
crime$X201712 = as.integer(crime$X201712)

crime$X201801 = as.integer(crime$X201801)
crime$X201802 = as.integer(crime$X201802)
crime$X201803 = as.integer(crime$X201803)
crime$X201804 = as.integer(crime$X201804)
crime$X201805 = as.integer(crime$X201805)
crime$X201806 = as.integer(crime$X201806)
crime$X201807 = as.integer(crime$X201807)
crime$X201808 = as.integer(crime$X201808)
crime$X201809 = as.integer(crime$X201809)
crime$X201810 = as.integer(crime$X201810)
crime$X201811 = as.integer(crime$X201811)
crime$X201812 = as.integer(crime$X201812)

#let's start by creating a new dataframe that will contain only variables we'll use:

library(dplyr)

df_crime = select(crime,"area_code","Borough","Major.Category","Minor.Category", "NbImmig2008","NbImmig2009","NbImmig2010",
                  "NbImmig2011","NbImmig2012","NbImmig2013","NbImmig2014","NbImmig2015","NbImmig2016","NbImmig2017",
                  "NbImmig2018")
#now let's add the variables crimes20XX :
df_crime$crimes2008 = crime$X200801+crime$X200802+crime$X200803+crime$X200804+crime$X200805+crime$X200806+crime$X200807+crime$X200808+crime$X200809+crime$X200810+crime$X200811+crime$X200812
df_crime$crimes2009 = crime$X200901+crime$X200902+crime$X200903+crime$X200904+crime$X200905+crime$X200906+crime$X200907+crime$X200908+crime$X200909+crime$X200910+crime$X200911+crime$X200912 
df_crime$crimes2010 = crime$X201001+crime$X201002+crime$X201003+crime$X201004+crime$X201005+crime$X201006+crime$X201007+crime$X201008+crime$X201009+crime$X201010+crime$X201011+crime$X201012 
df_crime$crimes2011 = crime$X201101+crime$X201102+crime$X201103+crime$X201104+crime$X201105+crime$X201106+crime$X201107+crime$X201108+crime$X201109+crime$X201110+crime$X201111+crime$X201112 
df_crime$crimes2012 = crime$X201201+crime$X201202+crime$X201203+crime$X201204+crime$X201205+crime$X201206+crime$X201207+crime$X201208+crime$X201209+crime$X201210+crime$X201211+crime$X201212 
df_crime$crimes2013 = crime$X201301+crime$X201302+crime$X201303+crime$X201304+crime$X201305+crime$X201306+crime$X201307+crime$X201308+crime$X201309+crime$X201310+crime$X201311+crime$X201312 
df_crime$crimes2014 = crime$X201401+crime$X201402+crime$X201403+crime$X201404+crime$X201405+crime$X201406+crime$X201407+crime$X201408+crime$X201409+crime$X201410+crime$X201411+crime$X201412 
df_crime$crimes2015 = crime$X201501+crime$X201502+crime$X201503+crime$X201504+crime$X201505+crime$X201506+crime$X201507+crime$X201508+crime$X201509+crime$X201510+crime$X201511+crime$X201512 
df_crime$crimes2016 = crime$X201601+crime$X201602+crime$X201603+crime$X201604+crime$X201605+crime$X201606+crime$X201607+crime$X201608+crime$X201609+crime$X201610+crime$X201611+crime$X201612 
df_crime$crimes2017 = crime$X201701+crime$X201702+crime$X201703+crime$X201704+crime$X201705+crime$X201706+crime$X201707+crime$X201708+crime$X201709+crime$X201710+crime$X201711+crime$X201712 
df_crime$crimes2018 = crime$X201801+crime$X201802+crime$X201803+crime$X201804+crime$X201805+crime$X201806+crime$X201807+crime$X201808+crime$X201809+crime$X201810+crime$X201811+crime$X201812 


#What we have now is 122k observations representing a minor type of felony, which belongs to a major type of felony, in a specific borough, and how many times that minor type of felony has happened over the course of the years.

#Now that we've finished preparing our variables and that we've set set them all to the correct type, we're ready to take care of the NA's

#Taking care of NAs
colSums(is.na(df_crime))
#We can see from the code above that the year 2017 and 2018 are the years that contain missing values. at this point our Dataset is too big
gg_miss_var(df_crime)

#we've seen the number of missing data in each variable, let's see if there is an intersection (observations where
# both variables have missing values at the same time)
gg_miss_upset(df_crime)


# let's calculate percentages of missing data
p2017 = length(which(is.na(df_crime$crimes2017)))/nrow(df_crime) * 100
p2018 = length(which(is.na(df_crime$crimes2018)))/nrow(df_crime) * 100

cat("crimes2017 NAs percentage: ", round(p2017,2),"|" )
cat(" crimes2018 NAs percentage: ", round(p2018,2) )
#The percentages are very little we can therefore afford to impute the missing data without missing valuable information in the dataset
#let's start by imputing the observation where NAs in both variables intersect
missing_intersection = intersect(which(is.na(df_crime$crimes2017)),which(is.na(df_crime$crimes2018)))
df_crime = df_crime[-missing_intersection,]

missing2017 = which(is.na(df_crime$crimes2017))
df_crime = df_crime[-missing2017,]
missing2018 = which(is.na(df_crime$crimes2018))
df_crime = df_crime[-missing2018,]



#2. Exploratoy data analysis
#2.1. Pearson's correlation between numerical variables
df_crime_numerical = df_crime
df_crime_numerical$area_code = NULL
df_crime_numerical$Borough = NULL
df_crime_numerical$Major.Category = NULL
df_crime_numerical$Minor.Category = NULL
#str(df_crime_numerical)

df_crime_numerical <- data.frame(lapply(df_crime_numerical, function(x) as.numeric(as.character(x))))

#sort on decreasing correlations with price
cor_numVar <- cor(df_crime_numerical, use="pairwise.complete.obs")
cor_sorted <- sort(cor_numVar[,22], decreasing = TRUE)
CorHigh <- names(which(cor_sorted>=0.1))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#Crime variables are highly correlated with each other. Which mean that they are very predictive of each other.
#Normally in this situation it is advisable to remove highly correlated variables from the model, but we want to use all of the available data so we will keep them.

df_cor <- cor(df_crime_numerical)
corrplot(df_cor)

#At this point, because all observation with the same Borough or area have the same value of number of immigrants, we could consider NbImmig20XX as a categorical variable. We will see if there is a need to do that later on in our study.

#2.2. Spearman's correlation between numerical variables
#Pearson's correlation assesses linear relationships, let's compute Spearman's correlation which assesses monotonic relationships (whether linear or not) between variables.
cor(df_crime_numerical, method = c("spearman"))


#2.3. Evolution of crime and number of immigrants throughout the years
evolution_immig = read.csv("evolutionImmigrationYears.csv")
names(evolution_immig)[names(evolution_immig) == "Area"] <- "Years"

#2.4. Exploring the crime~immigrants relationship
#Boroughs with the respective numbers of crimes occuring
df_crime %>%
  ggplot(aes(Borough,y=..count..)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Distribution of the most common major category of crime in Boroughs
df_crime %>%
  group_by(Borough)%>%
  ggplot(aes(Major.Category,y=..count..,fill=Borough)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Distribution of the most common minor category of crime throughout Boroughs
df_crime %>%
  group_by(Major.Category) %>%
  ggplot(aes(Minor.Category,y=..count..,fill=Major.Category)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Glimpse at a time serial relationship between crime through years
#QUESTION TO ANSWER : Does the number of crimes in 2018 depend on the number of crimes in 2017?
df_crime %>%
  group_by(Borough) %>%
  ggplot(aes(crimes2018,crimes2017, color = Borough)) + 
  geom_point() +
  facet_wrap(~Borough) + 
  ylim(0,1000)+
  ggtitle("crime2017 ~ crime2018 in every area")
#The number of crimes during a specific year is highly correlated which the number of crimes the following year

#Evolution of crime and incoming immigrants in a specific Borough throughout the years
#Example of "Greenwich" and "Southwark"
evolutionCrimeYears <- df_crime %>%
  group_by(Borough) %>%
  summarize(Crimes2008 = sum(crimes2008),Crimes2009 = sum(crimes2009),Crimes2010 = sum(crimes2010),Crimes2011 = sum(crimes2011),
            Crimes2012 = sum(crimes2012),Crimes2013 = sum(crimes2013),Crimes2014 = sum(crimes2014),Crimes2015 = sum(crimes2015),
            Crimes2016 = sum(crimes2016),Crimes2017 = sum(crimes2017),Crimes2018 = sum(crimes2018))

evolutionCrimeYears <- as.data.frame(t(as.matrix(evolutionCrimeYears)))
#evolutionCrimeYears
#names(evolutionCrimeYears)[names(evolutionCrimeYears) == "Borough"] <- "Years"

evolutionImmigrantsYear <- df_crime %>%
  group_by(Borough) %>%
  summarize(Immigrants2008 = sum(NbImmig2008),Immigrants2009 = sum(NbImmig2009),Immigrants2010 = sum(NbImmig2010),Immigrants2011 = sum(NbImmig2011),
            Immigrants2012 = sum(NbImmig2012),Immigrants2013 = sum(NbImmig2013),Immigrants2014 = sum(NbImmig2014),Immigrants2015 = sum(NbImmig2015),
            Immigrants2016 = sum(NbImmig2016),Immigrants2017 = sum(NbImmig2017),Immigrants2018 = sum(NbImmig2018))

evolutionImmigrantsYear = as.data.frame(t(evolutionImmigrantsYear))

GreenwichImmigrants_per_year = as.vector(evolutionImmigrantsYear[,10])
GreenwichImmigrants_per_year = GreenwichImmigrants_per_year[2:12]

SouthwarkImmigrants_per_year = as.vector(evolutionImmigrantsYear[,27])
SouthwarkImmigrants_per_year = SouthwarkImmigrants_per_year[2:12]


GreenwichCrimes_per_year = as.vector(evolutionCrimeYears[,10])
GreenwichCrimes_per_year = GreenwichCrimes_per_year[2:12]

SouthwarkCrimes_per_year = as.vector(evolutionCrimeYears[,27])
SouthwarkCrimes_per_year = SouthwarkCrimes_per_year[2:12]

df <- data.frame(GWimmig = GreenwichImmigrants_per_year,
                 GWcrime =GreenwichCrimes_per_year,
                 SWimmig =SouthwarkImmigrants_per_year,
                 SWcrimes = SouthwarkCrimes_per_year,
                 years = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))

df$GWimmig = as.numeric(df$GWimmig)
df$GWcrime = as.numeric(df$GWcrime)
df$SWimmig = as.numeric(df$SWimmig)
df$SWcrimes = as.numeric(df$SWcrimes)

df %>% ggplot() +
  geom_line(aes(years,GWcrime,color="red")) +
  geom_line(aes(years,GWimmig,color = "blue"))+
  ggtitle("Evolution of crime and incoming immigrants in Greenwich") +
  ylab("Scaled numbers of crime and immigrants") +
  #scale_fill_discrete(labels=c("crime","immigrants"))+
  scale_color_manual(labels = c("Crime", "Immigrants"), values = c("red", "blue"))   +
  ggtitle("Evolution of crime and immigrants from 2008 to 2018 in Greenwich") 


df %>% ggplot() +
  geom_line(aes(years,SWcrimes,color="red")) +
  geom_line(aes(years,SWimmig, color = "blue")) +
  scale_color_manual(labels = c("Crime", "Immigrants"), values = c("red", "blue"))+
  ggtitle("Evolution of crime and immigrants from 2008 to 2018 in Southwark")


#In some areas, though not all, crime follows the trend that immigrants have. In Southwark for example, we can clearly see that crime decreases when the number of immigrants does, and goes up again when the number of immigrants goes up.
#In both areas, after 2016, crime rates have gone down while immigration numbers increased, which might prove true the misconception among people that crime rates have gone higher with immigration.
#Evolution of crime per month after Brexit in area of UK with the most immigrants that year
#2016:
crime_per_month_2016 <- crime %>% 
  group_by(Borough) %>%
  summarize(jan2016 = sum(X201601),fev2016 = sum(X201602), mars2016 = sum(X201603), avr2016 = sum(X201604),mai2016 = sum(X201605),
            juin2016 = sum(X201606), jui2016 = sum(X201607), aout2016 = sum(X201608), sep2016 = sum(X201609), oct2016 = sum(X201610),
            nov2016 = sum(X201611), dec2016 = sum(X201612))
crime_per_month_2016 = as.data.frame(t(crime_per_month_2016))

immig$Area[immig$X2016==max(immig$X2016)] #AREA WITH HIGHEST IMMIGRANTS NUMBER IN 2016 IS NEWHAM

monthly_crime_2016 = as.vector(crime_per_month_2016[,25])
monthly_crime_2016 = monthly_crime_2016[2:13]
monthly_crime_2016 = as.integer(monthly_crime_2016)

barplot(monthly_crime_2016, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","July", "Aug", "Sep", "Oct", "Nov","Dec"
), col="#69b3a2", main="Crime evolution in Newham in 2016")

#2017:
crime_per_month_2017 <- crime %>% 
  filter(!is.na(X201701),!is.na(X201702),!is.na(X201703),!is.na(X201704),!is.na(X201705),!is.na(X201706),!is.na(X201707),
         !is.na(X201708),!is.na(X201709),!is.na(X201710),!is.na(X201711),!is.na(X201712)) %>%
  group_by(Borough) %>%
  summarize(jan2017 = sum(X201701),fev2017 = sum(X201702), mars2017 = sum(X201703), avr2017 = sum(X201704),mai2017 = sum(X201705),
            juin2017 = sum(X201706), jui2017 = sum(X201707), aout2017 = sum(X201708), sep2017 = sum(X201709), oct2017 = sum(X201710),
            nov2017 = sum(X201711), dec2017 = sum(X201712))
crime_per_month_2017 = as.data.frame(t(crime_per_month_2017))

immig$Area[immig$X2017==max(immig$X2017)] #AREA WITH HIGHEST IMMIGRANTS NUMBER IN 2017 IS NEWHAM

monthly_crime_2017 = as.vector(crime_per_month_2017[,25])
monthly_crime_2017 = monthly_crime_2017[2:13]
monthly_crime_2017 = as.integer(monthly_crime_2017)

barplot(monthly_crime_2017, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","July", "Aug", "Sep", "Oct", "Nov","Dec"
), col="#69b3a2",main="Crime evolution in Newham in 2017")

#2018:
crime_per_month_2018 <- crime %>% 
  filter(!is.na(X201801),!is.na(X201802),!is.na(X201803),!is.na(X201804),!is.na(X201805),!is.na(X201806),!is.na(X201807),
         !is.na(X201808),!is.na(X201809),!is.na(X201810),!is.na(X201811),!is.na(X201812)) %>%
  group_by(Borough) %>%
  summarize(jan2018 = sum(X201801),fev2018 = sum(X201802), mars2018 = sum(X201803), avr2018 = sum(X201804),mai2018 = sum(X201805),
            juin2018 = sum(X201806), jui2018 = sum(X201807), aout2018 = sum(X201808), sep2018 = sum(X201809), oct2018 = sum(X201810),
            nov2018 = sum(X201811), dec2018 = sum(X201812))
crime_per_month_2018 = as.data.frame(t(crime_per_month_2018))

immig$Area[immig$X2018==max(immig$X2018)] #AREA WITH HIGHEST IMMIGRANTS NUMBER IN 2018 IS NEWHAM

monthly_crime_2018 = as.vector(crime_per_month_2018[,25])
monthly_crime_2018 = monthly_crime_2018[2:13]
monthly_crime_2018 = as.integer(monthly_crime_2018)

barplot(monthly_crime_2018, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","July", "Aug", "Sep", "Oct", "Nov","Dec"
), col="#69b3a2",main="Crime evolution in Newham in 2018")

#Crime per borough & immigrants per borough (2016)
#january 2016
#str(crime)
crime %>%
  group_by(Borough) %>%
  summarize(crime_per_borough2016_January = sum(X201601)) %>%
  ggplot(aes(Borough,crime_per_borough2016_January, color = Borough)) + 
  geom_bar(stat="identity", width=1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Crime in each area, January 2016")

#str(crime)
crime %>%
  group_by(Borough) %>%
  summarize(crime_per_borough2016_Dec= sum(X201612)) %>%
  ggplot(aes(Borough,crime_per_borough2016_Dec, color = Borough)) + 
  geom_bar(stat="identity", width=1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Crime in each area, December 2016")

#str(immig)
immig %>%
  group_by(Area) %>%
  summarize(immigrants_per_borough2016= sum(X2016)) %>%
  ggplot(aes(Area,immigrants_per_borough2016, color = Area)) + 
  geom_bar(stat="identity", width=1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Immigrants in each area, 2016")

yearlyCrime <- df_crime %>%
  group_by(Borough) %>%
  summarize(Crimes2008 = sum(crimes2008),Crimes2009 = sum(crimes2009),Crimes2010 = sum(crimes2010),Crimes2011 = sum(crimes2011),
            Crimes2012 = sum(crimes2012),Crimes2013 = sum(crimes2013),Crimes2014 = sum(crimes2014),Crimes2015 = sum(crimes2015),
            Crimes2016 = sum(crimes2016),Crimes2017 = sum(crimes2017),Crimes2018 = sum(crimes2018))

yearlyCrime <- as.data.frame(t(as.matrix(yearlyCrime)))

#3.Point estimations and confidence intervals
#bootstrap distribution
# Here we want to create a confidence interval of 95% for the mean of immigrants across 2008-2018 in a Bourough, we take for example Greenwich
foo <- function(data, indices){
  dt<-data[indices]
  mean(dt)
}
set.seed(12345) #for reproducibility and consistency of results
myBootstrap <- boot(evolution_immig$Greenwich, foo, R=1000)
myBootstrap$t0 #6490.455
# Plot a histogram of the bootstrap distribution of the mean of number of immigrants in the city of london
plot(myBootstrap,type="h")
boot.ci(myBootstrap, conf=0.95) #(5831, 7108 )
upper=quantile(myBootstrap$t,0.955) #5676
lower=quantile(myBootstrap$t,0.005) #7065

# Now let's create a confidence interval of 95% for the mean of crime across 2008-2018 in the same city
myBootstrap <- boot(c(21418,19459,18304,18270,19674,18647,19506,20523,22302,24067,24103), foo, R=1000)
myBootstrap$t0 #20570
# Plot a histogram of the bootstrap distribution of the mean of number of immigrants in the city of london
plot(myBootstrap,type="h")
boot.ci(myBootstrap, conf=0.95) #(19406, 21725 )
upper=quantile(myBootstrap$t,0.955) #19221
lower=quantile(myBootstrap$t,0.005) #21626

#4. 
# ----- yearly nb of immigrants vs boroughs? independent?
#---------------example City of London &  Barking and Dagenham
data1 = rep(c("City of London","Barking and Dagenham"), c(11204,62967))
data2 = rep(c("Year 2008","Year 2009","Year 2010","Year 2011","Year 2012","Year 2013","Year 2014","Year 2015","Year 2016"
              ,"Year 2017","Year 2018","Year 2008","Year 2009","Year 2010","Year 2011","Year 2012","Year 2013","Year 2014","Year 2015",
              "Year 2016","Year 2017","Year 2018"),
            c(724,580,780,801,705,712,892,975,1027,1451,2557,4884,5148,6088,4696,3513,4405,7727,7538,7585,5604,5779))
chi_table = table(data1,data2)
#calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2
# calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(data1)
  d2 = sample(data2)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist)
#The null hypothesis for this test is that there is no relationship between the yearly nb of immigrants and 
#and boroughs
p_value = mean(X2dist>X2)
p_value
# p < 0.05 -> we reject H0 therefore the two variables are dependant
chisq.test(chi_table)     

#---------------example Greenwich &  Enfield
data3 = rep(c("Greenwich","Enfield"), c(71395,77829))
data4 = rep(c("Year 2008","Year 2009","Year 2010","Year 2011","Year 2012","Year 2013","Year 2014","Year 2015","Year 2016"
              ,"Year 2017","Year 2018","Year 2008","Year 2009","Year 2010","Year 2011","Year 2012","Year 2013","Year 2014","Year 2015","Year 2016"
              ,"Year 2017","Year 2018"),
            c(6695,8260,8017,6343,4971,5149,7184,7002,6786,5392,5596,7856,6584,6769,5191,4745,5665,9593,9259,8663,6823,6681))
chi_table2 = table(data3,data4)
#calculating the X2 statistic
p2 = rowSums(chi_table2)
q2 = colSums(chi_table2)
expectedRowProportion2=p2/sum(chi_table2)
expectedRowProportion2
expectedColProportion2=q2/sum(chi_table2)
expectedColProportion2
E2=expectedRowProportion2 %o% expectedColProportion2 *sum(chi_table2)
X2_2=sum((chi_table2-E2)^2/E2)
X2_2
# calculating the p-value
X2dist_2 <- replicate(1000, {
  d1 = sample(data1)
  d2 = sample(data2)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist_2)
#The null hypothesis for this test is that there is no relationship between the yearly nb of immigrants and 
#and boroughs
p_value2 = mean(X2dist_2>X2_2) 
p_value2
chisq.test(chi_table2) #same result

# ----- yearly nb of crimes vs boroughs? independent?
#---------------example Hackney &  Brent
data5 = rep(c("Hackney","Brent"), c(276286,280576))
data6 = rep(c("Crime 2008","Crime 2009","Crime 2010","Crime 2011","Crime 2012","Crime 2013","Crime 2014","Crime 2015","Crime 2016"
              ,"Crime 2017","Crime 2018",
              "Crime 2008","Crime 2009","Crime 2010","Crime 2011","Crime 2012","Crime 2013","Crime 2014","Crime 2015","Crime 2016"
              ,"Crime 2017","Crime 2018"),
            c(21622,20527,19996,21246,26960,25480,24747,26808,28102,30774,30024,
              22935,24319,25042,27184,25922,23363,24011,24484,26304,28246,28766))
chi_table3 = table(data5,data6)
#calculating the X2 statistic
p3 = rowSums(chi_table3)
q3= colSums(chi_table3)
expectedRowProportion3=p3/sum(chi_table3)
expectedRowProportion3
expectedColProportion3=q3/sum(chi_table3)
expectedColProportion3
E3=expectedRowProportion3 %o% expectedColProportion3 *sum(chi_table3)
X2_3=sum((chi_table3-E3)^2/E3)
X2_3
# calculating the p-value
X2dist_3 <- replicate(1000, {
  d1 = sample(data1)
  d2 = sample(data2)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist_3)
#The null hypothesis for this test is that there is no relationship between the yearly nb of immigrants and 
#and boroughs
p_value3 = mean(X2dist_3>X2_3) 
p_value3
chisq.test(chi_table3) #same result




#5. Distribution tests
df_crime %>%
  group_by(Borough) %>%
  ggplot(aes(crimes2018,y=..count..,col=Borough)) +
  geom_bar()
df_crime %>%
  group_by(Borough) %>%
  ggplot(aes(log(crimes2018),y=..count..,col=Borough)) +
  geom_bar()

ggdensity(df_crime$crimes2018, 
          main = "Density plot of Crime in 2018",
          xlab = "Crime numbers",
          xlim =c(0,100))
ggdensity(log(df_crime$crimes2018), 
          main = "Density plot of Crime in 2018",
          xlab = "Crime numbers")

ggqqplot(df_crime$crimes2018)
ggqqplot(log(df_crime$crimes2018))

#5.2. goodness of fit tests
# city of london
plotdist(df_crime$crimes2018, histo = TRUE, demp = TRUE) 
#Cullen and Frey graph
descdist(df_crime$crimes2018, boot = 1000) 

fn <- fitdist(df_crime$crimes2018, "norm")
fe <- fitdist(df_crime$crimes2018, "exp")
df_crime$crimes2018[df_crime$crimes2018==0]=0.001
fw <- fitdist(df_crime$crimes2018, "weibull")
fg <- fitdist(df_crime$crimes2018, "gamma")
fln <- fitdist(df_crime$crimes2018, "lnorm")

denscomp(list(fw, fln, fg,fn,fe),ylim=c(0,25))
qqcomp(list(fw, fln, fg,fn,fe))
cdfcomp(list(fw, fln, fg,fn,fe))
ppcomp(list(fw, fln, fg,fn,fe))

ks.test(unique(df_crime$crimes2018), "pgamma",0.2779506 , 0.0398331)
ks.test(unique(df_crime$crimes2018), "pexp",mean(df_crime$crimes2018) , 0.1432812)
ks.test(unique(df_crime$crimes2018), "pweibull",0.3929692 , 3.0407091)
ks.test(unique(df_crime$crimes2018), "pnorm",6.979284 , 21.581382)
ks.test(unique(df_crime$crimes2018), "plnorm",-0.5696274 , 3.8231452)

distFit<-fitdistr(df_crime$crimes2018,"Poisson")
ks.test(df_crime$crimes2018,"ppois",lambda=distFit$estimate)

distFit<-fitdistr(df_crime$crimes2018,"Geometric")
ks.test(df_crime$crimes2018,"pgeom",distFit$estimate)


#6. Machine learning predictions
####LINEAR REGRESSION
set.seed(123)

split = sample.split(df_crime$crimes2018, SplitRatio = 0.7)
training_set = subset(df_crime, split == TRUE)
testing_set = subset(df_crime, split == FALSE)

lm.fit = lm(crimes2018~.-Borough - Major.Category -area_code- Minor.Category, data=training_set)
summary(lm.fit)
plot(lm.fit)
stdResiduals = rstandard(lm.fit)
hist(stdResiduals)

#weighting the model
SD_Func = lm(abs(lm.fit$residuals) ~.-Borough - Major.Category -area_code- Minor.Category, data=training_set) 
Weights = 1 / SD_Func$fitted.values^2 
ModelWeighted = lm(crimes2018~.-Borough - Major.Category -area_code- Minor.Category, data=training_set
                   , weights=Weights)
plot(ModelWeighted)
summary(ModelWeighted)


# cook's distance
cooksd <- cooks.distance(ModelWeighted)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(df_crime)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line


#predictions
modelPreds = predict(lm.fit, newdata = testing_set)
modelPreds[modelPreds<0] = -modelPreds[modelPreds<0]
weightedModelPreds = predict(ModelWeighted, newdata = testing_set)
weightedModelPreds[weightedModelPreds<0] = -weightedModelPreds[weightedModelPreds<0]

rmse = sqrt(mean(modelPreds-testing_set$crimes2018)^2)
rmse_weighted = sqrt(mean(weightedModelPreds-testing_set$crimes2018)^2)

error= median(abs(modelPreds-testing_set$crimes2018)/testing_set$crimes2018)
errorWeighted= median(abs(weightedModelPreds-testing_set$crimes2018)/testing_set$crimes2018)

comparison = data.frame("Actual" = testing_set$crimes2018, "Predicted" =modelPreds )
comparisonWeighted = data.frame("Actual" = testing_set$crimes2018, "Predicted" =weightedModelPreds )


