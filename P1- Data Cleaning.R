#######################################################
#       Data Wrangling and Cleaning                   #
#######################################################




# Installing and Importing necessary Packages

install.packages("naniar")
library(dplyr)
library(ggplot2)
library(naniar)
library(plotrix)
library(magrittr)

#Importing the Dataset into an Data frame

Insurance <- data.frame(read.csv(file.choose()))
View(Insurance)


#Performing basic actions to get insights of the data 

str(Insurance)
dim(Insurance)
head(Insurance)
tail(Insurance)
nrow(Insurance)
attach(Insurance)



# Removing the unwanted Columns

#1) the i column consists of only numbers indicating index
Insurance$i <- NULL

#2) The Campaign_DESC is an empty row 
Insurance$CAMPAIGN_DESC <- NULL

#3) The Clerical column
Insurance$CLERICAL <- NULL
dim(Insurance)

#Replacing all the empty cells with NA
Insurance[Insurance == "" | Insurance == "" ] <- NA
View(Insurance)

# Lets check the Overall Na's in the Data set
Na_summary=miss_var_summary(Insurance)
View(Na_summary)



## 1) Since the P1_PT_EMP_STATUS column sonsists of 99.3% of missing / NA Values and hence cannot be used so we can drop that 
Insurance = subset(Insurance, select = -c(P1_PT_EMP_STATUS))
dim(Insurance)


##2) Lets check for the payment Frequency Column
View(select(Insurance,PAYMENT_FREQUENCY))
## it consists of 68.55% of NA values , Now lets check for the values in this column
unique(PAYMENT_FREQUENCY)
## Since it consits of only two variables 1 & Na where,
## 1 means the Payment is frequent and Na means there is no payment frequency 
## hence we can NA can be replaced 0 which will have no other impact on the column
Insurance = Insurance %>% mutate(PAYMENT_FREQUENCY=ifelse(is.na(PAYMENT_FREQUENCY),0,1))
## Lets check for the replaced 0 
View(select(Insurance,PAYMENT_FREQUENCY))


## 3) lets check for the Risk_rated_Area_B & Risk_Rated_Area_C
# this column consists of only  44% & 26% and hence the NA's can be replaced with mean/median
# lets plot the boxplot to find if there are any outliers in the column
boxplot(RISK_RATED_AREA_B,RISK_RATED_AREA_C, col = rainbow(2))
#there are ouliers in the column hence we need to replace the Na with Median
Insurance=Insurance %>% 
  mutate(RISK_RATED_AREA_B=ifelse(is.na(RISK_RATED_AREA_B),median(RISK_RATED_AREA_B,na.rm =T),RISK_RATED_AREA_B),
         RISK_RATED_AREA_C=ifelse(is.na(RISK_RATED_AREA_C),median(RISK_RATED_AREA_C,na.rm =T),RISK_RATED_AREA_C))
View(Insurance)
boxplot(RISK_RATED_AREA_B,RISK_RATED_AREA_C, col = rainbow(2))
# there are still ouliers but none of them affect the data


## 4) Lets check for the MTA_Date 
## It is considered to be a mid term adjustment date 
## which basically tells you if the MTA happened or not
## but since we have other columns that indicate MTA  we can remove this column.
## And also this column contains more 85% of NA Values 
View(select(Insurance,MTA_DATE))
Insurance = subset(Insurance, select = -c(MTA_DATE))
View(Insurance)
Na_summary=miss_var_summary(Insurance)
View(Na_summary)


## 5) Lets check for the MTA_FAP & MTA_APRP
View(select(Insurance, MTA_APRP))
View(select(Insurance,MTA_FAP))
## On checking up the column we got to know that 
## it has above 75% of missing values in both column
## the column also contains 0 hence we can replace the NA's with 0
Insurance=Insurance %>% 
  mutate(MTA_APRP=ifelse(is.na(MTA_APRP),0,MTA_APRP),
         MTA_FAP=ifelse(is.na(MTA_FAP),0,MTA_FAP))


## 6) Lets check for the Cover start column 
## lets check for the data type of Cover start column
str(COVER_START)
## as it shows that its character ,lets convert it to date type format 
Insurance$COVER_START=as.Date(Insurance$COVER_START,"%d/%m/%Y")
str(COVER_START)
## lets check for the data type again
## Since Cover_START is column that represents if the Cover Payment has started or not 
## letrs remove the NA values
Insurance=Insurance %>% filter(COVER_START!=is.na(COVER_START))
View(Insurance)
View(select(Insurance,SUM_INSURED_BUILDINGS))



# lets check again for the remaining NA Percentage in the whole data set
Na_summary=miss_var_summary(Insurance)
View(Na_summary)



## 7) lets check for the Quote date column
View(select(Insurance,QUOTE_DATE))
## lets convert it to Date type Format since it is a character format
QUOTE_DATE=as.Date(QUOTE_DATE,"%m/%d/%Y")
str(QUOTE_DATE)
## We have converted the data type into Date type format
## this column presents the date of Quotation was made 
## lets check if other columns are available for the NA dates in Quote Date 
Insurance %>% filter(is.na(QUOTE_DATE)) %>% View()
## since we can see that Cover_Start Dates are available for even the rows which have Quote Date missing
## WE can replace the NA's with Y which will represent that the quotation  of policy
## Lets first change the name of the column as Policy_Quoted 
Insurance = Insurance %>% rename(Quotation = QUOTE_DATE)
View(Insurance)
## Now convert the NA's and Dates into Y
Insurance = Insurance %>% mutate(Quotation=ifelse(is.na(Quotation),"Y","Y"))



### Now lets check if all NUll Values are removed orr not
Na_summary=miss_var_summary(Insurance)
View(Na_summary)


## Lets check again if something is missing/ some column with incorrect datatype .
str(Insurance)
## Ww can see that the P1_DOB column is in character format lets change it to date format 
Insurance$P1_DOB=as.Date(Insurance$P1_DOB,"%d/%m/%Y")
str(P1_DOB)
## The Data type conversion of P1_DOB is Done.




## Since all the NA values are cleared 
## Lets do Feature/Column Selection 


colnames(Insurance)
## Since Every Policy is being quoted The Quotation Column wont help in any analysis
## Police column is also of no use as the column only contains numbers chronologically.
## Columns like BUS_USE , CLAIM3YEARS , PAYING_GUESTS , PAYMENT METHOD are of no help in Analysis 

Insurance = subset(Insurance ,select = -c(Quotation,BUS_USE,Police,PAYMENT_METHOD,CLAIM3YEARS,PAYING_GUESTS))
dim(Insurance)



## Since now all the unwanted columns and NA values have been removed from the data, 
## the data is now ready for further analysis step.
## Lets View and save the Cleaned data into an CSV Format followed by the name "CLEANED INSURANCE.csv"
View(Insurance)
write.table(Insurance, file = "CLEANED INSURANCE.csv",row.names = F,sep = ",")

## Note :- The file is either saved in 'DOWNLOADS' or "DOCUMENTS" Folder as those two are default Locations. 

## lets rewrite the File with name "cleaned"
cleaned = Insurance
View(cleaned)