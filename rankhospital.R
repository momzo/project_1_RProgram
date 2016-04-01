
dir("rprog_data_ProgAssignment3-data.zip")
files<-list.files("rprog_data_ProgAssignment3-data.zip")
files[4]
data<-read.csv("outcome-of-care-measures.csv",sep = ",",header = TRUE,na.strings = "NA",colClasses = "character")
str(data)

data[, 13]<-as.numeric(data[, 13])
data[, 19]<-as.numeric(data[, 19])
data[, 25]<-as.numeric(data[, 25])
library(plyr)
library(dplyr)

`heart attack`<-data%>%
  select(State, Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
  arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
  mutate( Rate = Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          Rank = rank(Rate, na.last = TRUE, ties.method ="first")) %>%
  select(State,Hospital.Name, Rate, Rank) 
  

head(`heart attack`)
head(`heart attack`[`heart attack`$State=="TX",])


`heart failure`<-data%>%
  select(State, Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
  arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
  mutate(Rate = Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
         Rank = rank(Rate,na.last =TRUE,ties.method ="first")) %>%
  select(Hospital.Name, Rate, Rank)

head(`heart failure`)


`pneumonia`<-data%>%
  select(State, Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
  arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
  mutate(Rate = Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
         Rank = rank(Rate, na.last = TRUE, ties.method ="first")) %>%
  select(Hospital.Name, Rate, Rank)

head(`pneumonia`)
  
  
  


  
