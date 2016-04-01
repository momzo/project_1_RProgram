
dir("rprog_data_ProgAssignment3-data.zip")
data<-read.csv("outcome-of-care-measures.csv",sep = ",",
               header=TRUE,na.strings = "NA",colClasses = "character")
str(data)
state<-data$State
hospital.name<-data$Hospital.Name

data[, 13]<-as.numeric(data[, 13])
data[, 19]<-as.numeric(data[, 19])
data[, 25]<-as.numeric(data[, 25])
hist(data[, 25], col = "green")

library(plyr)
library(dplyr)

`heart attack`<-data %>%
  select(State, Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
  arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%
  mutate(Rate = Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,hospital =Hospital.Name ,state =State,rank=rank(Hospital.Name,ties.method ="first")) %>%
  select(State,hospital,state)
head(`heart attack`)

`heart failure`<-data %>%
  select(State, Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
  arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
  mutate(Rate = Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,hospital =Hospital.Name,state =State) %>%
  select(State, hospital,state)
head(`heart failure`)
  
  `pneumonia`<-data %>%
    select(State,Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
    arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
    mutate(Rate = Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,hospital =Hospital.Name,state =State) %>%
    select(State,hospital,state)
  head(`pneumonia`)

  

rankall<-function(outcome, num ="best"){
  num =rank
  outcome<-`heart attack`[`heart attack`$State ==`heart attack`$State,][1,2]
  outcome<-`heart failure`[`heart failure`$State ==`heart failure`$State,][1,2]
  outcome<-`pneumonia`[`pneumonia`$State ==`pneumonia`$State,][1,2]
  
  for(i in 1:3){
    
  
    return(outcome[i])
  }
}

rankall("heart attack",20)
