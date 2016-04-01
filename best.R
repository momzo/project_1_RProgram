
dir("rprog_data_ProgAssignment3-data.zip")
data <- read.csv("outcome-of-care-measures.csv", sep = ",", header = TRUE, 
      na.strings = "NA", colClasses = "character")
str(data)

hospital.name <- data$Hospital.Name
state <- data$State

data[, 13] <- as.numeric(data[, 13])
data[, 19] <- as.numeric(data[, 19])
data[, 25] <- as.numeric(data[, 25])
hist(data[, 13], col = "red")

library(plyr)
library(dplyr)

`heart attack` <- data %>% 
  select(State, Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>% 
      arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
tail(`heart attack`)
`heart attack`[`heart attack`$State == "TX", ][1,2]
`heart attack`[`heart attack`$State == "MD", ][1,2]

`heart failure` <- data %>% 
  select(State, Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>% 
      arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)

`heart failure`[`heart failure`$State == "TX", ][1,2]

pneumonia <- data %>% 
  select(State, Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>% 
      arrange(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

`pneumonia`[`pneumonia`$State == "TX", ][1,2]
`pneumonia`[`pneumonia`$State == "MD", ][1,2]



best <- function(state, outcome){
      
      read.csv("outcome-of-care-measures.csv")
  
      state<-`heart attack`[which(`heart attack`[, "State"]== `heart attack`$State), ]
      state<-`heart failure`[which(`heart failure`[, "State"]== `heart failure`$State), ]
      state<-`pneumonia`[which(`pneumonia`[, "State"]== `pneumonia`$State), ]
        
            if (!nzchar(state)){ 
        
            stop("Error in best(state,outcome) : invalid state")
              
            }
               state
 
  

            outcome<-c("heart attack","heart failure","pneumonia")
             
            if (!nzchar(c("heart attack" , "heart failure" , "pneumonia"))){
                  
            stop("Error in best(state,outcome) : invalid outcome")
            }
          
              
            if(nzchar("heart attack")){
                
              outcome<-`heart attack`[`heart attack`$State == `heart attack`$State, ][1,2]
              outcome
            }
            if(nzchar("heart failure")){
                  
              outcome<-`heart failure`[`heart failure`$State == `heart failure`$State, ][1,2]
              outcome
            }
    
            if(nzchar("pneumonia")){
                
              outcome<-`pneumonia`[`pneumonia`$State == `pneumonia`$State, ][1,2]
              outcome
            }
     
}

best("TX","heart attack")