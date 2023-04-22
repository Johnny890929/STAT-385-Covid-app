library(tidyverse)
df <- read_csv("Provisional.csv")
df<- df%>%
  rename("Covid_Deaths"= `COVID-19 Deaths`, "Total_Deaths"= `Total Deaths`,"Pneumonia_Deaths" = `Pneumonia Deaths`,"Pneumonia_and_COVID-19 Deaths" = `Pneumonia and COVID-19 Deaths`, "Influenza_Deaths" = `Influenza Deaths`,"Pneumonia_Influenza_or_COVID-19 Deaths"=`Pneumonia, Influenza, or COVID-19 Deaths`,'Age' = `Age Group`, 'Sex'=`Sex`, "State" = State)

function1 <- function(){
  unique(names(df)[10:15])
}