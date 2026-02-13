#rversion: 4.5.1

#load relevant packages

library(tidyverse)


#load file

data <- read.csv("synth_random10.csv", header = T)

#### Action Plan -----
#1) Create an ID for each record
#2) Recode and label the following predictors
#       Cancer mortality (0 = No death by cancer; 1 = Death by cancer)
#       Income (0 = Not low income; 1= Low income)
#       Gender (0 = Male; 1 = Female)
#       Minority (0 = other, 1 = Minority, 2 = Indiginity)
#3) Assess for missing values
#4) Drop all other variables
#

## Create an ID for each record ----
data <- data|> 
  mutate(ID = row_number()) |> 
  select(ID,everything())

## Recode and Label Predictors ----

## Cancer Mortality

data <- data$COD2_synth(recode(
  mutate()
))

## Income


## Gender


## Minoriy 



