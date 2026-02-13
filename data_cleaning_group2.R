#rversion: 4.5.1

#load relevant packages

install.packages("labelled")

library(tidyverse)
library(labelled)


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

#create a new variable with cancer (binary)
data <- data |> 
  mutate(cancer = case_when(
    COD2_synth <= 2 ~ 0,
    COD2_synth == 3 ~ 1,
    COD2_synth == 4 ~ 1,
    COD2_synth >= 5 & COD2_synth <= 13 ~ 0, 
    COD2_synth == 14 ~ 2
      ))

data <- data |> select(-cancer)

table(data$cancer)
# print
# 0         1 
# 432604   2061 


#label the variables

cancer <- labelled(data$cancer, c("Death by Other" = 0,"Death by Cancer" = 1, "Alive" = 2))

data <-data |> 
  mutate(cancer_lab = case_when(
    cancer == 0 ~ "Death by Other",
    cancer == 1 ~ "Death by Cancer",
    cancer == 2 ~ "Alive"
  ))

table(data$cancer_lab)

## Income

#create a new variable with income

table(data$LOINCA_synth)

data <- data |> 
  mutate(income = case_when(
    LOINCA_synth == 1 ~ "Not Low Income",
    LOINCA_synth == 2 ~ "Low Income",
    TRUE ~ NA))

table(data$income)

## Gender

table(data$SEX_synth)

data <- data |> 
  mutate(sex = case_when(
    SEX_synth == 1 ~ "Female",
    SEX_synth == 2 ~ "Male",
    TRUE ~ NA))

table(data$sex)

## Minority 


data <- data |> 
  mutate(minority_status = case_when(
    DVISMIN_synth >= 1 & DVISMIN_synth <= 12 ~ "Visible Minority",
    DVISMIN_synth == 13 ~ "Indigenous",
    DVISMIN_synth == 14 ~ "Non-Minority",
    TRUE ~ NA))

table(data$minority_status)


data_clean <- data |> 
  select(ID, cancer_lab, income, sex, minority_status)

##### VERIFY THE DATA -----

ggplot(data_clean, aes(income, cancer_lab))+
  geom_bar(stat="identity", fill = "seagreen")
