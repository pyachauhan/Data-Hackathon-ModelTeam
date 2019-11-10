library(readxl)
library(ggplot2)
library(mosaic)
library(wesanderson)
library(forecast)
library(dplyr)
library(tidyverse)
library(rmarkdown)
library(stats)
library(gmodels)

#start reading the data
df <- read.csv(file = "~/Documents/Data-Hackathon-ModelTeam/bank_full_cleaned.csv"
               ,header=TRUE,sep=",")
head(df)
colnames(df)
summary(df, na.rm = TRUE)


##quick skim through of the freq dist
job <- table(df['job'])
CrossTable(df$job)
marital <- table(df['marital'])
CrossTable(df$marital)

age <- data.frame(Variable.name = "age",
           df %>%
             summarise(N = n(),
                       Min = min(age),
                       Med = median(age),
                       Max = max(age),
                       Mean = round(mean(age),1),
                       Std.Dev = round(sd(age),1),
                       Var = round((sd(age))^2,1),
                       Min_zscore = round(min(age),1),
                       Max_zscore = round(max(age),1) ))

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}


##Dummy Vars for Job
df['admin'] <- ifelse(df$job == "admin.",1,0)
df['blue-collar']<- ifelse(df$job == "blue-collar",1,0)
df['entrepreneur']<- ifelse(df$job == "entrepreneur",1,0)
df['housemaid']<- ifelse(df$job == "housemaid",1,0)
df['management']<- ifelse(df$job == "management",1,0)
df['retired']<- ifelse(df$job == "retired",1,0)
df['self-employed']<- ifelse(df$job == "self-employed ",1,0)
df['services']<- ifelse(df$job == "services",1,0)
df['student']<- ifelse(df$job == "student",1,0)
df['technician']<- ifelse(df$job == "technician",1,0)
df['unemployed']<- ifelse(df$job == "unemployed",1,0)
df['unknown']<- ifelse(df$job == "unknown",1,0)

##Dummy Vars for marital
df['divorced']<- ifelse(df$marital == "divorced",1,0)
df['married']<- ifelse(df$marital == "married",1,0)
df['single']<- ifelse(df$marital == "single",1,0)
df['normage'] <- normalize(df$age)

#all independent var cortest
df_cor <- df[,18:33]
cortest <- data.frame(cor(df_cor, method="pearson",use="complete.obs"))
view(cortest)
