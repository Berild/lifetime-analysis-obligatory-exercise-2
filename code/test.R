#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(survival)
library(survminer)
library(kableExtra)
library(dplyr)
library(SurvRegCensCov)
library(flexsurv)
library(rms)

coef <- wei.lung$coefficients
lifeWeil <- function(data, coef){
  res = vector()
  for (i in seq(1,length(data[,1]))){
    x = c(1,data$Treat[i], data$PS[i], data$Month[i], data$Age[i], data$Prior[i],as.numeric(data$Cell[i]==2),
          as.numeric(data$Cell[i]==3),as.numeric(data$Cell[i]==4))
    res = c(res,exp(as.numeric(coef)%*%x))
  }
  return(median(sort(res)))
}
patient1 <- lungcancer.df[lungcancer.df$Cell == lungcancer.df$Cell[1],]
patient2 <- lungcancer.df[lungcancer.df$Cell == lungcancer.df$Cell[19],]
life1 <- lifeWeil(patient1,coef)
life2 <- lifeWeil(patient2,coef)
cat("Estimated median lifetime ")


lifeWeil <- function(data, coef){
  x = c(1,data$Treat, data$PS, data$Month, data$Age, data$Prior,as.numeric(data$Cell==2),
        as.numeric(data$Cell==3),as.numeric(data$Cell==4))
  return(exp(as.numeric(coef)%*%x))
}
life1 <- lifeWeil(lungcancer.df[1,],coef)
life2 <- lifeWeil(lungcancer.df[23,],coef)