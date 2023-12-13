library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)

read.csv("lcmdd.csv", header=T) -> data

#reverse code
5 - data[,"HIn11psa015"] -> data2[,"HIn11psa015"]
5 - data[,"HCh11psa015"] -> data2[,"HCh11psa015"]
5 - data[,"HCh12psa023"] -> data2[,"HCh12psa023"]
5 - data[,"HIn12psa023"] -> data2[,"HIn12psa023"]

#mean score
data2 %>% mutate(M_inter3=rowSums(cur_data()[,c("HIn11psa010", "HIn11psa021", "HIn11psa027","HIn11psa029",
                                 "HIn11psa031","HIn11psa033","HIn11psa036","HIn11psa038","HIn11psa039")], na.rm=F)/9,
                 M_disr3=rowSums(cur_data()[,c("HIn11psa011", "HIn11psa013", "HIn11psa015","HIn11psa018",
                                       "HIn11psa020","HIn11psa022","HIn11psa023","HIn11psa024","HIn11psa026",
                                       "HIn11psa028", "HIn11psa030", "HIn11psa034","HIn11psa037")], na.rm=F)/13,
                 M_disc3=rowSums(cur_data()[,c("HIn11psa012", "HIn11psa014", "HIn11psa016","HIn11psa017",
                                       "HIn11psa019","HIn11psa025","HIn11psa032","HIn11psa035")], na.rm=F)/8,
                 M_inter4=rowSums(cur_data()[,c("HCh12psa010", "HCh12psa021", "HCh12psa027","HCh12psa029",
                                                 "HCh12psa031","HCh12psa033","HCh12psa036","HCh12psa038","HCh12psa039")], na.rm=F)/9,
                  M_disr4=rowSums(cur_data()[,c("HCh12psa011", "HCh12psa013", "HCh12psa015","HCh12psa018",
                                                "HCh12psa020","HCh12psa022","HCh12psa023","HCh12psa024","HCh12psa026",
                                                "HCh12psa028", "HCh12psa030", "HCh12psa034","HCh12psa037")], na.rm=F)/13,
                  M_disc4=rowSums(cur_data()[,c("HCh12psa012", "HCh12psa014", "HCh12psa016","HCh12psa017",
                                                "HCh12psa019","HCh12psa025","HCh12psa032","HCh12psa035")], na.rm=F)/8) -> data3

#regression

#peer interaction ~ pear interaction + language exposure
model_inter <- lm(M_inter4 ~ Ttime + M_inter3, data=data3, )
summary(model_inter)
#peer disruption ~ pear disruption + language exposure
model_inter <- lm(M_disr4 ~ Ttime + M_disr3, data=data3, )
summary(model_inter)
#peer disconnection ~ pear disconnection + language exposure
model_inter <- lm(M_disc4 ~ Ttime + M_disc3, data=data3, )
summary(model_inter)
