rm(list=ls())
#libraries
library("tidyverse")
library("readxl")    
library("xlsx")      
#constants
n <- 4                #best n pieces of assessment count. 
from_brightspace <- "Fall 2020 ECON 381 A01 - ES 312 A01 X_GradesExport_2020-12-16-00-23.csv"
from_fast_312 <- "Student Grade Entry_15-12-2020_04-22-05_PM.xlsx"
from_fast_381 <- "Student Grade Entry_15-12-2020_04-20-28_PM.xlsx"
assignment_names_start_with <- "ass"
experiment_names_start_with <- "ex"
#functions
sum_best_n <- function(data,n){
  data%>%
    t()%>%
    as.vector()%>%
    sort(decreasing=TRUE)%>%
    head(n=n)%>%
    sum()
}
#program
grades <- read_csv(from_brightspace)
colnames(grades) <- trimws(sapply(str_split(colnames(grades),pattern="Points"),"[",1))
grades[is.na(grades)] <- 0 
grades <- grades%>%
  mutate(`Student ID`=str_sub(OrgDefinedId,start=2))# gets rid of leading hashtag

assignments <- grades%>%
  select(`Student ID`, starts_with(assignment_names_start_with))%>%
  group_by(`Student ID`)%>%
  nest()%>%
  mutate(assignments=map_dbl(data,sum_best_n,n))%>%
  select(-data)

experiments <- grades%>%
  select(`Student ID`, starts_with(experiment_names_start_with))%>%
  group_by(`Student ID`)%>%
  nest()%>%
  mutate(participation=map_dbl(data,sum_best_n,n))%>%
  select(-data)

grades <- left_join(grades,assignments)
grades <- left_join(grades,experiments)
grades <- grades%>%
  mutate(final=round(presentation+essay+assignments+participation))%>%
  select(`Student ID`,final)

grades381 <- read_xlsx(path=from_fast_381)         
grades312 <- read_xlsx(path=from_fast_312)

grades381 <- left_join(grades381,grades)%>%
  select(-Grade)%>%
  rename(Grade=final)%>%
  relocate(Grade, .after = Name)%>%
  mutate(`F or N`=ifelse(Grade<50,"F",NA))%>%
  as.data.frame()

grades312 <- left_join(grades312,grades)%>%
  select(-Grade)%>%
  rename(Grade=final)%>%
  relocate(Grade, .after = Name)%>%
  mutate(`F or N`=ifelse(Grade<50,"F",NA))%>%
  as.data.frame()

write.xlsx(grades381, "381.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(grades312, "312.xlsx", showNA = FALSE, row.names = FALSE)


 