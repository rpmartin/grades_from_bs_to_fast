rm(list=ls())
#libraries
library("tidyverse")
library("readxl")    
library("xlsx")      
#constants
n <- 5                #best n pieces of assessment count. 
from_brightspace <- "Spring 2021 ECON 381 A01 A02 - ES 312 A01 A02 X_GradesExport_2021-04-23-17-13.csv"
from_fast_A01 <- "A01.xlsx"
from_fast_A02 <- "A02.xlsx"
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
grades <- read_csv(from_brightspace)%>%
  filter(!str_starts(OrgDefinedId,"#demo"))
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

gradesA01 <- read_xlsx(path=from_fast_A01,col_types = c("text","text","numeric","text","text"))         
gradesA02 <- read_xlsx(path=from_fast_A02,col_types = c("text","text","numeric","text","text"))

gradesA01 <- left_join(gradesA01,grades)%>%
  select(-Grade)%>%
  rename(Grade=final)%>%
  relocate(Grade, .after = Name)%>%
  mutate(`F or N`=ifelse(Grade<50,"F",NA))%>%
  as.data.frame()

gradesA02 <- left_join(gradesA02,grades)%>%
  select(-Grade)%>%
  rename(Grade=final)%>%
  relocate(Grade, .after = Name)%>%
  mutate(`F or N`=ifelse(Grade<50,"F",NA))%>%
  as.data.frame()

write.xlsx(gradesA01, "grades_A01.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(gradesA02, "grades_A02.xlsx", showNA = FALSE, row.names = FALSE)


 