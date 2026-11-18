################################################################################
# by Pasha Lambre, 2026
# paololambre0@gmail.com
################################################################################

################################################################################
# NYDOCCS Category Analysis
################################################################################

setwd("C:/Users/Paolo/Desktop/PI Report")

library(dplyr)
library(stringr)
library(dplyr)

NYDOCCS <- read.csv("Incarcerated_Individuals_Under_Custody__Beginning_2008_20261117.csv")
NYDOCCS_2024 <- NYDOCCS[NYDOCCS$Snapshot.Year == 2024, ]
table(NYDOCCS_2024$Most.Serious.Crime)

violent_patterns <- c(
  #Homicide, Manslaughter
  "MURDER", "MANSLAUGHTER", "CRIM NEGLI HOMICIDE", "VEHICULR MANSL", "AGGRAVATED MURDER",
  #Assault, Gang Assault
  "ASSAULT", "GANG ASSAULT", "RECKLESS ASLT", "STRANGULATION", 
  #Robbery, Burglary
  "ROBBERY", "BURGLARY 1ST", "BURGLARY 2ND", "ATT BURGLARY 1ST", "ATT BURGLARY 2ND", 
  #Sex Offenses
  "SEX ABUSE", "SEX ACT", "RAPE", "SODOMY", "INCEST", "PRED", "PROMO CHILD PORN", 
  "CHILD PORNOGRAPHY", "FAC SEX PERF CHILD", "SEX TRAFFICKING", "OBSENITY", "DIS INDEC MAT",
  #Weapons, Arson
  "WEAPON", "FIREARM", "ARSON", "EXPLOS",
  #Kidnapping, Coercion, Stalking, Restraint
  "KIDNAPPING", "COERCION", "INTIMIDATE WIT", "INTIMI WIT", "ARREST", "ESCAPE",
  #Public Safety
  "POLICE", "FIRE/EMT", "MENACING", "RECK ENDANG", "RIOT", "TERROR",
  #Youthful Offender VFO
  "YO-JO", "JO-", "YO ASSAULT", "YO ATT MURDER", "YO ATT ROBBERY", "YO BURGLARY", 
  "YO DANG WEAPONS", "YO MANSLAUGHTER", "YO RAPE", "YO ROBBERY"
)

vulnerability_regex <- paste(violent_patterns, collapse = "|")

NYDOCCS_2024 <- NYDOCCS_2024 %>%
  mutate(
    Offender_Type = if_else(
      str_detect(Most.Serious.Crime, regex(vulnerability_regex, ignore_case = TRUE)),
      "VFO charge",
      "Non-VFO charge"
    )
  )

table(NYDOCCS_2024$Offender_Type)


NYDOCCS_2024 <- NYDOCCS_2024 %>%
  mutate(
    Age_Group = case_when(
      Current.Age <= 24 ~ "24 or younger",
      Current.Age >= 25 & Current.Age <= 39 ~ "25–39",
      Current.Age >= 40 & Current.Age <= 54 ~ "40–54",
      Current.Age >= 55 & Current.Age <= 64 ~ "55–64",
      Current.Age >= 65 ~ "65 or older",
      TRUE ~ NA_character_
    ),
    Age_Group = factor(
      Age_Group, 
      levels = c("24 or younger", "25–39", "40–54", "55–64", "65 or older")
    )
  )


NYDOCCS_race_vfo <- table(NYDOCCS_2024$Race.Ethnicity, NYDOCCS_2024$Offender_Type)
NYDOCCS_age_vfo <- table(NYDOCCS_2024$Age_Group, NYDOCCS_2024$Offender_Type)
NYDOCCS_race_age <- table(NYDOCCS_2024$Race.Ethnicity, NYDOCCS_2024$Age_Group)
