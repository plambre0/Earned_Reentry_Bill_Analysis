################################################################################
# by Pasha Lambre, 2026
# paololambre0@gmail.com
################################################################################

setwd("C:/Users/Paolo/Desktop/PI Report")
set.seed(11182026)

################################################################################
# Import Statistical Tables
################################################################################
life_table <- read.csv("cdc_life_table.csv")
life_table[, 2:19] <- life_table[, 2:19]/100000
p_parole_age <- read.csv("vera_parole_age.csv")
p_parole_race <- read.csv("vera_parole_race.csv")
p_parole_vfo <- read.csv("vera_parole_vfo.csv")
p_parole_past <- read.csv("vera_parole_past.csv")
p_recidivism_age <- read.csv("bjs_recidivism_age.csv")
p_recidivism_race <- read.csv("bjs_recidivism_race.csv")
p_recidivism_vfo <- read.csv("bjs_recidivism_vfo.csv")
p_offense_served <- read.csv("bjs_offense_served.csv")
p_race_offense <- read.csv("bjs_race_offense.csv")
p_race_age_vogel <- read.csv("vogel_race_age.csv")[2:12, c(1, 3, 7, 11)]
p_race_age_vogel <- as.data.frame(p_race_age_vogel)
p_race_age_vogel[, 2:4] <- sapply(p_race_age_vogel[,2:4], as.numeric)
names(p_race_age_vogel) <- c("Age range", "White", "Black", "Hispanic")

################################################################################
# Declare Parameters
################################################################################
incarceration_cost <- 89408
supervision_cost <- 5814
recidivism_cost <- 151662
hearing_cost <- 200.55
po_supervisor_salary <- 95984.95
office_admin_salary <- 46229

################################################################################
# Set Marginal Probability Estimates
################################################################################
source("marginals.r")

################################################################################
# IDOC Data
################################################################################
source("load_IDOC.r")

################################################################################
# Analyses
################################################################################

