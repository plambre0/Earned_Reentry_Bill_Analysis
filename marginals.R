################################################################################
# by Pasha Lambre, 2026
# paololambre0@gmail.com
################################################################################

library(surveysd)
library(data.table)

################################################################################
# Function Declaration
################################################################################
make_constraint <- function(data, variable, measure) {
  temp <- data.frame(
    category = data[[variable]],
    value = data[[measure]]
  )
  names(temp)[1] <- variable
  tab <- xtabs(value ~ ., data = temp)
  tab * (N / sum(tab))
}


################################################################################
# Parole IPF
################################################################################
source("NYDOCCS_categories.r")

seed_parole <- as.data.table(expand.grid(
  Age.group = p_parole_age$Age.group,
  Race.Ethnicity = p_parole_race$Race.Ethnicity,
  VFO.designation = p_parole_vfo$VFO.designation,
  Eligibility.group = p_parole_past$Eligibility.group
))

seed_parole[, pWeight := 1]

race_age <- as.matrix(NYDOCCS_race_age)
race_vfo <- as.matrix(NYDOCCS_race_vfo)
age_vfo <- as.matrix(NYDOCCS_age_vfo)

race_age_ratio <- race_age / outer(rowSums(race_age), colSums(race_age)) * sum(race_age)

race_vfo_ratio <- race_vfo / outer(rowSums(race_vfo), colSums(race_vfo)) * sum(race_vfo)

age_vfo_ratio <- age_vfo / outer(rowSums(age_vfo), colSums(age_vfo)) * sum(age_vfo)

seed_parole[, pWeight := 
              mapply(function(r, a) race_age_ratio[r, a], Race.Ethnicity, Age.group) *
              mapply(function(r, v) race_vfo_ratio[r, v], Race.Ethnicity, VFO.designation) *
              mapply(function(a, v) age_vfo_ratio[a, v], Age.group, VFO.designation)
]

seed_parole[, pWeight := pWeight / sum(pWeight) * 100000]

N <- 100000

con_age_parole <- make_constraint(
  p_parole_age, "Age.group", "Releases"
)

con_race_parole <- make_constraint(
  p_parole_race, "Race.Ethnicity", "Releases"
)

con_vfo_parole <- make_constraint(
  p_parole_vfo, "VFO.designation", "Releases"
)

con_past_parole <- make_constraint(
  p_parole_past, "Eligibility.group", "Releases"
)

parole_ipf <- ipf(
  seed_parole,
  conP = list(
    con_age_parole,
    con_race_parole,
    con_vfo_parole,
    con_past_parole
  ),
  w = "pWeight",
  bound = NULL,
  maxIter = 2000,
  epsP = 1e-4,
  verbose = TRUE
)

################################################################################
# Recidivism IPF
################################################################################

race_offense <- copy(as.data.table(p_race_offense))

setnames(
  race_offense,
  "Most.serious.commitment.offense",
  "Type"
)

race_offense_long <- melt(
  race_offense,
  id.vars = "Type",
  variable.name = "Race",
  value.name = "Count"
)

race_offense_long[, Race := gsub("\\.$", "", Race)]

race_offense_long[Race == "Asian.Native.Hawaiian.Other.Pacific.Islander",
                  Race := "Asian/Native Hawaiian/Other Pacific Islander"]

race_offense_long[Race == "American.Indian.Alaska.Native",
                  Race := "American Indian/Alaska Native"]

race_offense_matrix <- dcast(
  race_offense_long,
  Race ~ Type,
  value.var = "Count"
)

race_offense_matrix <- as.matrix(
  race_offense_matrix[, -1, with = FALSE]
)

rownames(race_offense_matrix) <- race_offense_long[
  !duplicated(Race), Race
]

race_offense_ratio <- race_offense_matrix /
  outer(
    rowSums(race_offense_matrix),
    colSums(race_offense_matrix)
  ) * sum(race_offense_matrix)


vogel_age <- copy(as.data.table(p_race_age_vogel))

vogel_age[, Age := fcase(
  `Age range` %in% c("18–19", "20–24"), "24 or younger",
  `Age range` %in% c("25–29", "30–34", "35–39"), "25–39",
  `Age range` %in% c("40–44", "45–49", "50–54"), "40–54",
  `Age range` %in% c("55–59", "60–64"), "55–64",
  `Age range` == "65+", "65 or older"
)]

vogel_long <- melt(
  vogel_age,
  id.vars = c("Age"),
  measure.vars = c("White", "Black", "Hispanic"),
  variable.name = "Race",
  value.name = "Share"
)

vogel_age_race <- vogel_long[
  ,
  .(Share = sum(Share)),
  by = .(Age, Race)
]

vogel_age_matrix <- dcast(
  vogel_age_race,
  Age ~ Race,
  value.var = "Share"
)

vogel_age_matrix <- as.matrix(
  vogel_age_matrix[, -1, with = FALSE]
)

rownames(vogel_age_matrix) <- vogel_age_race[
  !duplicated(Age), Age
]

vogel_age_ratio <- vogel_age_matrix /
  outer(
    rowSums(vogel_age_matrix),
    colSums(vogel_age_matrix)
  ) * sum(vogel_age_matrix)


seed_recidivism <- as.data.table(expand.grid(
  Age = p_recidivism_age$Age,
  Race = p_recidivism_race$Race,
  Type = p_recidivism_vfo$Type
))

seed_recidivism[, recidivismWeight :=
                  mapply(
                    function(a, r) {
                      if (r %in% colnames(vogel_age_ratio)) {
                        vogel_age_ratio[a, r]
                      } else {
                        1
                      }
                    },
                    Age, Race
                  ) *
                  mapply(
                    function(r, o) {
                      if (r %in% rownames(race_offense_ratio) &&
                          o %in% colnames(race_offense_ratio)) {
                        race_offense_ratio[r, o]
                      } else {
                        1
                      }
                    },
                    Race, Type
                  )
]

seed_recidivism[, recidivismWeight :=
                  recidivismWeight / sum(recidivismWeight) * 100000
]

con_age_recidivism <- make_constraint(
  p_recidivism_age, "Age", "N"
)

con_race_recidivism <- make_constraint(
  p_recidivism_race, "Race", "N"
)

con_vfo_recidivism <- make_constraint(
  p_recidivism_vfo, "Type", "N"
)

recidivism_ipf <- ipf(
  seed_recidivism,
  conP = list(
    con_age_recidivism,
    con_race_recidivism,
    con_vfo_recidivism
  ),
  w = "recidivismWeight",
  bound = NULL,
  maxIter = 2000,
  epsP = 1e-4,
  verbose = TRUE
)


################################################################################
# Labor IPF
################################################################################

################################################################################
# Public Assistance IPF
################################################################################