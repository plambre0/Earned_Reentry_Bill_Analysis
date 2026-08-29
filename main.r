################################################################################
# Written by Paolo Lambre in May 2026
# paololambre0@gmail.com
################################################################################

library(dplyr)
library(tidyr)

################################################################################
# Import CDC Life Tables
################################################################################
ssa_table <- matrix(c(.006064,100000,74.74,.005119,100000,80.18,
                      .000491,99394,74.20,.000398,99488,79.60,
                      .000309,99345,73.23,.000240,99449,78.63,
                      .000248,99314,72.25,.000198,99425,77.65,
                      .000199,99290,71.27,.000160,99405,76.66,
                      .000167,99270,70.29,.000134,99389,75.67,
                      .000143,99253,69.30,.000118,99376,74.68,
                      .000126,99239,68.31,.000109,99364,73.69,
                      .000121,99226,67.32,.000106,99353,72.70,
                      .000121,99214,66.32,.000106,99343,71.71,
                      .000127,99202,65.33,.000111,99332,70.72,
                      .000143,99190,64.34,.000121,99321,69.72,
                      .000171,99176,63.35,.000140,99309,68.73,
                      .000227,99159,62.36,.000162,99295,67.74,
                      .000320,99136,61.37,.000188,99279,66.75,
                      .000451,99104,60.39,.000224,99260,65.76,
                      .000622,99060,59.42,.000276,99238,64.78,
                      .000826,98998,58.46,.000337,99211,63.80,
                      .001026,98916,57.50,.000395,99177,62.82,
                      .001182,98815,56.56,.000450,99138,61.84,
                      .001301,98698,55.63,.000496,99094,60.87,
                      .001404,98570,54.70,.000532,99044,59.90,
                      .001498,98431,53.78,.000567,98992,58.93,
                      .001586,98284,52.86,.000610,98936,57.97,
                      .001679,98128,51.94,.000650,98875,57.00,
                      .001776,97963,51.03,.000699,98811,56.04,
                      .001881,97789,50.12,.000743,98742,55.08,
                      .001985,97605,49.21,.000796,98669,54.12,
                      .002095,97412,48.31,.000855,98590,53.16,
                      .002219,97208,47.41,.000924,98506,52.20,
                      .002332,96992,46.51,.000988,98415,51.25,
                      .002445,96766,45.62,.001053,98318,50.30,
                      .002562,96529,44.73,.001123,98214,49.35,
                      .002653,96282,43.84,.001198,98104,48.41,
                      .002716,96026,42.96,.001263,97986,47.47,
                      .002791,95765,42.08,.001324,97863,46.53,
                      .002894,95498,41.19,.001403,97733,45.59,
                      .002994,95222,40.31,.001493,97596,44.65,
                      .003091,94937,39.43,.001596,97450,43.72,
                      .003217,94643,38.55,.001700,97295,42.79,
                      .003353,94339,37.67,.001803,97129,41.86,
                      .003499,94022,36.80,.001905,96954,40.93,
                      .003642,93693,35.93,.002009,96769,40.01,
                      .003811,93352,35.05,.002116,96575,39.09,
                      .003996,92997,34.19,.002223,96371,38.17,
                      .004175,92625,33.32,.002352,96156,37.25,
                      .004388,92238,32.46,.002516,95930,36.34,
                      .004666,91833,31.60,.002712,95689,35.43,
                      .004973,91405,30.75,.002936,95429,34.53,
                      .005305,90950,29.90,.003177,95149,33.63,
                      .005666,90468,29.05,.003407,94847,32.73,
                      .006069,89955,28.22,.003642,94524,31.84,
                      .006539,89409,27.39,.003917,94180,30.96,
                      .007073,88825,26.56,.004238,93811,30.08,
                      .007675,88196,25.75,.004619,93413,29.20,
                      .008348,87520,24.94,.005040,92982,28.34,
                      .009051,86789,24.15,.005493,92513,27.48,
                      .009822,86003,23.37,.005987,92005,26.63,
                      .010669,85159,22.59,.006509,91454,25.78,
                      .011548,84250,21.83,.007067,90859,24.95,
                      .012458,83277,21.08,.007658,90217,24.12,
                      .013403,82240,20.34,.008305,89526,23.31,
                      .014450,81138,19.61,.008991,88782,22.50,
                      .015571,79965,18.89,.009681,87984,21.70,
                      .016737,78720,18.18,.010343,87132,20.90,
                      .017897,77402,17.48,.011018,86231,20.12,
                      .019017,76017,16.79,.011743,85281,19.34,
                      .020213,74572,16.11,.012532,84279,18.56,
                      .021569,73064,15.43,.013512,83223,17.79,
                      .023088,71488,14.76,.014684,82099,17.03,
                      .024828,69838,14.09,.016025,80893,16.27,
                      .026705,68104,13.44,.017468,79597,15.53,
                      .028761,66285,12.80,.019195,78206,14.80,
                      .031116,64379,12.16,.021195,76705,14.08,
                      .033861,62376,11.53,.023452,75079,13.37,
                      .037088,60263,10.92,.025980,73319,12.68,
                      .041126,58028,10.32,.029153,71414,12.00,
                      .045241,55642,9.74,.032394,69332,11.35,
                      .049793,53125,9.18,.035888,67086,10.71,
                      .054768,50479,8.64,.039676,64678,10.09,
                      .060660,47715,8.11,.044156,62112,9.49,
                      .067027,44820,7.60,.049087,59370,8.90,
                      .073999,41816,7.11,.054635,56455,8.34,
                      .081737,38722,6.64,.061066,53371,7.79,
                      .090458,35557,6.18,.068431,50112,7.26,
                      .100525,32340,5.75,.076841,46683,6.76,
                      .111793,29089,5.34,.086205,43095,6.28,
                      .124494,25837,4.94,.096851,39380,5.83,
                      .138398,22621,4.58,.109019,35566,5.40,
                      .153207,19490,4.23,.121867,31689,5.00,
                      .169704,16504,3.91,.135805,27827,4.62,
                      .187963,13703,3.60,.151108,24048,4.27,
                      .208395,11128,3.32,.168020,20414,3.94,
                      .230808,8809,3.06,.186340,16984,3.64,
                      .253914,6776,2.83,.206432,13819,3.36,
                      .277402,5055,2.63,.228086,10967,3.10,
                      .300882,3653,2.44,.250406,8465,2.87,
                      .324326,2554,2.28,.273699,6346,2.66,
                      .347332,1726,2.13,.296984,4609,2.47,
                      .369430,1126,2.00,.319502,3240,2.30,
                      .391927,710,1.88,.342716,2205,2.14,
                      .414726,432,1.76,.366532,1449,2.00,
                      .437722,253,1.66,.390844,918,1.87,
                      .460800,142,1.56,.415531,559,1.75,
                      .483840,77,1.47,.440463,327,1.63,
                      .508032,40,1.39,.466891,183,1.52,
                      .533434,19,1.31,.494904,97,1.42,
                      .560105,9,1.23,.524599,49,1.32,
                      .588111,4,1.15,.556075,23,1.23,
                      .617516,2,1.08,.589439,10,1.14,
                      .648392,1,1.01,.624805,4,1.05,
                      .680812,0,0.94,.662294,2,0.97,
                      .714852,0,0.87,.702031,1,0.89,
                      .750595,0,0.81,.744153,0,0.82,
                      .788125,0,0.75,.788125,0,0.75,
                      .827531,0,0.70,.827531,0,0.70,
                      .868907,0,0.64,.868907,0,0.64,
                      .912353,0,0.59,.912353,0,0.59,
                      .957970,0,0.54,.957970,0,0.54,
                      1.000000,0,0.50,1.000000,0,0.50), ncol = 6, byrow = TRUE)
ssa_table <- data.frame(ssa_table)
colnames(ssa_table) <- c("Male_Death_probability", 
                         "Male_Number_of_lives", 
                         "Male_Life_expectancy", 
                         "Female_Death_probability", 
                         "Female_Number_of_lives", 
                         "Female_Life_expectancy")
ssa_table$Age <- c(0:119)

################################################################################
# Import IPF Parole Probabilities
################################################################################

race_margins <- data.frame(
  race = c("Black", "White", "Latinx", "Native American", "Asian/PI"),
  heard = c(3052, 2312, 1311, 114, 47),
  granted = c(1042, 1138, 463, 49, 20)
) %>% mutate(rate = granted/heard)

age_margins <- data.frame(
  age = c("Under 25", "25-34", "35-44", "45-54", "55+"),
  heard = c(554, 2062, 2036, 1177, 1201),
  granted = c(145, 774, 888, 520, 478)
) %>% mutate(rate = granted/heard)

vfo_margins <- data.frame(
  vfo = c("Non-VFO", "VFO"),
  heard = c(5228, 1781),
  granted = c(2328, 470)
) %>% mutate(rate = granted/heard)

races <- race_margins$race
ages <- age_margins$age
vfos <- vfo_margins$vfo

joint <- expand.grid(race = races, age = ages, vfo = vfos,
                     stringsAsFactors = FALSE)

N_total <- sum(race_margins$heard)
joint$n <- N_total/nrow(joint)

ipf <- function(seed, race_m, age_m, vfo_m,
                max_iter = 200, tol = 1e-6){
  df <- seed
  for (i in seq_len(max_iter)){
    df_old <- df$n
    race_current <- df %>% group_by(race) %>% summarise(n_sum = sum(n))
    df <- df %>%
      left_join(race_current, by = "race") %>%
      left_join(race_m %>% select(race, heard), by = "race") %>%
      mutate(n = n * heard/n_sum) %>%
      select(-n_sum, -heard)
    age_current <- df %>% group_by(age) %>% summarise(n_sum = sum(n))
    df <- df %>%
      left_join(age_current, by = "age") %>%
      left_join(age_m %>% select(age, heard), by = "age") %>%
      mutate(n = n * heard/n_sum) %>%
      select(-n_sum, -heard)
    vfo_current <- df %>% group_by(vfo) %>% summarise(n_sum = sum(n))
    df <- df %>%
      left_join(vfo_current, by = "vfo") %>%
      left_join(vfo_m %>% select(vfo, heard), by = "vfo") %>%
      mutate(n = n * heard/n_sum) %>%
      select(-n_sum, -heard)
    max_delta <- max(abs(df$n - df_old))
    if (max_delta < tol){
      cat(sprintf("IPF converged in %d iterations (max delta = %.2e)\n", 
                  i, max_delta))
      return(df)
    }
  }
  warning("IPF did not converge — inspect margins for inconsistencies.")
  return(df)
}

joint_fitted <- ipf(joint, race_margins, age_margins, vfo_margins)

p_grand <- sum(race_margins$granted)/sum(race_margins$heard)
lo_grand <- log(p_grand/(1 - p_grand))

lo_race <- with(race_margins, log(rate/(1 - rate)) - lo_grand)
lo_age <- with(age_margins,  log(rate/(1 - rate)) - lo_grand)
lo_vfo <- with(vfo_margins,  log(rate/(1 - rate)) - lo_grand)

names(lo_race) <- race_margins$race
names(lo_age)  <- age_margins$age
names(lo_vfo)  <- vfo_margins$vfo

joint_fitted <- joint_fitted %>%
  mutate(
    lo_cell = lo_grand + lo_race[race] + lo_age[age] + lo_vfo[vfo],
    p_parole = exp(lo_cell)/(1 + exp(lo_cell)),
    n_granted_est = n * p_parole
  )

check_race <- joint_fitted %>%
  group_by(race) %>%
  summarise(
    n_fitted = sum(n),
    n_actual = race_margins$heard[match(first(race), race_margins$race)],
    grant_fit = sum(n_granted_est),
    grant_act = race_margins$granted[match(first(race), race_margins$race)],
    rate_fit = grant_fit/n_fitted,
    rate_act = grant_act/n_actual
  )
print(check_race)

check_age <- joint_fitted %>%
  group_by(age) %>%
  summarise(
    n_fitted = sum(n),
    n_actual = age_margins$heard[match(first(age), age_margins$age)],
    grant_fit = sum(n_granted_est),
    grant_act = age_margins$granted[match(first(age), age_margins$age)],
    rate_fit = grant_fit/n_fitted,
    rate_act = grant_act/n_actual
  )
print(check_age)

check_vfo <- joint_fitted %>%
  group_by(vfo) %>%
  summarise(
    n_fitted = sum(n),
    n_actual = vfo_margins$heard[match(first(vfo), vfo_margins$vfo)],
    grant_fit = sum(n_granted_est),
    grant_act = vfo_margins$granted[match(first(vfo), vfo_margins$vfo)],
    rate_fit = grant_fit/n_fitted,
    rate_act = grant_act/n_actual
  )
print(check_vfo)

joint_out <- joint_fitted %>%
  select(race, age, vfo, n_heard_est = n, p_parole, n_granted_est) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(vfo, race, age)

print(as.data.frame(joint_out))

write.csv(joint_out, "parole_ipf_joint_table.csv", row.names = FALSE)
################################################################################
pop_2025 <- readxl::read_xls("December-2025-Prison.xls")
parole_ipf_joint_table <- read.csv("parole_ipf_joint_table.csv")

pop_2025 <- as.data.frame(pop_2025)[1:30041, ]
colnames(pop_2025) <- pop_2025[4,]
pop_2025 <- pop_2025[5:nrow(pop_2025),]
rownames(pop_2025) <- c(1:nrow(pop_2025))

pop_2025$`Date of Birth` <- as.Date(as.numeric(pop_2025$`Date of Birth`), 
                                    origin = "1899-12-30")
pop_2025$`Sentence Date` <- as.Date(as.numeric(pop_2025$`Sentence Date`), 
                                    origin = "1899-12-30")
pop_2025$`Current Admission Date` <- 
  as.Date(as.numeric(pop_2025$`Current Admission Date`), origin = "1899-12-30")

pop_2025$Age <- -1 * as.numeric(difftime(pop_2025$`Date of Birth`, 
                                            as.Date('2026-01-01'))) / 365
pop_2025$Served <- -1 * as.numeric(difftime(pop_2025$`Current Admission Date`, 
                                            as.Date('2026-01-01'))) / 365

prisoners_yr <- pop_2025
years <- list()
years_eligable <- list()
years_out <- 10

for (yr in 1:years_out) {
  prisoners_yr$Age <- prisoners_yr$Age + 1
  prisoners_yr$Served <- prisoners_yr$Served + 1
  years[[yr]] <- prisoners_yr
}

for (yr in 1:years_out) {
  if (yr == 1) {
    years_eligable[[yr]] <- years[[yr]][years[[yr]]$Served >= 35, ]
  } else if (yr == 2) {
    years_eligable[[yr]] <- years[[yr]][years[[yr]]$Served >= 25, ]
  } else {
    years_eligable[[yr]] <- years[[yr]][years[[yr]]$Served >= 20, ]
  }
}

years_eligable <- lapply(years_eligable, function(x) {
  x %>%
    mutate(
      # Remap IDOC race labels to IPF table labels
      Race = case_when(
        Race == "American Indian" ~ "Native American",
        Race == "Asian"           ~ "Asian/PI",
        Race == "Hispanic"        ~ "Latinx",
        TRUE                      ~ Race
      ),
      # All eligible inmates are VFO under this bill
      vfo = "VFO",
      # Age buckets matching IPF table
      age = cut(Age, c(0, 24, 34, 44, 54, 200),
                labels = c("Under 25", "25-34", "35-44", "45-54", "55+"))
    ) %>%
    left_join(
      parole_ipf_joint_table %>% select(race, age, vfo, p_parole),
      by = c("Race" = "race", "age" = "age", "vfo" = "vfo")
    )
})

colnames(ssa_table) <- c("Male_Death_probability", "Male_Number_of_lives", "Male_Life_expectancy",
                         "Female_Death_probability", "Female_Number_of_lives", "Female_Life_expectancy",
                         "Age")

get_qx <- function(race, sex, age) {
  age_floor <- pmax(0, pmin(floor(age), max(ssa_table$Age)))
  row <- ssa_table[ssa_table$Age == age_floor, ]
  if (sex == "Female") row$Female_Death_probability else row$Male_Death_probability
}

years_eligable <- lapply(years_eligable, FUN = function(x) {
  x %>%
    rowwise() %>%
    mutate(p_death_1yr = get_qx(Race, Sex, Age)) %>%
    ungroup()
})

results <- lapply(seq_along(years_eligable), FUN = function(yr) {
  years_eligable[[yr]] %>%
    mutate(
      years_to_hearing = yr - 1,
      p_survive_to_hearing = (1 - p_death_1yr)^years_to_hearing,
      p_paroled = p_survive_to_hearing * p_parole,
      p_dies = 1 - p_survive_to_hearing,
      p_nothing = p_survive_to_hearing * (1 - p_parole)
    )
})

check <- results[[1]] %>%
  mutate(check = round(p_paroled + p_dies + p_nothing, 6)) %>%
  pull(check) %>%
  unique()
cat("Probability sum check (should be 1):", check, "\n\n")

ev_summary <- lapply(seq_along(results), FUN = function(yr) {
  results[[yr]] %>%
    summarise(
      year = yr,
      n_total = n(),
      EV_paroled = sum(p_paroled, na.rm = TRUE),
      EV_dies = sum(p_dies, na.rm = TRUE),
      EV_nothing = sum(p_nothing, na.rm = TRUE),
      EV_paroled_Black = sum(p_paroled[Race == "Black"], na.rm = TRUE),
      EV_paroled_White = sum(p_paroled[Race == "White"], na.rm = TRUE),
      EV_paroled_Latinx = sum(p_paroled[Race == "Latinx"], na.rm = TRUE),
      EV_paroled_AIAN = sum(p_paroled[Race == "Native American"], na.rm = TRUE),
      EV_paroled_Asian = sum(p_paroled[Race == "Asian/PI"], na.rm = TRUE),
      EV_paroled_VFO = sum(p_paroled[vfo == "VFO"], na.rm = TRUE)
    )
}) %>%
  bind_rows() %>% data.frame()

print(ev_summary)
