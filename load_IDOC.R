library(readxl)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(clipr)
library(tidyr)

################################################################################
# IDOC Datasets
################################################################################

files_IDOC <- c(
  "C:/Users/Paolo/Desktop/PI Report/internet-data-set-prison-stock-dec-2016.xls",
  "C:/Users/Paolo/Desktop/PI Report/internet-data-set-prison-stock-dec-2017.xls",
  "C:/Users/Paolo/Desktop/PI Report/december-2018-prison-stock.xls",
  "C:/Users/Paolo/Desktop/PI Report/dec-31-2019-prison-stock-pop.xls",
  "C:/Users/Paolo/Desktop/PI Report/dec-2020-prison-stock.xls",
  "C:/Users/Paolo/Desktop/PI Report/december-2021-prison-stock.xls",
  "C:/Users/Paolo/Desktop/PI Report/Dec-2022-Prison-Population-Data-Set.xls",
  "C:/Users/Paolo/Desktop/PI Report/December-2023-Prison.xls",
  "C:/Users/Paolo/Desktop/PI Report/December-2024-Prison.xls",
  "C:/Users/Paolo/Desktop/PI Report/December-2025-Prison.xls"
)

year_list <- 2016:2025

IDOC <- files_IDOC %>%
  set_names(year_list) %>%
  map(~ read_excel(.x, skip = 5, col_types = "text"))

names(IDOC$`2018`)[7] <- c("Current Admission Date")

clean_date_col <- function(col) {
  x <- str_trim(as.character(col))
  p <- str_pad(x, 8, pad = "0")
  
  case_when(
    str_detect(x, "^\\d{1,5}(\\.\\d+)?$") ~
      as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30"),
    str_detect(x, "^\\d{8}$") & between(as.integer(str_sub(x, 1, 4)), 1800, 2100) ~
      ymd(x, quiet = TRUE),
    str_detect(x, "^\\d{7,8}$") ~ mdy(p, quiet = TRUE),
    TRUE ~ as.Date(parse_date_time(x, orders = c("ymd HMS", "ymd", "mdy"), quiet = TRUE))
  )
}

IDOC <- IDOC %>%
  map(~ mutate(.x,
               `Date of Birth`          = clean_date_col(`Date of Birth`),
               `Sentence Date`          = clean_date_col(`Sentence Date`),
               `Current Admission Date` = clean_date_col(`Current Admission Date`)
  ))

IDOC_all <- bind_rows(IDOC, .id = "Year") %>%
  mutate(
    Year = as.integer(Year),
    Age_at_admission = floor(time_length(interval(`Date of Birth`, `Current Admission Date`), "years")),
    Age_dataset_year = floor(time_length(interval(`Date of Birth`, make_date(Year, 12, 31)), "years"))
  )

C_IDOC <- IDOC_all %>%
  filter(`Sentence Date` >= as.Date("1900-01-01"),
         `Sentence Date` <= as.Date("1978-01-31"))

C_IDOC %>% count(Year, `Crime Class`)     %>% pivot_wider(names_from = Year, values_from = n)
C_IDOC %>% count(Year, Race)              %>% pivot_wider(names_from = Year, values_from = n)
C_IDOC %>% count(Year, Age_dataset_year)  %>% pivot_wider(names_from = Year, values_from = n)

C_IDOC %>%
  mutate(Age_group = cut(Age_dataset_year, c(0, 49, 59, 69, 79, Inf),
                         labels = c("<50", "50-59", "60-69", "70-79", "80+"))) %>%
  count(Year, Age_group) %>%
  pivot_wider(names_from = Year, values_from = n)

C_IDOC %>%
  filter(Age_dataset_year < 50 | is.na(Age_dataset_year)) %>%
  select(Year, `Date of Birth`, `Sentence Date`, `Current Admission Date`, Age_dataset_year)

C_IDOC <- C_IDOC %>%
  mutate(
    Age_group = cut(Age_dataset_year,
                    breaks = c(25, 40, 55, 65, Inf),
                    labels = c("25–39", "40–54", "55–64", "65_or_older"),
                    right  = FALSE,
                    ordered_result = TRUE),
    Race_clean  = str_to_upper(str_trim(Race)),
    Class_clean = str_to_upper(str_trim(`Crime Class`))
  )

C_IDOC %>% count(Race_clean)
C_IDOC %>% count(Class_clean)
C_IDOC %>% count(Age_group)

yearly_table_IDOC <- C_IDOC %>%
  group_by(Year) %>%
  summarise(
    `25–39_IDOC`        = sum(Age_group == "25–39",       na.rm = TRUE),
    `40–54_IDOC`        = sum(Age_group == "40–54",       na.rm = TRUE),
    `55–64_IDOC`        = sum(Age_group == "55–64",       na.rm = TRUE),
    `65_or_older_IDOC`  = sum(Age_group == "65_or_older", na.rm = TRUE),
    
    Black_IDOC          = sum(str_detect(Race_clean, "BLACK"),    na.rm = TRUE),
    Hispanic_IDOC       = sum(str_detect(Race_clean, "HISPANIC"), na.rm = TRUE),
    White_IDOC          = sum(str_detect(Race_clean, "WHITE"),    na.rm = TRUE),
    
    Class_1_IDOC        = sum(Class_clean %in% c("CLASS 1", "1"), na.rm = TRUE),
    Class_2_IDOC        = sum(Class_clean %in% c("CLASS 2", "2"), na.rm = TRUE),
    Class_3_IDOC        = sum(Class_clean %in% c("CLASS 3", "3"), na.rm = TRUE),
    Class_4_IDOC        = sum(Class_clean %in% c("CLASS 4", "4"), na.rm = TRUE),
    Class_X_IDOC        = sum(Class_clean %in% c("CLASS X", "X"), na.rm = TRUE),
    Murder_IDOC         = sum(str_detect(Class_clean, "MURDER") | Class_clean %in% c("M", "CLASS M"),
                                 na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Year))

yearly_table_IDOC
clipr::write_clip(yearly_table_IDOC)

################################################################################
# Parole Datasets
################################################################################

files_parole <- c(
  "C:/Users/Paolo/Desktop/PI Report/december-2016-parole-stock-pop.xls",
  "C:/Users/Paolo/Desktop/PI Report/december-2017-stock-pop.xls",
  "C:/Users/Paolo/Desktop/PI Report/dec-2018-parole-stock-population.xls",
  "C:/Users/Paolo/Desktop/PI Report/december-2019-parole-stock-pop.xls",
  "C:/Users/Paolo/Desktop/PI Report/december-2020-parole-stock.xls",
  "C:/Users/Paolo/Desktop/PI Report/parole-stock-12.31.2021.xls",
  "C:/Users/Paolo/Desktop/PI Report/Dec2022-Parole-Population-Final.xls",
  "C:/Users/Paolo/Desktop/PI Report/December-2023-Parole.xls",
  "C:/Users/Paolo/Desktop/PI Report/Dec-2024-Parole-MSR.xls",
  "C:/Users/Paolo/Desktop/PI Report/Parole-Population-December-2025.xls",
  "C:/Users/Paolo/Desktop/PI Report/December-2026-Prison.xls"
)

year_list <- 2016:2025

parole <- files_parole %>%
  set_names(year_list) %>%
  map(~ read_excel(.x, skip = 5, col_types = "text"))

clean_date_col <- function(col) {
  x <- str_trim(as.character(col))
  p <- str_pad(x, 8, pad = "0")
  
  case_when(
    str_detect(x, "^\\d{1,5}(\\.\\d+)?$") ~
      as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30"),
    str_detect(x, "^\\d{8}$") & between(as.integer(str_sub(x, 1, 4)), 1800, 2100) ~
      ymd(x, quiet = TRUE),
    str_detect(x, "^\\d{7,8}$") ~ mdy(p, quiet = TRUE),
    TRUE ~ as.Date(parse_date_time(x, orders = c("ymd HMS", "ymd", "mdy"), quiet = TRUE))
  )
}

parole <- parole %>%
  map(~ mutate(.x,
               `Date of Birth`          = clean_date_col(`Date of Birth`),
               `Sentence Date`          = clean_date_col(`Sentence Date`),
               `Current Admission Date` = clean_date_col(`Current Admission Date`)
  ))

parole_all <- bind_rows(parole, .id = "Year") %>%
  mutate(
    Year = as.integer(Year),
    Age_at_admission = floor(time_length(interval(`Date of Birth`, `Current Admission Date`), "years")),
    Age_dataset_year = floor(time_length(interval(`Date of Birth`, make_date(Year, 12, 31)), "years"))
  )

C_paroled <- parole_all %>%
  filter(`Sentence Date` >= as.Date("1900-01-01"),
         `Sentence Date` <= as.Date("1978-01-31"))

C_paroled %>% count(Year, `Crime Class`)     %>% pivot_wider(names_from = Year, values_from = n)
C_paroled %>% count(Year, Race)              %>% pivot_wider(names_from = Year, values_from = n)
C_paroled %>% count(Year, Age_dataset_year)  %>% pivot_wider(names_from = Year, values_from = n)

C_paroled %>%
  mutate(Age_group = cut(Age_dataset_year, c(0, 49, 59, 69, 79, Inf),
                         labels = c("<50", "50-59", "60-69", "70-79", "80+"))) %>%
  count(Year, Age_group) %>%
  pivot_wider(names_from = Year, values_from = n)

C_paroled %>%
  filter(Age_dataset_year < 50 | is.na(Age_dataset_year)) %>%
  select(Year, `Date of Birth`, `Sentence Date`, `Current Admission Date`, Age_dataset_year)

C_paroled <- C_paroled %>%
  mutate(
    Age_group = cut(Age_dataset_year,
                    breaks = c(25, 40, 55, 65, Inf),
                    labels = c("25–39", "40–54", "55–64", "65_or_older"),
                    right  = FALSE,           # 25–39 means 25 <= age < 40
                    ordered_result = TRUE),
    Race_clean  = str_to_upper(str_trim(Race)),
    Class_clean = str_to_upper(str_trim(`Crime Class`))
  )

C_paroled %>% count(Race_clean)
C_paroled %>% count(Class_clean)
C_paroled %>% count(Age_group)

yearly_table_parole <- C_paroled %>%
  group_by(Year) %>%
  summarise(
    `25–39_Paroled`        = sum(Age_group == "25–39",       na.rm = TRUE),
    `40–54_Paroled`        = sum(Age_group == "40–54",       na.rm = TRUE),
    `55–64_Paroled`        = sum(Age_group == "55–64",       na.rm = TRUE),
    `65_or_older_Paroled`  = sum(Age_group == "65_or_older", na.rm = TRUE),
    
    Black_Paroled          = sum(str_detect(Race_clean, "BLACK"),    na.rm = TRUE),
    Hispanic_Paroled       = sum(str_detect(Race_clean, "HISPANIC"), na.rm = TRUE),
    White_Paroled          = sum(str_detect(Race_clean, "WHITE"),    na.rm = TRUE),
    
    Class_1_Paroled        = sum(Class_clean %in% c("CLASS 1", "1"), na.rm = TRUE),
    Class_2_Paroled        = sum(Class_clean %in% c("CLASS 2", "2"), na.rm = TRUE),
    Class_3_Paroled        = sum(Class_clean %in% c("CLASS 3", "3"), na.rm = TRUE),
    Class_4_Paroled        = sum(Class_clean %in% c("CLASS 4", "4"), na.rm = TRUE),
    Class_X_Paroled        = sum(Class_clean %in% c("CLASS X", "X"), na.rm = TRUE),
    Murder_Paroled         = sum(str_detect(Class_clean, "MURDER") | Class_clean %in% c("M", "CLASS M"),
                                 na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Year))

clipr::write_clip(yearly_table_parole)

################################################################################
# Collect IPRB C-Numbers Statistics
################################################################################

Parole_Annualized <- read.csv("C:/Users/Paolo/Desktop/PI Report/Parole_Annualized.csv")
Parole_Annualized <- Parole_Annualized %>% left_join(yearly_table_IDOC, by = "Year")

Parole_Annualized <- 
  C_paroled %>%
  group_by(Year) %>%
  summarise(`Average Age` = mean(Age_dataset_year)) %>% 
  left_join(Parole_Annualized)
