#TFM: EUROPEAN IDENTITY

# ---- Join data ----

# Open data and select individual, socio-demographic and political opinion variable available in the 4 databases (years 2020-2023) 

rm(list = ls())

library(haven)
datos_euro_2023 <- read_dta("EUROBAR/ZA7953_2023.dta")
datos_euro_2022 <- read_dta("EUROBAR/ZA7848_2022.dta")
datos_euro_2021 <- read_dta("EUROBAR/ZA7780_2021.dta")
datos_euro_2020 <- read_dta("EUROBAR/ZA7649_2020.dta")

library(dplyr)
datos_euro_2023 <- datos_euro_2023 |>
  select (studyno1, isocntry,
          d71_1, d71_2, #usually speaks about politics (national and european)
          qa1_1, qa1_3, qa1_5, qa1_6, #judge situation: country, economy, EU...
          qa5_1, qa5_2, qa5_3, qa5_4, qa5_5, qa5_6, qa5_7, qa5_9, qa5_10, qa5_11, qa5_12, qa5_13, #EU issues: immigration, inflation,...
          d73_1, d73_2, #situation country and EU
          qa6_2, qa6_3, qa6_4, qa6_9, qa6_11, qa6_12, #trust in institutions, political parties, government... 
          qa11_1, qa11_3, # trust in EU institutions
          qa12_1, #knows how EU works
          sd18a, sd18b, #satisfied with democracy in country and EU
          qb1_1, #interests of country are taken into account
          qd1_1, qd1_2,qd1_3, qd1_4, #dep variable: attached to city, country, EU and Europe
          qd4_1, qd4_2, qd4_3, qd4_4, qd4_5, qd4_6, qd4_9, qd4_12, #importance for EU community: history, culture, religion...
          sd20a_t2, #political knowledge: 0 correct answers
          d8r1, d10, d11, d25, d60,d63, d1,  #sociodemographics: education level, age, gender, political scale, social class...
          qc1_3, #satisfaction with EU covid measures
          qe1_2 # satisfaction with russia-ukraine measures (only available in 2023)
        )

datos_euro_2022 <- datos_euro_2022 |>
  select (studyno1,isocntry, d71_1, d71_2, qa1_1, qa1_3,  qa1_5, qa1_6, qa5_1,
          qa5_2, qa5_3, qa5_4, qa5_5, qa5_6, qa5_7, qa5_9, qa5_10, qa5_11, qa5_12, 
          qa5_13, d73_1, d73_2, qa6b_1, qa6b_2, qa6b_3, qa6b_8, qa6b_10, qa6b_11, 
          qa8_1, qa8_3, qa9_1, sd18a, sd18b, qb1_1, qc1a_1, qc1a_2, qc1a_3, qc1a_4,
          qc4_1, qc4_2, qc4_3, qc4_4, qc4_5, qc4_6, qc4_9, qc4_12, sd20a_t2, d8r1, d10,
          d11, d25, d60,d63, d1, qa12_3
  )

datos_euro_2021 <- datos_euro_2021 |>
  select (studyno1, isocntry, d71_1, d71_2, qa1a_1, qa1a_3, qa1a_5, qa1a_6,qa5_1,
          qa5_2, qa5_3, qa5_4, qa5_5, qa5_6, qa5_7, qa5_9, qa5_10, qa5_11, qa5_12, 
          qa5_13, d73_1, d73_2, qa6b_1, qa6b_2, qa6b_3, qa6b_8, qa6b_10, qa6b_11, 
          qa8_1, qa8_3,qa9_1,sd18a, sd18b, qb1_1, qc1a_1, qc1a_2, qc1a_3, qc1a_4,
          qc4_1, qc4_2, qc4_3, qc4_4,qc4_5, qc4_6, qc4_9, qc4_12, sd22t2, d8r1, d10,
          d11, d25, d60, d63, d1, qa10_3
  )


datos_euro_2020 <- datos_euro_2020 |>
  select (studyno1, isocntry, d71a_1, d71a_2, qa1a_1, qa1a_3, qa1a_5, qa1a_6, qa5_1,
          qa5_2, qa5_3, qa5_4, qa5_5, qa5_6, qa5_7, qa5_9, qa5_10, qa5_11, qa5_12, 
          qa5_13,  d73a_1, d73a_2, qa6a_2, qa6a_3, qa6a_4, qa6a_9, qa6a_11, qa6a_12, 
          qa12_1, qa12_3,qa13_1, sd18a, sd18b,  qb1_1,
          qc1a_1, qc1a_2, qc1a_3, qc1a_4, qc4_1, qc4_2, qc4_3, qc4_4, qc4_5, qc4_6, qc4_9, qc4_12,
          sd20t2, d8r1, d10, d11, d25, d60, d63, d1, qa21a_2
  )


# We rename some variables for the join
# We try to keep the name similar to the codebook so it's easier to work with them

datos_euro_2023 <- datos_euro_2023 |>
  rename(year = studyno1) |>
  mutate(year = "2023") |>
  rename (
    qc1a_1 = qd1_1, qc1a_2 = qd1_2, qc1a_3 = qd1_3, qc1a_4 = qd1_4,
    qc4_1=qd4_1, qc4_2 = qd4_2, qc4_3 = qd4_3, qc4_4 = qd4_4, qc4_5 = qd4_5,
    qc4_6 = qd4_6, qc4_9 = qd4_9, qc4_12 = qd4_12,
    qa6b_1 = qa6_2, qa6b_2 = qa6_3, qa6b_3 = qa6_4, qa6b_8 = qa6_9, qa6b_10 = qa6_11, qa6b_11 = qa6_12 
  )

datos_euro_2022 <- datos_euro_2022 |>
  rename(year = studyno1) |>
  mutate(year = "2022") |>
  rename(
    qa11_1 = qa8_1, qa11_3 = qa8_3, qa12_1 = qa9_1, qc1_3 = qa12_3
  )

datos_euro_2021 <- datos_euro_2021 |>
  rename(year = studyno1) |>
  mutate(year = "2021") |>
  rename(
    qa1_1 = qa1a_1, qa1_3 = qa1a_3, qa1_5 = qa1a_5, qa1_6 = qa1a_6,
    qa11_1 = qa8_1, qa11_3 = qa8_3,
    qa12_1 = qa9_1,
    sd20a_t2 = sd22t2,
    qc1_3 = qa10_3
  )

datos_euro_2020 <- datos_euro_2020 |>
  rename(year = studyno1) |>
  mutate(year = "2020") |>
  rename (
    d71_1 = d71a_1, d71_2 = d71a_2, 
    qa1_1 = qa1a_1, qa1_3 = qa1a_3, qa1_5 = qa1a_5, qa1_6 = qa1a_6,
    d73_1 = d73a_1, d73_2 = d73a_2,
    qa11_1 = qa12_1, qa11_3 = qa12_3,
    qa12_1 = qa13_1,
    qa6b_1 = qa6a_2, qa6b_2 = qa6a_3, qa6b_3 = qa6a_4, qa6b_8 = qa6a_9, qa6b_10 = qa6a_11, qa6b_11 = qa6a_12,
    sd20a_t2= sd20t2,
    qc1_3 = qa21a_2,
  )


# Select only countries from the EU:
eu_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
                  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
                  "PL", "PT", "RO", "SK", "SI", "ES", "SE")

datos_euro_2023 <- datos_euro_2023 |> 
  mutate(isocntry = recode(isocntry, "DE-E" = "DE", "DE-W" = "DE")) |>
  filter(isocntry %in% eu_countries)
datos_euro_2022 <- datos_euro_2022 |> 
  mutate(isocntry = recode(isocntry, "DE-E" = "DE", "DE-W" = "DE")) |>
  filter(isocntry %in% eu_countries)
datos_euro_2021 <- datos_euro_2021 |> 
  mutate(isocntry = recode(isocntry, "DE-E" = "DE", "DE-W" = "DE")) |>
  filter(isocntry %in% eu_countries)
datos_euro_2020 <- datos_euro_2020 |> 
  mutate(isocntry = recode(isocntry, "DE-E" = "DE", "DE-W" = "DE")) |>
  filter(isocntry %in% eu_countries)
  

# Before joining the databases I had problems with the labelled columns, so we convert the variables to numeric and factors:
convertir_a_numeric <- function(df) {
  df[, names(df) != "isocntry"] <- lapply(df[, names(df) != "isocntry"], as.numeric)
  df$isocntry <- as.factor(df$isocntry) 
  return(df)
 }

datos_euro_2023 <- convertir_a_numeric(datos_euro_2023)
datos_euro_2022 <- convertir_a_numeric(datos_euro_2022)
datos_euro_2021 <- convertir_a_numeric(datos_euro_2021)
datos_euro_2020 <- convertir_a_numeric(datos_euro_2020)


#Now we join
datos <- bind_rows(datos_euro_2023, datos_euro_2022, datos_euro_2021, datos_euro_2020)

# Add country-level variables

# Excel manually created: year of joining the EU , years of using the euro and Goverment ideology (1-5) from CIBOB
library(readxl)
library (tidyr)

agregadas <- read_excel("agregadas/agregadas.xlsx")

datos <- datos |>
  left_join(agregadas, by = c("isocntry", "year"))

# From World Bank:
# Unemployment rate:
world_bank <- read_excel("agregadas/world_bank.xls", sheet = 1)

world_bank <- world_bank |>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "unemployment_rate") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(world_bank, by = c("isocntry", "year"))

#Gini:
world_bank_gini <- read_excel("agregadas/world_bank.xls", sheet = 2)
world_bank_gini <- world_bank_gini |>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "gini_index") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(world_bank_gini, by = c("isocntry", "year"))


#Net migration:
world_bank_mig <- read_excel("agregadas/world_bank.xls", sheet = 3)
world_bank_mig <- world_bank_mig |>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "migration") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(world_bank_mig, by = c("isocntry", "year"))


# From Eurostat (GDP per capita):
eurostat <- read_excel("agregadas/gdp_eurostat.xlsx", sheet = 1)

eurostat <- eurostat |>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "gdp") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(eurostat, by = c("isocntry", "year"))

# From Quality of Government: government corruption index 

qog <- read_excel("agregadas/qog.xlsx", sheet = 1)
datos <- datos |>
  left_join(qog, by = c("isocntry", "year"))


#To check isocntry names
uniq_datos <- unique(datos$isocntry)
uniq_qog <- unique(qog$isocntry)

setdiff(uniq_qog, uniq_datos)  

# ---- Clean data ----

# See NAs

datos |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "NAs") |>
  arrange(desc(NAs))


# ---- Descriptive analysis ----
