#TFM: EUROPEAN IDENTITY
#PREPROCESS DATA

rm(list = ls())

library(tidyr)
library(haven)
library (dplyr)
datos <- read_dta("datos.dta")

# ---- NA Imputation----

datos$attachment_EU <- as.factor(datos$attachment_EU)

# Delete variable for Russia-Ukraine war since we have data only for 2023 
datos_imp <- subset(datos, select = -russia_war)

# We see the NAs percentage of each variable
na_porcentaje <- datos_imp |>
  summarise(across(everything(), ~mean(is.na(.)) * 100)) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "porc_na") |>
  arrange(desc(porc_na))
print(na_porcentaje)

# We impute the NAs (except for the dependent variable)
library(mice)

vars_a_imputar <- c(
  "trust_EU_Bank", "trust_United_nations", "trust_EU_Parliament", "political_scale", 
  "rate_EU_economy", "trust_EU", "covid", "interests_country", 
  "satisfied_democracy_EU", "trust_government", "trust_justice", "trust_parties", 
  "rate_employ_country", "education_level", "trust_police", "knowledge_EUworks", 
  "rate_household", "social_class", "attachment_Europe", "rate_general_situation", 
  "satisfied_democracy_country", "difficulty_bills", "attachment_town", 
  "attachment_country", "gender", "rural_urban", "EU_issue_crime", "EU_issue_economy", 
  "EU_issue_inflation", "EU_issue_tax", "EU_issue_unemploy", "EU_issue_terrorism", 
  "EU_issue_influence", "EU_issue_immigration", "EU_issue_pensions", 
  "EU_issue_climate", "EU_issue_energy", "EU_issue_health", "importance_history_EU", 
  "importance_religion_EU", "importance_values_EU", "importance_geography_EU", 
  "importance_language_EU", "importance_law_EU", "importance_economy_EU", 
  "importance_culture_EU"
)

datos_a_imputar <- datos_imp[, vars_a_imputar]

mice_mod <- mice(datos_a_imputar, m = 5, method = 'rf', seed = 123)
datos_imputados <- complete(mice_mod, 5)
datos_imp[, vars_a_imputar] <- datos_imputados


write_dta(datos_imp, "datos_imp.dta")
