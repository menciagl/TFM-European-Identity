#TFM: EUROPEAN IDENTITY

# ---- Join data ----

# -- 1: Select individual-level variables from the Eurobarometer databases  ---- 

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

# To verify
colnames_2020 <- colnames(datos_euro_2020)
colnames_2021 <- colnames(datos_euro_2021)
colnames_2022 <- colnames(datos_euro_2022)
colnames_2023 <- colnames(datos_euro_2023)
setdiff(colnames_2022, colnames_2023)

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

# -- 2: Select country-level variables from Eurostat, World Bank, QoG...  ----

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

# Gini:
world_bank_gini <- read_excel("agregadas/world_bank.xls", sheet = 2)
world_bank_gini <- world_bank_gini |>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "gini_index") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(world_bank_gini, by = c("isocntry", "year"))


# From Eurostat
# GDP per capita:

eurostat_gdp <- read_excel("agregadas/eurostat.xlsx", sheet = 1)

eurostat_gdp <- eurostat_gdp |>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "gdp") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(eurostat_gdp, by = c("isocntry", "year"))

# Immigrants percentage:
eurostat_im <- read_excel("agregadas/eurostat.xlsx", sheet = 2)

eurostat_im <- eurostat_im |>
  select(isocntry, `2020`, `2021`, `2022`, `2023`)|>
  pivot_longer(cols = c("2023", "2022", "2021", "2020"), 
               names_to = "year",             
               values_to = "immigration") |>
  mutate(year = as.numeric(year))

datos <- datos |>
  left_join(eurostat_im, by = c("isocntry", "year"))


# From Quality of Government: absence of (government) corruption 

qog <- read_excel("agregadas/qog.xlsx", sheet = 1)
datos <- datos |>
  left_join(qog, by = c("isocntry", "year"))


# We check isocntry names in all databases:

uniq_datos <- unique(datos$isocntry)
uniq_eurostat <- unique(eurostat_im$isocntry)

setdiff(uniq_eurostat, uniq_datos)  

# ---- Clean data ----

# See NAs

# We recode the DK/NA responses and classify them as NA.

datos <- datos |>
  mutate(
    across(starts_with("71_") | starts_with("73_"), ~na_if(., 4)),
    across(starts_with("qa1_") | starts_with("qa12_") | starts_with("sd18") |
             starts_with("qb1_") | starts_with("qc1a_") | starts_with("qc1_") | 
             starts_with("qe1_"), ~na_if(., 5)),
    across(starts_with("qa6b_") | starts_with("qa11_") | starts_with("d10"), ~na_if(., 3)),
    d8r1 = ifelse(d8r1 %in% c(97, 98), NA, d8r1),
    d25 = ifelse(d25 == 8, NA, d25),
    d60 = ifelse(d60 == 7, NA, d60),
    d63 = ifelse(d63 %in% c(6,7,8,9), NA, d63),
    d1 = ifelse(d1 %in% c(97, 98), NA, d1)
  )

na_porcentaje <- datos |>
  summarise(across(everything(), ~mean(is.na(.)) * 100)) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "porc_na") |>
  arrange(desc(porc_na))

print(na_porcentaje)

# Qe1 has a lot of NAs (76%) because is the question about Ukraine and it was only asked in 2023
# The rest have low percentages of NAs (most below 3-4% of NAs).
# However, 2 var have around 10% of NAs: confidence in the European Central Bank and in the EU in general


# ---- Descriptive analysis ----

# -- 1: differences between countries: how many people are attached to the EU ----

# Group by country and see % of people having more EU identity

attachment_percentages <- datos |>
  filter(qc1a_3 %in% c(1, 2)) |>
  group_by(isocntry) |>
  summarise(attached = n()) |>
  left_join(
    datos |>
      group_by(isocntry) |>
      summarise(total = n()),
    by = "isocntry"
  ) |>
  mutate(percentage = (attached / total) * 100)

print(attachment_percentages)

country_name_map <- data.frame(
  isocntry = c("AT", "BE", "BG", "HR", "DK", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "NL", "PL", "PT", "ES", "SE", "GB", "CZ", "RO", "SK", "LT", "LV", "EE", "SI", "CY", "LU", "MT"),
  map_name = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Denmark", "Finland",
    "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
    "Netherlands", "Poland", "Portugal", "Spain", "Sweden", "UK",
    "Czech Republic", "Romania", "Slovakia", "Lithuania", "Latvia", "Estonia",
    "Slovenia", "Cyprus", "Luxembourg", "Malta"
  )
)

library(ggplot2)
library(mapdata)

# Mapa base
europe_map <- map_data("world", region = unique(country_name_map$map_name))

# Join 
europe_data <- europe_map |>
  left_join(country_name_map, by = c("region" = "map_name")) |>
  left_join(attachment_percentages, by = "isocntry")

# Map
ggplot(data = europe_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = percentage),
               color = "gray90", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "viridis", direction = -1,
    name = "%",
    na.value = "lightgray"
  ) +
  labs(
    title = "Attachment to the European Union (2020-2023)",
    subtitle = "Percentage of respondents feeling fairly and very 'attached' to the EU"
  ) +
  coord_quickmap() +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.4),
    plot.subtitle = element_text(size = 10, hjust = 0.4, color = "gray25"),
    legend.title = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# -- 2: differences over time (2020-23)  ----

evolution_data <- datos |>
  filter(year %in% 2020:2023, qc1a_3 %in% 1:4) |>
  group_by(year, qc1a_3) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(percentage = (count / sum(count)) * 100)

evolution_data$label <- factor(evolution_data$qc1a_3,
                               levels = 1:4,
                               labels = c("Very attached", "Fairly attached", "Not very attached", "Not at all attached"))


ggplot(evolution_data, aes(x = year, y = percentage, color = label, group = label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Identity Level") +
  labs(
    title = "Evolution of European Identity (2020–2023)",
    x = "Year",
    y = "Percentage (%)",
    color = "Attachment Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
    legend.position = "right"
  )


# -- 3: correlation among variables  ----

datos <- datos |>
  mutate(across(starts_with("qa5_"), as.factor)) |>
  mutate(across(starts_with("qc4_"), as.factor))

numeric_data <- datos |> select(where(is.numeric))

cor_matrix <- cor(numeric_data, use = "complete.obs")

library(ggcorrplot)
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE,        
           lab_size = 2)      



