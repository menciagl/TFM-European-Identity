#TFM: EUROPEAN IDENTITY
#YEAR

rm(list = ls())

# ---- Load and prepare data ----

library(haven)
library(dplyr)
datos_imp <- read_dta("datos_imp.dta")
datos_imp$attachment_EU <- as.factor(datos_imp$attachment_EU)



#Delete some variables that are very similar to others
datos_imp <- datos_imp |> select(-c(attachment_Europe, trust_EU, trust_EU_Bank, knowledge_EUworks, future_EU, population, trust_United_nations))


# Invert the attachment scale and reduce it to 2 categories
datos_imp$attachment_town <- recode(datos_imp$attachment_town, `4` = 1, `3` = 2, `2` = 3, `1` = 4)
datos_imp$attachment_town <- ifelse(as.numeric(as.character(datos_imp$attachment_town)) %in% c(1, 2), 0, 1)
datos_imp$attachment_town <- as.factor(datos_imp$attachment_town)


datos_imp$attachment_country <- recode(datos_imp$attachment_country, `4` = 1, `3` = 2, `2` = 3, `1` = 4)
datos_imp$attachment_country <- ifelse(as.numeric(as.character(datos_imp$attachment_country)) %in% c(1, 2), 0, 1)
datos_imp$attachment_country <- as.factor(datos_imp$attachment_country)

datos_imp$attachment_EU <- recode(datos_imp$attachment_EU, `4` = 1, `3` = 2, `2` = 3, `1` = 4)
datos_imp$attachment_EU <- ifelse(as.numeric(as.character(datos_imp$attachment_EU)) %in% c(1, 2), 0, 1)
datos_imp$attachment_EU <- as.factor(datos_imp$attachment_EU)
table(datos_imp$attachment_EU)

# Invert other variables scales
datos_imp <- datos_imp |>
  mutate(
    satisfied_democracy_country = recode(satisfied_democracy_country, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    satisfied_democracy_EU = recode(satisfied_democracy_EU, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    rate_household = recode(rate_household, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    rate_EU_economy = recode(rate_EU_economy, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    discuss_european_topics = recode(discuss_european_topics, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    discuss_national_topics = recode(discuss_national_topics, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    difficulty_bills = recode(difficulty_bills, `1` = 3, `2` = 2, `3` = 1),
    covid = recode(covid, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    interests_country = recode(interests_country, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
  )

datos_imp <- datos_imp |>
  mutate(trust_EU_Parliament = recode(trust_EU_Parliament, `2` = 0))



# Create new variable: ratio funds vs donations to the EU
datos_imp <- datos_imp |>
  mutate(ratio_funds_donors = funds_millions / donors_millions) |>
  select(-funds_millions, -donors_millions)

#Create regions
datos_imp <- datos_imp |>
  mutate(region = case_when(
    isocntry %in% c("DK", "FI", "SE", "IE") ~ "North",
    isocntry %in% c("ES", "PT", "IT", "GR", "CY", "MT") ~ "South",
    isocntry %in% c("PL", "CZ", "SK", "HU", "RO", "BG", "HR", "SI", "EE", "LV", "LT") ~ "East",
    isocntry %in% c("AT", "DE", "FR", "BE", "NL", "LU") ~ "Center",
    TRUE ~ NA_character_  
  ))

datos_imp$region <- as.factor(datos_imp$region)
datos_imp <- datos_imp |> select(-isocntry)


# Check variables
datos_imp <- datos_imp |> mutate(year = as.factor(year))

sapply(datos_imp, class)

# ---- Random Forest and Cross Validation BY YEAR----

library(randomForest)
library(caret)
library(doParallel)
library(dplyr)
library(caTools)
library(tidytext)
library(ggplot2)
library(tibble)

# Define years
years <- unique(datos_imp$year)

# Control: CV & grid
control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
tuneGrid <- expand.grid(mtry = c(2, 5, 8, 16, 20, 60))


resultados_rf_cv <- list()

# Loop by year
for (r in years) {
  cat("Year:", r, "\n")
  
  datos_year <- datos_imp |> filter(year == r) |> na.omit()
  
  set.seed(123)
  split <- sample.split(datos_year$attachment_EU, SplitRatio = 0.7)
  train <- subset(datos_year, split == TRUE)
  test <- subset(datos_year, split == FALSE)
  
  # Train model
  cl <- makePSOCKcluster(detectCores() - 1)
  registerDoParallel(cl)
  
  modelo_rf_cv <- train(
    attachment_EU ~ .,
    data = train,
    method = "rf",
    trControl = control,
    tuneGrid = tuneGrid,
    ntree = 100
  )
  
  stopCluster(cl)
  stopImplicitCluster()
  
  predicciones <- predict(modelo_rf_cv, newdata = test)
  conf <- confusionMatrix(predicciones, test$attachment_EU, positive = "1")
  
  resultados_rf_cv[[as.character(r)]] <- list(
    modelo = modelo_rf_cv,
    confusion = conf,
    importancia = varImp(modelo_rf_cv)
  )
  
  print(conf)
  cat("--------------------------\n")
}


# Names
nombre_variables <- c(
  "trust_EU_Parliament" = "Trust EU Parliament",
  "age" = "Age",
  "interests_country" = "Perception: EU Reflects National Interests",
  "satisfied_democracy_EU" = "Satisfaction with EU Democracy",
  "covid" = "Response to COVID",
  "political_scale" = "Political Scale",
  "education_level" = "Education Level",
  "satisfied_democracy_country" = "Satisfaction with National Democracy",
  "corruption_absence" = "Corruption Absence",
  "social_class" = "Social Class",
  "unemployment_rate" = "Unemployment Rate",
  "rate_EU_economy" = "Rate EU Economy",
  "immigration" = "% Immigration",
  "inflation" = "Inflation",
  "gdp" = "GDP",
  "difficulty_bills" = "Difficulty Paying Bills",
  "attachment_country1" = "Attachment to Country",
  "discuss_european_topics" = "Discuss European Topics",
  "rural_urban" = "Town size"
)

importancia_df_year <- bind_rows(
  lapply(names(resultados_rf_cv), function(year) {
    importancia <- as.data.frame(resultados_rf_cv[[year]]$importancia$importance)
    importancia$Variable <- rownames(importancia)
    rownames(importancia) <- NULL
    
    top10 <- importancia |>
      arrange(desc(Overall)) |>
      slice_head(n = 10) |>
      mutate(Year = year)
    
    return(top10)
  })
)

# Labels
importancia_df_year$Variable <- ifelse(
  importancia_df_year$Variable %in% names(nombre_variables),
  nombre_variables[importancia_df_year$Variable],
  importancia_df_year$Variable
)

importancia_df_year <- importancia_df_year |>
  mutate(Variable = reorder_within(Variable, Overall, Year))

#Plot

library(tidytext)

graph_year <- ggplot(importancia_df_year, aes(
  x = reorder_within(Variable, Overall, Year),
  y = Overall,
  fill = Overall
)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Year, scales = "free_y") +
  coord_flip() +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  scale_x_reordered() +
  labs(
    title = "Predictors of European Identity (by Year)",
    x = "",
    y = "Importance (Mean Decrease Accuracy)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 10)),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(graph_year)

# Save
ggsave("Resultados/Byyear/Variable_Importance_ByYear.jpeg", plot = graph_year, width = 13, height = 8, dpi = 300)



# ---- Evaluate the model----

library(pROC)

for (r in names(resultados_rf_cv)) {
  cat("ROC - Year:", r, "\n")
  
  # Filter
  datos_year <- datos_imp |> filter(year == r) |> na.omit()
  set.seed(123)
  split <- sample.split(datos_year$attachment_EU, SplitRatio = 0.7)
  test <- subset(datos_year, split == FALSE)
  
  # Probabilities
  probs <- predict(resultados_rf_cv[[r]]$modelo, newdata = test, type = "prob")[, "1"]
  
  # ROC & AUC
  roc_obj <- roc(test$attachment_EU, probs)
  auc_value <- auc(roc_obj)
  cat("AUC:", round(auc_value, 3), "\n")
  
  plot_title <- paste0("ROC Curve - ", r, " (AUC: ", round(auc_value, 3), ")")
  
  # Show results
  dev.new()  
  plot(roc_obj, main = plot_title, col = "black", lwd = 2)
  
  # JPEG
  jpeg(paste0("Resultados/Byyear/ROC/ROC_curve_", r, ".jpeg"), width = 800, height = 600, res = 120)
  plot(roc_obj, main = plot_title, col = "black", lwd = 2)
  dev.off()
 
}



# ---- Interpretation 2020 ----

# Scale data (except dependent var, year, region and categorical variables)
datos_imp_scaled <- datos_imp

numeric_vars <- sapply(datos_imp_scaled, is.numeric)
vars_to_scale <- setdiff(names(datos_imp_scaled)[numeric_vars], c("attachment_EU", "year", "region"))

datos_imp_scaled[vars_to_scale] <- scale(datos_imp_scaled[vars_to_scale])


# 2020

library(lme4)
library(broom.mixed)
library(ggplot2)
library(dplyr)

datos_2020 <- datos_imp_scaled |> filter(year == 2020)

modelo_logistic_2020 <- glmer(
  attachment_EU ~ trust_EU_Parliament + age + interests_country + satisfied_democracy_EU +
    covid + political_scale + education_level + rate_EU_economy + social_class +
    satisfied_democracy_country + (1 | region),
  data = datos_2020,
  family = binomial
)

summary(modelo_logistic_2020)

sink("Resultados/Byyear/Logistic/logistic_model_2020.txt")
print(summary(modelo_logistic_2020))
sink()

# Plot
names <- c(
  trust_EU_Parliament = "Trust EU Parliament",
  age = "Age",
  interests_country = "Perception: EU Reflects National Interests",
  satisfied_democracy_EU = "Satisfaction with EU Democracy",
  covid = "Response to COVID",
  political_scale = "Political Scale",
  education_level = "Education Level",
  satisfied_democracy_country = "Satisfaction with National Democracy",
  social_class = "Social Class",
  rate_EU_economy = "Rate EU economy",
  rural_urban = "Town Size",
  discuss_european_topics = "Discuss European Topics",
  `(Intercept)` = "Intercept"
)

coef_df_2020 <- tidy(modelo_logistic_2020, conf.int = TRUE) |> 
  filter(!grepl("^sd__", term)) 

coef_df_2020$term <- recode(coef_df_2020$term, !!!names)

coef_df_2020 <- coef_df_2020 |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Plot
logistic_plot_2020 <- ggplot(coef_df_2020, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "blue") +
  geom_text(aes(label = signif), nudge_x = 0.16, size = 4, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.3)+
  xlab("Coefficient Estimate") + 
  ylab("") +
  ggtitle("2020") +
  theme_minimal() +
  theme(plot.title = element_text(size = 26),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.text.y = element_text(size = 21),
        axis.text.x = element_text(size = 18)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_plot_2020)

# Save
ggsave("Resultados/Byyear/Logistic/logistic_2020.jpeg", plot = logistic_plot_2020, width = 8, height = 6, dpi = 300)


# ---- Interpretation 2021 ----

datos_2021 <- datos_imp_scaled |> filter(year == 2021)

modelo_logistic_2021 <- glmer(
  attachment_EU ~ trust_EU_Parliament + age + interests_country + satisfied_democracy_EU +
    covid + political_scale + education_level + discuss_european_topics + social_class +
    rural_urban + (1 | region),
  data = datos_2021,
  family = binomial
)

summary(modelo_logistic_2021)

sink("Resultados/Byyear/Logistic/logistic_model_2021.txt")
print(summary(modelo_logistic_2021))
sink()

# Plot
coef_df_2021 <- tidy(modelo_logistic_2021, conf.int = TRUE) |> 
  filter(!grepl("^sd__", term)) 

coef_df_2021$term <- recode(coef_df_2021$term, !!!names)

coef_df_2021 <- coef_df_2021 |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Plot
logistic_plot_2021 <- ggplot(coef_df_2021, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkred") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkred") +
  geom_text(aes(label = signif), nudge_x = 0.16, size = 4, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.3)+
  xlab("Coefficient Estimate") + 
  ylab("") +
  ggtitle("2021") +
  theme_minimal() +
  theme(plot.title = element_text(size = 26),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.text.y = element_text(size = 21),
        axis.text.x = element_text(size = 18)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_plot_2021)

# Save
ggsave("Resultados/Byyear/Logistic/logistic_2021.jpeg", plot = logistic_plot_2021, width = 8, height = 6, dpi = 300)


# ---- Interpretation 2022 ----

datos_2022 <- datos_imp_scaled |> filter(year == 2022)

modelo_logistic_2022 <- glmer(
  attachment_EU ~ trust_EU_Parliament + age + interests_country + satisfied_democracy_EU +
    covid + political_scale + education_level + rate_EU_economy +
    satisfied_democracy_country + social_class + (1 | region),
  data = datos_2022,
  family = binomial
)

summary(modelo_logistic_2022)

sink("Resultados/Byyear/Logistic/logistic_model_2022.txt")
print(summary(modelo_logistic_2022))
sink()

# Plot
coef_df_2022 <- tidy(modelo_logistic_2022, conf.int = TRUE) |> 
  filter(!grepl("^sd__", term)) 

coef_df_2022$term <- recode(coef_df_2022$term, !!!names)

coef_df_2022 <- coef_df_2022 |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Plot
logistic_plot_2022 <- ggplot(coef_df_2022, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkgreen") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkgreen") +
  geom_text(aes(label = signif), nudge_x = 0.16, size = 4, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.3)+
  xlab("Coefficient Estimate") + 
  ylab("") +
  ggtitle("2022") +
  theme_minimal() +
  theme(plot.title = element_text(size = 26),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.text.y = element_text(size = 21),
        axis.text.x = element_text(size = 18)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_plot_2022)

# Save
ggsave("Resultados/Byyear/Logistic/logistic_2022.jpeg", plot = logistic_plot_2022, width = 8, height = 6, dpi = 300)



# ---- Interpretation 2023 ----

datos_2023 <- datos_imp_scaled |> filter(year == 2023)

modelo_logistic_2023 <- glmer(
  attachment_EU ~ trust_EU_Parliament + age + interests_country + satisfied_democracy_EU +
    covid + political_scale + education_level + rate_EU_economy + social_class +
    satisfied_democracy_country + (1 | region),
  data = datos_2023,
  family = binomial
)

summary(modelo_logistic_2023)

sink("Resultados/Byyear/Logistic/logistic_model_2023.txt")
print(summary(modelo_logistic_2023))
sink()

# Plot
coef_df_2023 <- tidy(modelo_logistic_2023, conf.int = TRUE) |> 
  filter(!grepl("^sd__", term)) 

coef_df_2023$term <- recode(coef_df_2023$term, !!!names)

coef_df_2023 <- coef_df_2023 |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Plot
logistic_plot_2023 <- ggplot(coef_df_2023, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "orange") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "orange") +
  geom_text(aes(label = signif), nudge_x = 0.16, size = 4, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.3)+
  xlab("Coefficient Estimate") + 
  ylab("") +
  ggtitle("2023") +
  theme_minimal() +
  theme(plot.title = element_text(size = 26),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.text.y = element_text(size = 21),
        axis.text.x = element_text(size = 18)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_plot_2023)

# Save
ggsave("Resultados/Byyear/Logistic/logistic_2023.jpeg", plot = logistic_plot_2023, width = 8, height = 6, dpi = 300)


# ---- Combine plots ----

library(patchwork)
combined_plot <- (logistic_plot_2020 | logistic_plot_2021) /
  (logistic_plot_2022 | logistic_plot_2023)
print(combined_plot)

ggsave("Resultados/Byyear/Logistic/logistic_combined.jpeg", plot = combined_plot, width = 20, height = 12, dpi = 300)

