#TFM: EUROPEAN IDENTITY
#REGION

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

sapply(datos_imp, class)


# ---- Random Forest and Cross Validation BY REGION----

library(randomForest)
library(caret)
library(doParallel)
library(dplyr)
library(caTools)

# Define regions
regiones <- unique(datos_imp$region)

# Control: CV & grid
control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
tuneGrid <- expand.grid(mtry = c(2, 5, 8, 16, 20, 60))


resultados_rf_cv <- list()

# Loop by region
for (r in regiones) {
  cat("Región:", r, "\n")

  
  datos_region <- datos_imp |> filter(region == r) |> na.omit()
  
  # Split train/test
  set.seed(123)
  split <- sample.split(datos_region$attachment_EU, SplitRatio = 0.7)
  train <- subset(datos_region, split == TRUE)
  test <- subset(datos_region, split == FALSE)
  
  # Parallelize
  cl <- makePSOCKcluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Train the model
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
  
  # Evaluation
  predicciones <- predict(modelo_rf_cv, newdata = test)
  conf <- confusionMatrix(predicciones, test$attachment_EU, positive = "1")
  
  # Save
  resultados_rf_cv[[r]] <- list(
    modelo = modelo_rf_cv,
    confusion = conf,
    importancia = varImp(modelo_rf_cv)
  )
  
  # Show results
  print(conf)
  cat("--------------------------\n")
}


# Plot
library(ggplot2)
library(dplyr)
library(tibble)
library(tidytext)

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
  "attachment_country1" = "Attachment to country",
  "discuss_european_topics" = "Discuss European Topics"
)

# Top 10 variables
importancia_df <- bind_rows(
  lapply(names(resultados_rf_cv), function(region) {
    importancia <- as.data.frame(resultados_rf_cv[[region]]$importancia$importance)
    importancia$Variable <- rownames(importancia)
    rownames(importancia) <- NULL
    top10 <- importancia |>
      arrange(desc(Overall)) |>
      slice_head(n = 10) |>
      mutate(Region = region)
    return(top10)
  })
)


# Plot
importancia_df$Variable <- ifelse(
  importancia_df$Variable %in% names(nombre_variables),
  nombre_variables[importancia_df$Variable],
  importancia_df$Variable
)


importancia_df <- importancia_df |>
  mutate(Variable = reorder_within(Variable, Overall, Region))

graph1 <- ggplot(importancia_df, aes(x = Variable, y = Overall)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~Region, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Predictors of European Identity (by Region)",
    x = "",
    y = "Importance (Mean Decrease Accuracy)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold")  
  )

print(graph1)



# Save
ggsave("Resultados/Byregion/Variable_Importance_ByRegion.jpeg", plot = graph1, width = 10, height = 8, dpi = 300)
ggsave("Resultados/Byregion/Variable_Importance_ByRegion.png", plot = graph1, width = 10, height = 8, dpi = 300)


# ---- Evaluate the model----
library(pROC)

for (r in names(resultados_rf_cv)) {
  cat("ROC - Region:", r, "\n")
  

  datos_region <- datos_imp %>% filter(region == r) %>% na.omit()
  set.seed(123)
  split <- sample.split(datos_region$attachment_EU, SplitRatio = 0.7)
  test <- subset(datos_region, split == FALSE)
  
  # Probabilities
  probs <- predict(resultados_rf_cv[[r]]$modelo, newdata = test, type = "prob")[, "1"]
  
  # ROC y AUC
  roc_obj <- roc(test$attachment_EU, probs)
  auc_value <- auc(roc_obj)
  cat("AUC:", round(auc_value, 3), "\n")
  
  plot_title <- paste0("ROC Curve - ", r, " (AUC: ", round(auc_value, 3), ")")
  
  # Show
  dev.new()  
  plot(roc_obj, main = plot_title, col = "black", lwd = 2)
  
  # JPEG 
  jpeg(paste0("Resultados/Byregion/ROC/ROC_curve_", r, ".jpeg"), width = 800, height = 600, res = 120)
  plot(roc_obj, main = plot_title, col = "black", lwd = 2)
  dev.off()
  
  # PNG
  png(paste0("Resultados/Byregion/ROC/ROC_curve_", r, ".png"), width = 800, height = 600, res = 120)
  plot(roc_obj, main = plot_title, col = "black", lwd = 2)
  dev.off()
}

# ---- Interpretation Center ----

# Scale data (except dependent var, year, region and categorical variables)
datos_imp_scaled <- datos_imp

numeric_vars <- sapply(datos_imp_scaled, is.numeric)
vars_to_scale <- setdiff(names(datos_imp_scaled)[numeric_vars], c("attachment_EU", "year", "region"))

datos_imp_scaled[vars_to_scale] <- scale(datos_imp_scaled[vars_to_scale])


library(lme4)

# Center
datos_center <- datos_imp_scaled |> filter(region == "Center")

# Logistic model
modelo_logistic_center <- glmer(
  attachment_EU ~ interests_country + age + trust_EU_Parliament + satisfied_democracy_EU +
    political_scale + education_level + covid + satisfied_democracy_country + social_class +
    attachment_country + (1 | year),
  data = datos_center,
  family = binomial
)

summary(modelo_logistic_center)

# Save
sink("Resultados/byregion/Logistic/logistic_model_center.txt")
print(modelo_logistic_center)
sink()


#Plot
library(broom.mixed)
library(ggplot2)
library(dplyr)

coef_df <- tidy(modelo_logistic_center, effects = "fixed", conf.int = TRUE)

# Names
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
  attachment_country1 = "Attachment to Country",
  rate_EU_economy = "Rate EU economy",
  discuss_european_topics = "Discuss European Topics",
  difficulty_bills = "Difficulty Paying Bills",
  `(Intercept)` = "Intercept"
)

coef_df$term <- recode(coef_df$term, !!!names)


coef_df <- coef_df |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Plot
logistic_center <- ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "blue") +
  geom_text(aes(label = signif), nudge_x = 0.12, size = 4, color = "black") +
  xlab("Coefficient Estimate") + 
  ylab("") +
  ggtitle("Center") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(size = 14)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_center)

# Save
ggsave("Resultados/Byregion/Logistic/logistic_center.png", plot = logistic_center, width = 8, height = 6, dpi = 300)
ggsave("Resultados/Byregion/Logistic/logistic_center.jpeg", plot = logistic_center, width = 8, height = 6, dpi = 300)



# ---- Interpretation East ----

datos_east <- datos_imp_scaled |> filter(region == "East")

modelo_logistic_east <- glmer(
  attachment_EU ~  age + interests_country + trust_EU_Parliament + satisfied_democracy_EU +
    covid + political_scale + education_level + satisfied_democracy_country + rate_EU_economy +
    social_class + (1 | year),
  data = datos_east,
  family = binomial
)

summary(modelo_logistic_east)


sink("Resultados/Byregion/Logistic/logistic_model_east.txt")
print(summary(modelo_logistic_east))
sink()

# Plot

coef_df_east <- tidy(modelo_logistic_east, effects = "fixed", conf.int = TRUE)


coef_df_east$term <- recode(coef_df_east$term, !!!names)

coef_df_east <- coef_df_east |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

logistic_east <- ggplot(coef_df_east, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkred") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkred") +
  geom_text(aes(label = signif), nudge_x = 0.12, size = 4, color = "black") +
  xlab("Coefficient Estimate") +
  ylab("") +
  ggtitle("East") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(size = 12)) + coord_cartesian(xlim = c(-0.6, 1.3))


print(logistic_east)

ggsave("Resultados/Byregion/Logistic/logistic_east.jpeg", plot = logistic_east, width = 8, height = 6, dpi = 300)
ggsave("Resultados/Byregion/Logistic/logistic_east.png", plot = logistic_east, width = 8, height = 6, dpi = 300)


# ---- Interpretation North ----

datos_north <- datos_imp_scaled |> filter(region == "North")

modelo_logistic_north <- glmer(
  attachment_EU ~ age + interests_country + satisfied_democracy_EU + trust_EU_Parliament + 
    political_scale + covid + education_level + satisfied_democracy_country + social_class +
    discuss_european_topics + (1 | year),
  data = datos_north,
  family = binomial
)

summary(modelo_logistic_north)

sink("Resultados/Byregion/Logistic/logistic_model_north.txt")
print(summary(modelo_logistic_north))
sink()

# Plot

coef_df_north <- tidy(modelo_logistic_north, effects = "fixed", conf.int = TRUE)


coef_df_north$term <- recode(coef_df_north$term, !!!names)

coef_df_north <- coef_df_north |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

logistic_north <- ggplot(coef_df_north, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkgreen") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkgreen") +
  geom_text(aes(label = signif), nudge_x = 0.11, size = 4, color = "black") +
  xlab("Coefficient Estimate") +
  ylab("") +
  ggtitle("North") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(size = 12)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_north)

# Save
ggsave("Resultados/Byregion/Logistic/logistic_north.jpeg", plot = logistic_north, width = 8, height = 6, dpi = 300)
ggsave("Resultados/Byregion/Logistic/logistic_north.png", plot = logistic_north, width = 8, height = 6, dpi = 300)


# ---- Interpretation South ----

datos_south <- datos_imp_scaled |>filter(region == "South")


modelo_logistic_south <- glmer(
  attachment_EU ~ age + trust_EU_Parliament + satisfied_democracy_EU +  interests_country+
    education_level + political_scale + covid + satisfied_democracy_country + difficulty_bills +
      social_class + (1 | year),
  data = datos_south,
  family = binomial
)


summary(modelo_logistic_south)

sink("Resultados/Byregion/Logistic/logistic_model_south.txt")
print(summary(modelo_logistic_south))
sink()

# Plot
coef_df_south <- tidy(modelo_logistic_south, effects = "fixed", conf.int = TRUE)


coef_df_south$term <- recode(coef_df_south$term, !!!names)

coef_df_south <- coef_df_south |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))


logistic_south <- ggplot(coef_df_south, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkorange") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkorange") +
  geom_text(aes(label = signif), nudge_x = 0.11, size = 4, color = "black") +
  xlab("Coefficient Estimate") +
  ylab("") +
  ggtitle("South") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(size = 12)) + coord_cartesian(xlim = c(-0.6, 1.3))

print(logistic_south)

ggsave("Resultados/Byregion/Logistic/logistic_south.jpeg", plot = logistic_south, width = 8, height = 6, dpi = 300)
ggsave("Resultados/Byregion/Logistic/logistic_south.png", plot = logistic_south, width = 8, height = 6, dpi = 300)


# ---- Combine plots ----

library(patchwork)
combined_plot <- (logistic_center | logistic_east) /
  (logistic_north | logistic_south)
print(combined_plot)

ggsave("Resultados/Byregion/Logistic/logistic_combined.jpeg", plot = combined_plot, width = 16, height = 12, dpi = 300)



# ---- Important plots (to see differences between regions)----

# South
# Prediction of marginal probabilities according to difficulty_bills

library(ggeffects)
gg.pred1 <- ggpredict(
  modelo_logistic_south,
  terms = c("difficulty_bills"),  
  type = "fixed",                 
  bias_correction = TRUE          
)

plot_pred <- plot(gg.pred1) +
  labs(
    title = "Probability of European Identity (South)",
    subtitle = "According to Difficulty Paying Bills",
    x = "Difficulty Paying Bills (Scaled)",
    y = "Predicted Probability of Attachment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray25"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

print(plot_pred)

ggsave("Resultados/Byregion/Logistic/Predicted_Attachment_By_Bills_SocialClass.jpeg",
       plot = plot_pred, width = 9, height = 6, dpi = 300)


# Center
# Prediction of marginal probabilities according to attachment to country
gg.pred_center <- ggpredict(
  modelo_logistic_center,
  terms = c("attachment_country"),
  type = "fixed",
  bias_correction = TRUE
)


plot_center <- plot(gg.pred_center) +
  labs(
    title = "Probability of European Attachment (Center)",
    subtitle = "According to Attachment to Country",
    x = "Attachment to Country",
    y = "Predicted Probability of Attachment to the EU"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray25"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

print(plot_center)

ggsave("Resultados/Byregion/Logistic/Predicted_Attachment_By_AttachmentCountry.jpeg",
       plot = plot_center, width = 9, height = 6, dpi = 300)

# North
# Prediction of marginal probabilities according to discussion of european topics

gg.pred_north <- ggpredict(
  modelo_logistic_north,
  terms = c("discuss_european_topics"),
  type = "fixed",
  bias_correction = TRUE
)

plot_north <- plot(gg.pred_north) +
  labs(
    title = "Probability of European Attachment (North)",
    subtitle = "According to Frequency of Discussing European Topics",
    x = "Discussing European Topics (Scaled)",
    y = "Predicted Probability of Attachment to the EU"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray25"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

print(plot_north)

ggsave("Resultados/Byregion/Logistic/Predicted_Attachment_By_EUtopics.jpeg",
       plot = plot_north, width = 9, height = 6, dpi = 300)



# See differences between other variables: age and political scale

# AGE
coef_age <- coef_filtered %>% filter(term == "Age")

jpeg("Resultados/Byregion/Logistic/age_effect.jpeg", width = 1600, height = 1200, res = 300)

ggplot(coef_age, aes(y = region, x = estimate)) +
  geom_point(color = "black", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  labs(
    title = "Effect of Age on European Identity (by region)",
    x = "Coefficient Estimate",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
dev.off()

# POLITICAL SCALE
coef_pol <- coef_filtered %>% filter(term == "Political Scale")

jpeg("Resultados/Byregion/Logistic/political_scale_effect.jpeg", width = 1600, height = 1200, res = 300)

ggplot(coef_pol, aes(y = region, x = estimate)) +
  geom_point(color = "black", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  labs(
    title = "Effect of Political Orientation on European Identity (by region)",
    x = "Coefficient Estimate",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
dev.off()

