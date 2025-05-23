#TFM: EUROPEAN IDENTITY
#MAIN ANALYSIS

# ---- Load and prepare data ----

library(haven)
library(dplyr)
datos_imp <- read_dta("datos_imp.dta")
datos_imp$attachment_EU <- as.factor(datos_imp$attachment_EU)

#Delete some variables that are very similar to others
datos_imp <- datos_imp |> select(-c(attachment_Europe, trust_EU, trust_EU_Bank, knowledge_EUworks, future_EU, population))

#population, interests_country, satisfied_democracy_EU, trust_EU, future_EU, trust_EU_Parliament

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


# Create new variable: ratio funds vs donations to the EU
datos_imp <- datos_imp |>
  mutate(ratio_funds_donors = funds_millions / donors_millions) |>
  select(-funds_millions, -donors_millions)


#Create regions
datos_imp <- datos_imp |>
  mutate(region = case_when(
    isocntry %in% c("DK", "FI", "SE", "EE", "LV", "LT", "IE") ~ "North", #LETONIA Y LITUANIA?????
    isocntry %in% c("ES", "PT", "IT", "GR", "CY", "MT") ~ "South",
    isocntry %in% c("PL", "CZ", "SK", "HU", "RO", "BG", "HR", "SI") ~ "East",
    isocntry %in% c("AT", "DE", "FR", "BE", "NL", "LU") ~ "Centre",
    TRUE ~ NA_character_  
  ))

datos_imp$region <- as.factor(datos_imp$region)

datos_imp <- datos_imp |> select(-isocntry)

sapply(datos_imp, class)

# ---- Random Forest BY REGION----

library(randomForest)
library(caTools)
library(caret)
library(dplyr)

regiones <- unique(datos_imp$region)
resultados_rf <- list()

set.seed(123)

for (r in regiones) {
  cat("Región:", r, "\n")
  
  datos_region <- datos_imp %>% filter(region == r)
  
  # Split data
  split <- sample.split(datos_region$attachment_EU, SplitRatio = 0.7)
  train <- subset(datos_region, split == TRUE)
  test <- subset(datos_region, split == FALSE)
  
  # Train model
  modelo_rf <- randomForest(
    x = select(train, -attachment_EU),
    y = train$attachment_EU,
    ntree = 400
  )
  
  y_pred <- predict(modelo_rf, newdata = select(test, -attachment_EU))
  conf <- confusionMatrix(y_pred, test$attachment_EU)
  
  resultados_rf[[r]] <- list(
    modelo = modelo_rf,
    importancia = importance(modelo_rf),
    confusion = conf
  )
  
  print(conf)
  cat("\n---------------------------\n")
}


# Variable importance
varImpPlot(resultados_rf[["South"]]$modelo, main = "South")
varImpPlot(resultados_rf[["North"]]$modelo, main = "North")
varImpPlot(resultados_rf[["East"]]$modelo, main = "East")
varImpPlot(resultados_rf[["Centre"]]$modelo, main = "Centre")


# ---- Visualize results in a graph----

library(ggplot2)
library(dplyr)
library(tibble)
library (tidytext)

# Top most important 10 variables
importancia_df <- bind_rows(
  lapply(names(resultados_rf), function(region) {
    importancia <- as.data.frame(resultados_rf[[region]]$importancia)
    importancia$Variable <- rownames(importancia)
    rownames(importancia) <- NULL
    top10 <- importancia |>
      arrange(desc(MeanDecreaseGini)) |>
      slice_head(n = 10) |>
      mutate(Region = region)
    return(top10)
  })
)

# Reorder variables within each region
importancia_df <- importancia_df |>
  mutate(Variable = reorder_within(Variable, MeanDecreaseGini, Region))

# Graph
graph1 <- ggplot(importancia_df, aes(x = Variable, y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~Region, scales = "free_y") +
  scale_x_reordered() +  
  coord_flip() +
  labs(title = "Mos important variables that predict EU attachment (by region)",
       x = "Variable",
       y = "Importance (Mean Decrease Gini)") +
  theme_minimal()

graph1

#Save
ggsave("Resultados/subanalysis_region.jpeg", plot = graph1, width = 10, height = 8, dpi = 300)





