#TFM: EUROPEAN IDENTITY
#MAIN ANALYSIS

rm(list = ls())

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
    isocntry %in% c("DK", "FI", "SE", "EE", "LV", "LT", "IE") ~ "North",
    isocntry %in% c("ES", "PT", "IT", "GR", "CY", "MT") ~ "South",
    isocntry %in% c("PL", "CZ", "SK", "HU", "RO", "BG", "HR", "SI") ~ "East",
    isocntry %in% c("AT", "DE", "FR", "BE", "NL", "LU") ~ "Centre",
    TRUE ~ NA_character_  
  ))

datos_imp$region <- as.factor(datos_imp$region)

datos_imp <- datos_imp |> select(-isocntry)

sapply(datos_imp, class)

# ---- 1. Random Forest (without Cross Validation) ----

#Split data
library(randomForest)
library (caTools)

set.seed(123)
split <- sample.split(datos_imp$attachment_EU, SplitRatio = 0.7)

train <- subset(datos_imp, split == TRUE)
test <- subset(datos_imp, split == FALSE)


# Train RF model
set.seed(120)
classifier_RF <- randomForest(x = select(train, -attachment_EU),
                              y = train$attachment_EU,
                              ntree = 400)

classifier_RF

# Plot the model
plot(classifier_RF)

#save
jpeg("Resultados/RF_plot3.jpeg") 
plot(classifier_RF)
dev.off()

# Confusion matrix
y_pred <- predict(classifier_RF, newdata = test[, !(names(test) == "attachment_EU")])

library(caret)
conf_mat <- confusionMatrix(y_pred, test$attachment_EU)
print(conf_mat)

# Importance of the variables
importance(classifier_RF)
varImpPlot(classifier_RF)

#Save
jpeg("Resultados/RF_plot2.jpeg") 
varImpPlot(classifier_RF)
dev.off()


# ---- 2. Interpretation: logistic multilevel regression  ----

#Scale data (except dependent var, year, region and categorical variables)
datos_imp_scaled <- datos_imp

numeric_vars <- sapply(datos_imp_scaled, is.numeric)
vars_to_scale <- setdiff(names(datos_imp_scaled)[numeric_vars], c("attachment_EU", "year", "region"))

datos_imp_scaled[vars_to_scale] <- scale(datos_imp_scaled[vars_to_scale])

# Model

library(lme4)

modelo <- glmer(attachment_EU ~ age + interests_country + trust_EU_Parliament + satisfied_democracy_EU +
                  covid + political_scale + education_level + social_class + corruption_absence + unemployment_rate+
                  inflation + gdp + immigration + rural_urban + gini_index + (1 | region) + (1 | year),
                data = datos_imp_scaled,
                family = binomial)

summary (modelo)

#R2
library(MuMIn)
r.squaredGLMM(modelo)

#Check multicollinearity

library(car)
vif(modelo)

library(performance)
check_collinearity(modelo)


## THIS IS THE SAME AS BEFORE JUST USING CROSS VALIDATION:

# ---- 3. Random Forest with Cross Validation  ----

library(randomForest)
library(caret)
library(doParallel)
library (caTools)

# Parallelize
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Split and train
set.seed(123)

datos_imp1 <- na.omit(datos_imp)
split <- sample.split(datos_imp1$attachment_EU, SplitRatio = 0.7)
train <- subset(datos_imp1, split == TRUE)
test <- subset(datos_imp1, split == FALSE)

# Cross validation : 5 fold
control <- trainControl(method = "cv", number = 2, allowParallel = TRUE)

# Fine tune mtry
tuneGrid = data.frame(mtry = c(5, 8, 10)) 

# Apply model
model_rf_cv <- train(
  attachment_EU ~ ., 
  data = train,
  method = "rf",
  trControl = control,
  ntree = 100
)

stopCluster(cl)
print(model_rf_cv)


# Confusion matrix
predictions <- predict(model_rf_cv, newdata = test)
conf_mat <- confusionMatrix(predictions, test$attachment_EU)
print(conf_mat)


# Plot important variables
importance(model_rf_cv$finalModel)
varImpPlot(model_rf_cv$finalModel)


# ---- 4. Interpretation: logistic multilevel regression  ----

library(lme4)

modelo_cv <- glmer(attachment_EU ~ interests_country + trust_EU_Parliament + satisfied_democracy_EU +
                     covid + age + political_scale + education_level + satisfied_democracy_country + corruption_absence +
                     + gdp + rate_EU_economy + unemployment_rate+ immigration +  gini_index + inflation + democracy + ratio_funds_donors+
                     (1 | region) + (1 | year),
                   data = datos_imp_scaled,
                   family = binomial)

summary (modelo_cv)

#R2
library(MuMIn)
r.squaredGLMM(modelo)

#Check multicollinearity

library(car)
vif(modelo_cv)

