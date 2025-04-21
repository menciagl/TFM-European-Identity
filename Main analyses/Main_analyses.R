#TFM: EUROPEAN IDENTITY
#MAIN ANALYSIS

rm(list = ls())

library(tidyr)
library(haven)
datos <- read_dta("datos.dta")

#We can directly jump to line 55 of the code where we open the already imputed database
#(since NA imputation takes a long time)

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


# ---- Random Forest ----

# Splitting data in train and test data 

library(haven)
library(dplyr)
datos_imp <- read_dta("datos_imp.dta")
datos_imp$attachment_EU <- as.factor(datos_imp$attachment_EU)

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
                              ntree = 500)

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


# ---- Random Forest with BALANCED CLASSES ----

# Using SMOTE

library(haven)
library(dplyr)
library(randomForest)
library(caTools)
library(caret)
library(smotefamily)

datos_imp <- read_dta("datos_imp.dta")

#Target variable as factor, the rest -> numeric (except country -> dummy)
datos_imp$attachment_EU <- as.factor(datos_imp$attachment_EU)

datos_imp <- datos_imp |>
  mutate(across(-c(attachment_EU, isocntry), ~as.numeric(as.character(.))))

#country -> dummy
isocntry_dummies <- model.matrix(~ isocntry - 1, data = datos_imp)
datos_imp1 <- cbind(datos_imp, isocntry_dummies)
datos_imp1 <- datos_imp1 |>
  select(-isocntry)

#Split data
set.seed(123)
split <- sample.split(datos_imp1$attachment_EU, SplitRatio = 0.7)

train <- subset(datos_imp1, split == TRUE)
test <- subset(datos_imp1, split == FALSE)


#Balance data (SMOTE)
X_train <- train |> select(-attachment_EU)  # delete target variable
y_train <- factor(train$attachment_EU)  # target variable

set.seed(123)  
smote_result <- SMOTE(X = X_train, target = y_train, K = 5, dup_size = 2)

# Combine SMOTE in new dataframe
train_smote <- data.frame(smote_result$data)
names(train_smote)[ncol(train_smote)] <- "attachment_EU"
train_smote$attachment_EU <- as.factor(train_smote$attachment_EU)

table(train_smote$attachment_EU)

# Apply Random Forest
set.seed(123)
classifier_RF <- randomForest(x = train_smote %>% select(-attachment_EU),
                              y = train_smote$attachment_EU,
                              ntree = 500)
print(classifier_RF)

# Predictions
y_pred <- predict(classifier_RF, newdata = test |> select(-attachment_EU))

# Confusion matrix
test$attachment_EU <- factor(test$attachment_EU)
conf_mat <- confusionMatrix(y_pred, test$attachment_EU)
print(conf_mat)

# Importance of the variables
importance(classifier_RF)
varImpPlot(classifier_RF)
