#TFM: EUROPEAN IDENTITY
#MAIN ANALYSIS

rm(list = ls())

# ---- Load and prepare data ----

library(haven)
library(dplyr)
datos_imp <- read_dta("datos_imp.dta")
datos_imp$attachment_EU <- as.factor(datos_imp$attachment_EU)

#Delete some variables that are very similar to others
datos_imp <- datos_imp |> select(-c(attachment_Europe, trust_EU, trust_EU_Bank, knowledge_EUworks, future_EU, population, trust_United_nations))

#interests_country, trust_EU_Parliament


# Invert the attachment scales and reduce it to 2 categories
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



# ---- 1. Preliminar analysis: Random Forest (without CV) to check best number of tress----

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

# Save
jpeg("Resultados/General/RF_plot3.jpeg", width = 1200, height = 800, res = 150)
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


# Now that we know that the optimal number of trees is 80-100 approximately, we apply RF and CV

# ---- 2. Random Forest with Cross Validation  ----

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
control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Fine tune mtry
tuneGrid <- data.frame(mtry = c(2, 5, 8, 16, 20, 60))

# Apply model
model_rf_cv <- train(
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
print(model_rf_cv)
plot(model_rf_cv)

# Save
jpeg("Resultados/General/RF_CVplot.jpeg", width = 900, height = 900, res = 200)
plot(model_rf_cv)
dev.off()

png("Resultados/General/RF_CVplot.png", width = 900, height = 900, res = 200)
plot(model_rf_cv)
dev.off()

# Confusion matrix
predictions <- predict(model_rf_cv, newdata = test)
conf_mat <- confusionMatrix(predictions, test$attachment_EU)
print(conf_mat)

library(caret)
library(ggplot2)
library(reshape2)

# Calcular matriz
predictions <- predict(model_rf_cv, newdata = test)
conf_mat <- confusionMatrix(predictions, test$attachment_EU, positive = "1")
conf_mat 

# Save as text
sink("Resultados/General/confusion_matrix_general.txt")
print(conf_mat)
sink()

# Plot 
cm_table <- conf_mat$table
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Reference", "Freq")

total <- sum(cm_df$Freq)
cm_df <- cm_df |>
  mutate(Percentage = Freq / total * 100,
         Label = paste0(Freq, "\n", sprintf("%.1f%%", Percentage)))

# Heatmap
confusion_plot <- ggplot(data = cm_df, aes(x = Reference, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Label), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix",
       x = "Real Value",
       y = "Prediction") +
  theme_minimal(base_size = 15)
confusion_plot
# Save
ggsave(filename = "Resultados/General/confusion_matrix_plot.jpeg", plot = confusion_plot, width = 6,height = 5, dpi = 300,device = "jpeg")

# Plot important variables
importance(model_rf_cv$finalModel)
varImpPlot(model_rf_cv$finalModel)

# Save
jpeg("Resultados/General/RF_importance.jpeg", width = 1200, height = 1200, res = 170) 
varImpPlot(model_rf_cv$finalModel)
dev.off()

png("Resultados/General/RF_importance.png", width = 1200, height = 1200, res = 170)
varImpPlot(model_rf_cv$finalModel)
dev.off()

# Save the plot but with the names in a better way

imp <- importance(model_rf_cv$finalModel)

var_names <- c("Year", "Discuss National Topics", "Discuss European Topics", "Rate General Situation", "Rate EU Economy", "Rate Household", "Rate Employment Country", "EU Issue Crime", "EU Issue Economy", "EU Issue Inflation", "EU Issue Tax", "EU Issue Unemployment", "EU Issue Terrorism", "EU Issue Influence", "EU Issue Immigration", "EU Issue Pensions", "EU Issue Climate", "EU Issue Energy", "EU Issue Health", "Future Country Outlook", "Trust Political Parties", "Trust Justice System", "Trust Police", "Trust Government", "Trust EU Parliament", "Satisfaction Democracy Country", "Satisfaction Democracy EU", "Perception: EU Reflects National Interests", "Attachment to Town", "Attachment to Country", "Importance of EU History", "Importance of EU Religion", "Importance of EU Values", "Importance of EU Geography", "Importance of EU Language", "Importance of EU Law", "Importance of EU Economy", "Importance of EU Culture", "Lack of Political Knowledge", "Education Level", "Gender", "Age", "Town size", "Difficulty Paying Bills", "Social Class", "Political Scale", "Response to COVID", "EU Integration Attitude", "Euro", "Government Satisfaction", "Unemployment Rate", "Gini Index", "GDP", " % Immigration", "Corruption Absence", "Inflation", "Former Communist State", "Polarization Index", "Democracy Level", "Gender Equality", "Corruption Perception", "EU Electoral Participation", "Ratio of Funds to Donors", "Region East", "Region North", "Region South")

imp_df <- data.frame(Variable = var_names, Importance = imp[,1])

imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]

# Plot
imp_top30 <- imp_df |>
  arrange(desc(Importance)) |>
  slice(1:30)

p <- ggplot(imp_top30, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  xlab("") +
  ylab("Variable Importance") +
  ggtitle("Predictors of European Identity (Random Forest)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10))  # mueve el título del eje X hacia abajo
  )

p

ggsave("Resultados/General/Variable_Importance_Plot.png", plot = p, width = 8, height = 6, dpi = 500)
ggsave("Resultados/General/Variable_Importance_Plot.jpeg", plot = p, width = 8, height = 6, dpi = 500)


# Evaluate model
library(pROC)

probs <- predict(model_rf_cv, newdata = test, type = "prob")[, "1"]
roc_obj <- roc(test$attachment_EU, probs)

auc_value <- auc(roc_obj)
print(auc_value)

# Plot 
roc_auc <- plot(roc_obj, main = paste("ROC Curve - AUC:", round(auc_value, 3)))
print (roc_auc)

# Save
jpeg("Resultados/General/ROC_curve.jpeg", width = 800, height = 600, res = 120)
plot(roc_obj, main = paste("ROC Curve - AUC:", round(auc_value, 3)))
dev.off()

png("Resultados/General/ROC_curve.png", width = 800, height = 600, res = 120)
plot(roc_obj, main = paste("ROC Curve - AUC:", round(auc_value, 3)))
dev.off()


# ---- 3. Interpretation: logistic multilevel regression  ----

# Scale data (except dependent var, year, region and categorical variables)
datos_imp_scaled <- datos_imp

numeric_vars <- sapply(datos_imp_scaled, is.numeric)
vars_to_scale <- setdiff(names(datos_imp_scaled)[numeric_vars], c("attachment_EU", "year", "region"))

datos_imp_scaled[vars_to_scale] <- scale(datos_imp_scaled[vars_to_scale])

# Model

library(lme4)

modelo_logistic <- glmer(attachment_EU ~ trust_EU_Parliament + age + interests_country + satisfied_democracy_EU +
                     covid + political_scale + education_level + satisfied_democracy_country + corruption_absence +
                     + social_class + unemployment_rate + rate_EU_economy + immigration +  inflation + gdp +
                     (1 | region) + (1 | year),
                   data = datos_imp_scaled,
                   family = binomial)

summary (modelo_logistic)

sink("Resultados/General/logistic_model.txt")
print(modelo_logistic)
sink()

# Plot changing names

library(broom.mixed)
library(ggplot2)
library(dplyr)

coef_df <- tidy(modelo_logistic, effects = "fixed", conf.int = TRUE)

# More accurate names
name_map <- c(
  trust_EU_Parliament = "Trust EU Parliament",
  age = "Age",
  interests_country = "Perception: EU Reflects National Interests",
  satisfied_democracy_EU = "Satisfaction with EU Democracy",
  covid = "Response to COVID",
  political_scale = "Political Scale",
  education_level = "Education Level",
  satisfied_democracy_country = "Satisfaction with National Democracy",
  corruption_absence = "Corruption Absence",
  social_class = "Social Class",
  unemployment_rate = "Unemployment Rate",
  rate_EU_economy = "Rate EU Economy",
  immigration = "% Immigration",
  inflation = "Inflation",
  gdp = "GDP",
  `(Intercept)` = "Intercept"
)

coef_df$term <- recode(coef_df$term, !!!name_map)

# Significance
coef_df <- coef_df |>
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Plot
logistic <- ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "blue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "blue") +
  geom_text(aes(label = signif), nudge_x = 0.05, size = 3, color = "black") +
  xlab("Coefficient Estimate") + 
  ylab("") +
  ggtitle("Predictors of European Identity (Logistic Mixed Model)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 10)
  )

print(logistic)

# Save
ggsave("Resultados/General/logistic_model_plot.png", plot = logistic, width = 8, height = 6, dpi = 300)
ggsave("Resultados/General/logistic_model_plot.jpeg", plot = logistic, width = 8, height = 6, dpi = 300)


# ---- 4. Evaluating the logistic model  ----

# Random effects
ranef(modelo_logistic)

#R2
library(MuMIn)
r.squaredGLMM(modelo_logistic)

#Multicollineality
library(car)
vif(modelo_logistic) 


# Comparison with a benchmark
modelo_nulo <- glmer(attachment_EU ~ 1 + (1 | region) + (1 | year), data = datos_imp_scaled, family = binomial)
summary (modelo_nulo)
r.squaredGLMM(modelo_nulo)

anova(modelo_nulo, modelo_logistic)


# Residuals
resid_plot <- resid(modelo_logistic, type = "pearson")

# Save
jpeg("Resultados/General/pearson_residuals.jpeg", width = 800, height = 600, res = 150)
hist(resid_plot, main = "Pearson Residuals", xlab = "Residuals", col = "skyblue", border = "white")
dev.off()

png("Resultados/General/pearson_residuals.png", width = 800, height = 600, res = 150)
hist(resid_plot, main = "Pearson Residuals", xlab = "Residuals", col = "skyblue", border = "white")
dev.off()

# Overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(modelo_logistic)

