#TFM: EUROPEAN IDENTITY
#DESCRIPTIVE ANALYSIS

rm(list = ls())

library(tidyr)
library(haven)
datos <- read_dta("datos.dta")

# ---- Descriptive analysis ----

# Inverse scale for "attachment to the EU"
datos <- datos |>
  mutate(attachment_EU = recode(as.character(attachment_EU),
                                "1" = "4",
                                "2" = "3",
                                "3" = "2",
                                "4" = "1")) |> 
  mutate(attachment_EU = as.numeric(attachment_EU))

# -- 1: EU attachment ----
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)

etiquetas_attachment <- c(
  "1" = "Not at all",
  "2" = "Not very",
  "3" = "Fairly",
  "4" = "Very"
)

table <- table (datos$attachment_EU)
prop.table(table)


attachment_porcentaje <- datos |>
  filter(!is.na(attachment_EU)) |>
  mutate(attachment_EU = recode(as.character(attachment_EU), !!!etiquetas_attachment)) |>
  group_by(attachment_EU) |>
  summarise(count = n()) |>
  mutate(porcentaje = count / sum(count) * 100) |>
  mutate(attachment_EU = fct_relevel(attachment_EU, "Not at all", "Not very", "Fairly", "Very"))

ggplot(attachment_porcentaje, aes(x = attachment_EU, y = porcentaje, fill = attachment_EU)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c( "#D32F2F", "#E57373","#81C784", "#388E3C")) +
  labs(
    title = "Attachment to the European Union",
    subtitle = "Distribution of responses (%)",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10), color = "gray25"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12,margin = margin(r = 12)),
    panel.grid.major.x = element_blank()
  )

ggsave("Resultados/Descriptive_plot1.jpeg", plot = last_plot(), width = 7, height = 5, dpi = 300)
ggsave("Resultados/Descriptive_plot1.png", plot = last_plot(), width = 7, height = 5, dpi = 300)

# -- 2: Differences between countries: how many people are attached to the EU ----

# Group by country and see % of people having more EU identity
attachment_percentages <- datos |>
  filter(attachment_EU %in% c(3, 4)) |>
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

# Mapa base
library(ggplot2)
library(mapdata)
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
    name = "Percentage (%)",
    na.value = "lightgray"
  ) +
  labs(
    title = "Attachment to the European Union",
    subtitle = "Percentage of respondents feeling 'fairly' and 'very' attached to the EU"
  ) +
  coord_quickmap() +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, color = "gray25"),
    legend.title = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("Resultados/Descriptive_plot2_map.jpeg", plot = last_plot(), width = 7, height = 5, dpi = 300)
ggsave("Resultados/Descriptive_plot2_map.png", plot = last_plot(), width = 7, height = 5, dpi = 300)


# -- 3: Differences over time in attachment to the European Union (2020-23)  ----

evolution_data <- datos |>
  filter(year %in% 2020:2023, attachment_EU %in% 1:4) |>
  group_by(year, attachment_EU) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(percentage = (count / sum(count)) * 100)

evolution_data$label <- factor(evolution_data$attachment_EU,
                               levels = 1:4,
                               labels = c("Not at all", "Not very", "Fairly", "Very"))


ggplot(evolution_data, aes(x = year, y = percentage, color = label, group = label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Very" = "#388E3C",
               "Fairly" = "#81C784",
               "Not very" = "#E57373",
               "Not at all" = "#D32F2F"),
    name = "Attachment Level"
  )+
  labs(
    title = "Evolution of Attachment to the European Union (2020–2023)",
    subtitle = "Distribution of responses (%)",
    x = "Year",
    y = "Percentage (%)",
    color = "Attachment Level"
  ) +
  scale_y_continuous(labels = label_percent(scale = 1))+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15,  hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10), color = "gray25"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12,margin = margin(r = 12)),
    legend.position = "right"
  )

ggsave("Resultados/Descriptive_plot3.jpeg", plot = last_plot(), width = 7, height = 5, dpi = 300)
ggsave("Resultados/Descriptive_plot3.png", plot = last_plot(), width = 7, height = 5, dpi = 300)


# Now the same but in a dichotomous way:
evolution_data <- datos |>
  filter(year %in% 2020:2023, attachment_EU %in% 1:4) |>
  mutate(response_group = case_when(
    attachment_EU %in% 1:2 ~ "Yes",
    attachment_EU %in% 3:4 ~ "No"
  )) |>
  group_by(year, response_group) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(percentage = (count / sum(count)) * 100)

ggplot(evolution_data, aes(x = year, y = percentage, color = response_group, group = response_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02"), name = "Attachment") +
  scale_y_continuous(limits = c(35, 65), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Evolution of Attachment to the European Union (2020–2023)",
    subtitle = "Distribution of responses (%)",
    x = "Year",
    y = "Percentage (%)",
    color = "Attachment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10), color = "gray25"),
    legend.position = "right"
  )


# -- 4: Correlation between variables  ----

datos <- datos |>
  mutate(across(starts_with("EU_issue_"), as.factor)) |>
  mutate(across(starts_with("importance_"), as.factor))

numeric_data <- datos |> select(where(is.numeric))

cor_matrix <- cor(numeric_data, use = "complete.obs")

library(ggcorrplot)
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE,        
           lab_size = 2)      

# Focus on correlation between depentent variable (attachment) and others

library(tibble)

cor_attachment <- cor_matrix["attachment_EU", , drop = FALSE] |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column(var = "Variable") |> 
  rename(Correlation = "attachment_EU") |> 
  filter(!Variable %in% c("attachment_EU", "year"))  # Excluir la autocorrelación

# Correlation plot
ggplot(cor_attachment, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  labs(
    title = "Correlation with 'Attachment to the European Union'",
    x = NULL,
    y = "Pearson Correlation"
  ) +
  geom_text(aes(label = round(Correlation, 2)), 
            hjust = ifelse(cor_attachment$Correlation > 0, -0.1, 1.1),
            size = 2.5) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust= 0.1),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  ylim(min(cor_attachment$Correlation) - 0.1, max(cor_attachment$Correlation) + 0.1)

ggsave("Resultados/Descriptive_plot4_cor.jpeg", plot = last_plot(), width = 8, height = 5, dpi = 300)
ggsave("Resultados/Descriptive_plot4_cor.png", plot = last_plot(), width = 8, height = 5, dpi = 300)


# -- 5: Correlation between European attachment and attachment to the EU ----


#Inverse scale for "European attachment"
datos <- datos |>
  mutate(attachment_Europe = recode(as.character(attachment_Europe),
                                "1" = "4",
                                "2" = "3",
                                "3" = "2",
                                "4" = "1")) |> 
  mutate(attachment_Europe = as.numeric(attachment_Europe))


# Correlation by country
correlaciones_por_pais <- datos |>
  group_by(isocntry) |>
  summarise(correlacion = cor(attachment_EU, attachment_Europe, method = "spearman", use = "complete.obs"))  # Correlación Spearman

# Plot
ggplot(correlaciones_por_pais, aes(x = reorder(isocntry, correlacion), y = correlacion, fill = correlacion)) +
  geom_bar(stat = "identity") +  # Barras para cada país
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Correlación") +  # Colores para la correlación
  labs(
    title = "Correlation between Attachment to EU and to Europe",
    subtitle = "Spearman correlation by country",
    x = "Country",
    y = "Correlation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray25"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12, margin = margin(r = 12))
  )

ggsave("Resultados/Descriptive_plot5.jpeg", plot = last_plot(), width = 7, height = 5, dpi = 300)
ggsave("Resultados/Descriptive_plot5.png", plot = last_plot(), width = 7, height = 5, dpi = 300)
