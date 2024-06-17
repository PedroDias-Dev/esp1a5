# Maiores Médias de Ocupação Diária de UTIs
icu_hospitalizations <- covid_hospitalizations_filtered[covid_hospitalizations_filtered$indicator == "Daily ICU occupancy", ]

icu_hospitalizations_summary <- aggregate(value ~ entity, data = icu_hospitalizations, FUN = mean)

icu_hospitalizations_summary <- icu_hospitalizations_summary[order(icu_hospitalizations_summary$value, decreasing = TRUE), ]

top_countries <- head(icu_hospitalizations_summary, 10)

ggplot(top_countries, aes(x = reorder(entity, value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Países com Maior Média de Ocupação Diária de UTI",
       x = "País",
       y = "Média de Ocupação Diária de UTI") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Maiores Médias de Ocupação Diária de Hospitais
hospitalizations <- covid_hospitalizations_filtered[covid_hospitalizations_filtered$indicator == "Daily hospital occupancy", ]

hospitalizations_summary <- aggregate(value ~ entity, data = hospitalizations, FUN = mean)

hospitalizations_summary <- hospitalizations_summary[order(hospitalizations_summary$value, decreasing = TRUE), ]

top_countries <- head(hospitalizations_summary, 10)

ggplot(top_countries, aes(x = reorder(entity, value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Países com Maior Média de Ocupação Diária de Hospitais",
       x = "País",
       y = "Média de Ocupação Diária de Hospitais") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Maiores Médias de Admissões Semanais
weekly_hospitalizations <- covid_hospitalizations_filtered[covid_hospitalizations_filtered$indicator == "Weekly new hospital admissions", ]

weekly_hospitalizations_summary <- aggregate(value ~ entity, data = weekly_hospitalizations, FUN = mean)

weekly_hospitalizations_summary <- weekly_hospitalizations_summary[order(weekly_hospitalizations_summary$value, decreasing = TRUE), ]

top_countries <- head(weekly_hospitalizations_summary, 10)

ggplot(top_countries, aes(x = reorder(entity, value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Países com Maior Média de Admissões Semanais",
       x = "País",
       y = "Média de Admissões Semanais") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Admissões Semanais nos Estados Unidos
usa_hospital_admissions <- covid_hospitalizations_filtered %>%
  filter(iso_code == "USA", indicator == "Weekly new hospital admissions")

usa_hospital_admissions$date <- as.Date(usa_hospital_admissions$date)

ggplot(usa_hospital_admissions, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Número de Admissões Semanais nos Estados Unidos",
       x = "Data",
       y = "Número de Admissões Semanais") +
  theme_minimal()