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


# Ocupação Diária de UTIs nos Estados Unidos
usa_icu_occupancy <- covid_hospitalizations %>%
  filter(iso_code == "USA", indicator == "Daily ICU occupancy")

# Converter coluna de data para formato Date
usa_icu_occupancy$date <- as.Date(usa_icu_occupancy$date)

# Criar gráfico de linha para visualizar a ocupação diária de UTIs
ggplot(usa_icu_occupancy, aes(x = date, y = value)) +
  geom_line(color = "red") +
  labs(title = "Ocupação Diária de UTIs nos Estados Unidos",
       x = "Data",
       y = "Número de Ocupações Diárias de UTIs") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)