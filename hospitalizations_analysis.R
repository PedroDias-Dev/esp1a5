library(stats)
library(dplyr)
library(ggplot2)

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
       y = "Número de Admissões Semanais") +
  theme_minimal()

# Teste de Shapiro-Wilk para normalidade
teste_normalidade_internacao <- shapiro.test(covid_hospitalizations_sample$value)

# Exibe resultado do teste
cat("Teste de Shapiro-Wilk para normalidade da ocupação de UTI:\n")
cat("p-valor:", teste_normalidade_internacao$p.value, "\n")

# Se p-valor > 0.05, não podemos rejeitar a normalidade.
if (teste_normalidade_internacao$p.value > 0.05) {
  cat("Não há evidência suficiente para rejeitar a normalidade da ocupação de UTI (p-valor > 0.05).\n")
} else {
  cat("Há evidência para rejeitar a normalidade da ocupação de UTI (p-valor <= 0.05).\n")
}

# Realiza teste t para médias independentes (entre 2022 e 2023)
teste_t_hospitalizations <- t.test(covid_hospitalizations_filtered$value, older_covid_hospitalizations_filtered$value)
teste_t_hospitalizations

# Verifica resultado do teste
if (teste_t_hospitalizations$p.value < 0.05) {
  cat("Há diferença estatísticamente significativa (p-valor < 0.05) entre as médias de internação nos dois grupos.")
} else {
  cat("Não há diferença estatísticamente significativa (p-valor >= 0.05) entre as médias de internação nos dois grupos.")
}

# Teste de Correlação de Spearman
correlacao_spearman <- cor.test(covid_hospitalizations$value, as.numeric(as.Date(covid_hospitalizations$date)), method = "spearman")

# Exibe resultado do teste
cat("Teste de Correlação de Spearman (Ocupação UTI vs Data):\n")
cat("Coeficiente de correlação:", correlacao_spearman$estimate, "\n")
cat("p-valor:", correlacao_spearman$p.value, "\n")

# Interpretação do p-valor
if (correlacao_spearman$p.value < 0.05) {
  cat("Existe uma correlação estatisticamente significativa entre a ocupação de UTI e a data (p-valor < 0.05).\n")
  
  # Direção da correlação (positiva ou negativa)
  if (correlacao_spearman$estimate > 0) {
    cat("A correlação é positiva, indicando que a ocupação de UTI tende a aumentar com o tempo.\n")
  } else {
    cat("A correlação é negativa, indicando que a ocupação de UTI tende a diminuir com o tempo.\n")
  }
} else {
  cat("Não há correlação estatisticamente significativa entre a ocupação de UTI e a data (p-valor >= 0.05).\n")
}

summary(is.na(covid_hospitalizations))

# Modelo de regressão
covid_data <- covid_hospitalizations[covid_hospitalizations$indicator == "Daily ICU occupancy", ]

covid_data$date_numerico <- as.numeric(as.Date(covid_data$date))

modelo_regressao <- lm(value ~ date_numerico, data = covid_data)
summary(modelo_regressao)

# Gráfico de dispersão com reta de regressão
# (obs: esse gráfico demora para plotar)
ggplot(covid_data, aes(x = date_numerico, y = value)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relação entre Ocupação Diária de UTI e Data",
       x = "Data (dias)", y = "Ocupação de UTI")

