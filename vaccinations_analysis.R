library(dplyr)
library(ggplot2)
library(scales)

# Função para filtrar continentes
filtrar_continentes <- function(locations) {
  continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  return(locations[locations %in% continents])
}

# Função para filtrar países
filtrar_paises <- function(locations) {
  continents_and_classes <- c("World", "Africa", "Asia", "Europe", "North America", "Oceania", "South America", "High income", "Upper middle income", "Lower middle income", "Low income")
  return(locations[!locations %in% continents_and_classes])
}

# Função para filtrar classes sociais
filtrar_classes_sociais <- function(locations) {
  classes_sociais <- c("High income", "Upper middle income", "Lower middle income", "Low income")
  return(locations[locations %in% classes_sociais])
}


locations <- vaccinations_filtered$location

continentes <- filtrar_continentes(locations)
paises <- filtrar_paises(locations)
classes_sociais <- filtrar_classes_sociais(locations)

# Total de Vacinações no Mundo
world_vaccinations <- vaccinations_filtered %>%
  filter(location == "World")

ggplot(world_vaccinations, aes(x = date, y = people_vaccinated)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total de Pessoas Vacinadas no Mundo",
       x = "Data",
       y = "Total de Pessoas Vacinadas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

# Gráfico dos continentes com mais vacinações
top_paises <- vaccinations %>%
  filter(location %in% continentes) %>%
  arrange(location, desc(date)) %>% 
  distinct(location, .keep_all = TRUE) %>% 
  group_by(location) %>%
  summarise(people_vaccinated = sum(people_vaccinated, na.rm = TRUE)) %>%
  arrange(desc(people_vaccinated)) %>%
  top_n(5)

ggplot(top_paises, aes(x = reorder(location, people_vaccinated), y = people_vaccinated, fill = location)) +
  geom_bar(stat = "identity") +
  labs(title = "Continentes com Mais Vacinações Totais",
       x = "Continente",
       y = "Total de Vacinações") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = comma)

# Gráfico dos países com mais vacinações
top_paises <- vaccinations %>%
  filter(location %in% paises) %>%
  arrange(location, desc(date)) %>%
  distinct(location, .keep_all = TRUE) %>%
  group_by(location) %>%
  summarise(people_vaccinated = sum(people_vaccinated, na.rm = TRUE)) %>%
  arrange(desc(people_vaccinated)) %>%
  top_n(5)

ggplot(top_paises, aes(x = reorder(location, people_vaccinated), y = people_vaccinated, fill = location)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Países com Mais Vacinações Totais",
       x = "País",
       y = "Total de Vacinações") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = comma)

# Gráfico das classes sociais com mais vacinações
top_paises <- vaccinations %>%
  filter(location %in% classes_sociais) %>%
  arrange(location, desc(date)) %>%
  distinct(location, .keep_all = TRUE) %>%
  group_by(location) %>%
  summarise(people_vaccinated = sum(people_vaccinated, na.rm = TRUE)) %>%
  arrange(desc(people_vaccinated)) %>%
  top_n(5)

ggplot(top_paises, aes(x = reorder(location, people_vaccinated), y = people_vaccinated, fill = location)) +
  geom_bar(stat = "identity") +
  labs(title = "Classes Sociais com Mais Vacinações Totais",
       x = "Classe Social",
       y = "Total de Vacinações") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = comma)

# Gráfico das maiores médias de vacinação diária
top_paises_media <- vaccinations_filtered %>%
  filter(location %in% paises) %>%
  group_by(location) %>%
  summarise(media_daily_vaccinations = mean(daily_vaccinations, na.rm = TRUE)) %>%
  arrange(desc(media_daily_vaccinations)) %>%
  top_n(5)

ggplot(top_paises_media, aes(x = reorder(location, media_daily_vaccinations), y = media_daily_vaccinations, fill = location)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Países com Maior Média de Vacinações Diárias",
       x = "País",
       y = "Média de Vacinações Diárias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = comma)

# Total de vacinados por data
total_vacinados_por_data <- aggregate(total_vaccinations ~ date, vaccinations, sum)
total_vacinados_por_data

# Gera plot de linha do total de vacinados
ggplot(total_vacinados_por_data, aes(x = date, y = total_vaccinations)) +
  geom_line(color = "blue") +
  labs(title = "Total de Pessoas Vacinadas por Data (Todos os Países)",
       x = "Data", y = "Total Vacinados")

# Gráfico de Vacinações nos Estados Unidos
us_vaccinations <- vaccinations %>% 
  filter(location == "United States")

ggplot(us_vaccinations, aes(x = date, y = people_vaccinated)) +
  geom_line(color = "blue") +
  labs(title = "Total de Pessoas Vacinadas nos Estados Unidos",
       x = "Data",
       y = "Total de Pessoas Vacinadas") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
