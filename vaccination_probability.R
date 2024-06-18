install.packages("ggplot2")
install.packages("dplyr")

library(dplyr)
library(ggplot2)

soma_vacinados <- function(df, start_date, end_date) {
  df %>%
    filter(date > start_date & date < end_date) %>%
    summarise(total_vacinados = sum(daily_people_vaccinated, na.rm = TRUE))
}

# Quantidade de pessoas vacinadas por ano
pessoas_vacinadas_2020 <- 7230216
pessoas_vacinadas_2021 <- 4558247607
pessoas_vacinadas_2022 <- 5504032360
pessoas_vacinadas_2023 <- 5631195241
pessoas_vacinadas_2024 <- 5631262670

# Total de pessoas no mundo
total_habitantes_mundo_aproximadamente <- 7900000000

#Probabilidade de uma pessoa estar vacinada por ano
prob_vac_2020 <- pessoas_vacinadas_2020/total_habitantes_mundo_aproximadamente
prob_vac_2021 <- pessoas_vacinadas_2021/total_habitantes_mundo_aproximadamente
prob_vac_2022 <- pessoas_vacinadas_2022/total_habitantes_mundo_aproximadamente
prob_vac_2023 <- pessoas_vacinadas_2023/total_habitantes_mundo_aproximadamente
prob_vac_2024 <- pessoas_vacinadas_2024/total_habitantes_mundo_aproximadamente

# Quantidade de pessoas totalmente vacinadas por ano (2 doses)

pessoas_totalmente_vacinadas_2020 <- 47384
pessoas_totalmente_vacinadas_2021 <- 3879378967
pessoas_totalmente_vacinadas_2022 <- 5044053376
pessoas_totalmente_vacinadas_2023 <- 5177907087
pessoas_totalmente_vacinadas_2024 <- 5177942360
