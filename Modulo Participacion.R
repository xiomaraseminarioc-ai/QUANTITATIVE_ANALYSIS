install.packages(c('tidyverse','dplyr','ggplot2','readr',
                   'tidyr','tibble','stringr', 'knitr'))

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(knitr)

getwd()

setwd("/Users/xiomaraseminario/Desktop/enaho_24/enaho_24/csvs/mod_part")

mod_part1 <- read_csv("Enaho01-2024-800A.csv", locale = locale(encoding = "Latin1"))

mod_part2 <- read_csv("Enaho01-2024-800B.csv", locale = locale(encoding = "Latin1"))

mod_part1 <- mod_part1 %>%
  mutate(organization_participation = if_else(
    rowSums(across(starts_with("P801_"), ~ .x %in% c(1:18, 20)), na.rm = TRUE) > 0,
    1, 0
  ))
mod_part_long <- mod_part1 %>%
  pivot_longer(cols = starts_with("P801_"),
               names_to = "tipo_org",
               values_to = "codigo_part")

mod_part_long <- mod_part_long %>%
  filter(codigo_part %in% c(1:18, 20))

tabla_org <- mod_part_long %>%
  count(codigo_part) %>%
  arrange(desc(n))

etiquetas <- c(
  "1" = "Deportiva", "2" = "Política", "3" = "Cultural", "4" = "Vecinal",
  "5" = "Ronda campesina", "6" = "Regantes", "7" = "Profesional",
  "8" = "Sindicato", "9" = "Club de madres", "10" = "APAFA",
  "11" = "Vaso de leche", "12" = "Comedor popular", "13" = "CLAS",
  "14" = "Presupuesto participativo", "15" = "Consejo local", "16" = "Comunidad campesina",
  "17" = "Agropecuaria", "18" = "Otro", "20" = "Desayuno escolar"
)

ggplot(tabla_org, aes(x = reorder(as.factor(codigo_part), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Participación por tipo de organización",
    x = "Tipo de organización",
    y = "Número de personas"
  ) +
  scale_x_discrete(labels = etiquetas) +
  theme_minimal()

sum(is.na(mod_part1$organization_participation))

summary(mod_part1$organization_participation)

#Crear subset
mod_part_subset <- mod_part1 %>%
  select(CONGLOME, VIVIENDA, HOGAR, CODINFOR, UBIGEO, DOMINIO, ESTRATO, organization_participation)

summary(mod_part_subset$organization_participation)

prop.table(table(mod_part_subset$organization_participation)) * 100

ggplot(mod_part_subset, aes(x = factor(organization_participation))) +
  geom_bar(fill = "blue") +
  labs(
    x = "Participa en alguna organización",
    y = "Número de personas",
    title = "Participación total"
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Sí")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Cree una variable, departamento a partir de ubigeo
departamentos <- c(
  "01" = "Amazonas",     "02" = "Áncash",       "03" = "Apurímac",    "04" = "Arequipa",
  "05" = "Ayacucho",     "06" = "Cajamarca",    "07" = "Callao",      "08" = "Cusco",
  "09" = "Huancavelica", "10" = "Huánuco",      "11" = "Ica",         "12" = "Junín",
  "13" = "La Libertad",  "14" = "Lambayeque",   "15" = "Lima",        "16" = "Loreto",
  "17" = "Madre de Dios","18" = "Moquegua",     "19" = "Pasco",       "20" = "Piura",
  "21" = "Puno",         "22" = "San Martín",   "23" = "Tacna",       "24" = "Tumbes",
  "25" = "Ucayali"
)

mod_part_subset <- mod_part_subset %>%
  mutate(departamento = substr(UBIGEO, 1, 2))

mod_part_subset <- mod_part_subset %>%
  mutate(nombre_departamento = departamentos[departamento])

tabla_grafico <- mod_part_subset %>%
  group_by(nombre_departamento) %>%
  summarise(
    porcentaje_participacion = mean(organization_participation, na.rm = TRUE) * 100
  )

resumen_departamento <- mod_part_subset %>%
  group_by(nombre_departamento) %>%
  summarise(
    media = mean(organization_participation),
    sd = sd(organization_participation),
    varianza = var(organization_participation)
  ) %>%
  arrange(desc(varianza))

print(resumen_departamento)

kable(resumen_departamento, digits = 3, caption = "Resumen estadístico por departamento")

ggplot(tabla_grafico, aes(x = reorder(nombre_departamento, porcentaje_participacion), 
                          y = porcentaje_participacion)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Porcentaje de personas que participan en organizaciones por departamento",
    x = "Departamento",
    y = "Porcentaje de participación (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

ggplot(resumen_departamento, aes(x = reorder(nombre_departamento, media), y = media)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), width = 0.3) +
  coord_flip() +
  labs(
    title = "Promedio de participación con desviación estándar por departamento",
    x = "Departamento",
    y = "Promedio de participación"
  ) +
  theme_minimal()

# Calcular la varianza por departamento
tabla_varianza <- mod_part_subset %>%
  group_by(nombre_departamento) %>%
  summarise(
    varianza_participacion = var(organization_participation, na.rm = TRUE)
  ) %>%
  arrange(desc(varianza_participacion))

ggplot(tabla_varianza, aes(x = reorder(nombre_departamento, -varianza_participacion), y = varianza_participacion)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Varianza de participación por departamento",
    x = "Departamento",
    y = "Varianza"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





