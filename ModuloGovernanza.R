install.packages(c('tidyverse','dplyr','ggplot2','readr',
                   'tidyr','tibble','stringr','purrr'))

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)

getwd()

setwd("~/Desktop/enaho_24/enaho_24/csvs/mod_gob")

#Renombrar variables

mod_gob1 <- read_csv("Enaho01B-2024-1.csv", locale = locale(encoding = "Latin1"))

mod_gob2 <- read_csv("Enaho01B-2024-2.csv", locale = locale(encoding = "Latin1"))

mod_gob1 <- mod_gob1 %>% 
 rename(official_trust_Provincial =`P1$04`)

mod_gob1 <- mod_gob1 %>% 
  rename(official_trust_Distrital =`P1$05`)

mod_gob1 <- mod_gob1 %>% 
  rename(official_trust_Regional= `P1$08`)

mod_gob1 <- mod_gob1 %>% 
  rename(official_evaluation_Central= `P2A1$1`)

mod_gob1 <- mod_gob1 %>% 
  rename(official_evaluation_Regional= `P2A1$2`)

mod_gob1 <- mod_gob1 %>% 
  rename(official_evaluation_Provincial= `P2A1$3`)

mod_gob1 <- mod_gob1 %>% 
  rename(official_evaluation_Distrital= `P2A1$4`)
  
mod_gob1 <- mod_gob1 %>% 
  rename(dem_importance = P6)

mod_gob1 <- mod_gob1 %>% 
  rename(dem_functioning = P7)

mod_gob1 <- mod_gob1 %>% 
  rename(regional_identification = P21)

names(mod_gob1)

#Crear un subset
mod_gob_subset <- mod_gob1 %>%
  select(CONGLOME, VIVIENDA, HOGAR, CODPERSO, UBIGEO, DOMINIO, ESTRATO, 
         official_trust_Provincial, official_trust_Distrital, 
         official_trust_Regional,official_evaluation_Central,
         official_evaluation_Regional,official_evaluation_Provincial, 
         official_evaluation_Distrital, dem_importance, dem_functioning, 
         regional_identification
         )

mod_gob_clean <- mod_gob_subset %>%
  mutate(across(
    c(official_trust_Provincial, official_trust_Distrital, official_trust_Regional,
      official_evaluation_Central, official_evaluation_Regional,
      official_evaluation_Provincial, official_evaluation_Distrital,
      dem_importance, dem_functioning,regional_identification),
    ~ ifelse(. == 5, NA, .)
  ))

#Estadisticas descriptivas
table(mod_gob_clean$official_trust_Distrital)
table(mod_gob_clean$official_trust_Provincial)
table(mod_gob_clean$official_trust_Regional)
table(mod_gob_clean$official_evaluation_Central)
table(mod_gob_clean$official_evaluation_Regional)
table(mod_gob_clean$official_evaluation_Provincial)
table(mod_gob_clean$official_evaluation_Distrital)
table(mod_gob_clean$dem_importance)
table(mod_gob_clean$dem_functioning)
table(mod_gob_clean$regional_identification)

#Confianza Gobierno Regional
ggplot(mod_gob_clean, aes(x = factor(official_trust_Distrital)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Distribucion de confianza en el Gobierno Regional',
    x = 'Nivel de Confianza',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Nada',
    '2' = 'Poca',
    '3' = 'Algo',
    '4' = 'Mucha',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Confianza Gobierno Provincial
ggplot(mod_gob_clean, aes(x = factor(official_trust_Provincial)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Distribucion de confianza en el Gobierno Provincial',
    x = 'Nivel de Confianza',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Nada',
    '2' = 'Poca',
    '3' = 'Algo',
    '4' = 'Mucha',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Confianza Gobierno Distrital
ggplot(mod_gob_clean, aes(x = factor(official_trust_Distrital)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title= 'Distribucion de confianza en el Gobierno Distrital',
    x = 'Nivel de Confianza',
    y = 'Numero de personas'
  ) + scale_x_discrete(labels= c(
    '1' = 'Nada',
    '2' = 'Poca',
    '3' = 'Algo',
    '4' = 'Mucha',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Evaluacion Gobierno Central
ggplot(mod_gob_clean, aes(x = factor(official_evaluation_Central)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Opinion sobre la Gestion del Gobierno Central',
    x = 'Opinion',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Muy buena',
    '2' = 'Buena',
    '3' = 'Mala',
    '4' = 'Muy Mala',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Evaluacion Gobierno Regional
ggplot(mod_gob_clean, aes(x = factor(official_evaluation_Regional)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Opinion sobre la Gestion del Gobierno Regional',
    x = 'Opinion',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Muy buena',
    '2' = 'Buena',
    '3' = 'Mala',
    '4' = 'Muy Mala',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Evaluacion Gobierno Provincial
ggplot(mod_gob_clean, aes(x = factor(official_evaluation_Provincial)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Opinion sobre la Gestion del Gobierno Provincial',
    x = 'Opinion',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Muy buena',
    '2' = 'Buena',
    '3' = 'Mala',
    '4' = 'Muy Mala',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Evaluacion Gobierno Distrital
ggplot(mod_gob_clean, aes(x = factor(official_evaluation_Distrital)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Opinion sobre la Gestion del Gobierno Distrital',
    x = 'Opinion',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Muy buena',
    '2' = 'Buena',
    '3' = 'Mala',
    '4' = 'Muy Mala',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Importancia Democracia
ggplot(mod_gob_clean, aes(x = factor(dem_importance)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Opinion sobre la importancia de la democracia',
    x = 'Opinion',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Nada Importante',
    '2' = 'Poco Importante',
    '3' = 'Importante',
    '4' = 'Muy Importante',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Funcionamiento Democracia
ggplot(mod_gob_clean, aes(x = factor(dem_functioning)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Opinion sobre el funcionamiento de la democracia',
    x = 'Opinion',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels= c(
    '1' = 'Muy mal',
    '2' = 'Mal',
    '3' = 'Bien',
    '4' = 'Muy bien',
    '5' = 'No sabe'
  ))+
  theme_minimal()

#Identificacion Regional

ggplot(mod_gob_clean, aes(x = factor(regional_identification))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Identificación con la región",
    x = "¿Se identifica con su región?",
    y = "Número de personas"
  ) +
  scale_x_discrete(labels = c("1" = "Sí", "0" = "No")) +
  theme_minimal()

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

mod_gob_clean <- mod_gob_clean %>%
  mutate(departamento = substr(UBIGEO, 1, 2))

mod_gob_clean <- mod_gob_clean %>%
  mutate(nombre_departamento = departamentos[departamento])

#Varianza por departamento
##Un indicador compuesto por dimension (confianza, evaluacion,democracia)
mod_gob_clean <- mod_gob_clean %>%
  mutate(
    ind_confianza = rowMeans(across(c(official_trust_Provincial, official_trust_Distrital, official_trust_Regional)), na.rm = TRUE),
    ind_evaluacion = rowMeans(across(c(official_evaluation_Central, official_evaluation_Regional, official_evaluation_Provincial, official_evaluation_Distrital)), na.rm = TRUE),
    ind_democracia = rowMeans(across(c(dem_importance, dem_functioning)), na.rm = TRUE)
  )

summary(mod_gob_clean$ind_confianza)
summary(mod_gob_clean$ind_evaluacion)
summary(mod_gob_clean$ind_democracia)

varianza_departamental <- mod_gob_clean %>%
  group_by(nombre_departamento) %>%
  summarise(
    var_confianza = var(ind_confianza, na.rm = TRUE),
    var_evaluacion = var(ind_evaluacion, na.rm = TRUE),
    var_democracia = var(ind_democracia, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(nombre_departamento)

print(varianza_departamental)

ggplot(varianza_departamental, aes(x = reorder(nombre_departamento, var_confianza), y = var_confianza)) +
  geom_col(fill = 'steelblue') +
  coord_flip() +
  labs(
    title = "Varianza en la confianza en el gobierno local por departamento",
    x = "Departamento",
    y = "Varianza del indicador de confianza"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(varianza_departamental, aes(x = reorder(nombre_departamento, var_evaluacion), y = var_evaluacion)) +
  geom_col(fill = 'orange') +
  coord_flip() +
  labs(
    title = "Varianza en la evaluación de la gestión por departamento",
    x = "Departamento",
    y = "Varianza del indicador de evaluación"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(varianza_departamental, aes(x = reorder(nombre_departamento, var_democracia), y = var_democracia)) +
  geom_col(fill = 'red') +
  coord_flip() +
  labs(
    title = "Varianza en la percepción sobre la democracia por departamento",
    x = "Departamento",
    y = "Varianza del indicador de democracia"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

#Identidad Regional
ident_por_dep <- mod_gob_clean %>%
  filter(!is.na(regional_identification)) %>%
  group_by(nombre_departamento) %>%
  summarise(
    total = n(),
    identificados = sum(regional_identification == 1),
    porcentaje = round(100 * identificados / total, 1)
  )

ggplot(ident_por_dep, aes(x = reorder(nombre_departamento, porcentaje), y = porcentaje)) +
  geom_col(fill = "#1b9e77") +
  coord_flip() +
  labs(
    title = "Porcentaje de identificación con la región por departamento",
    x = "Departamento",
    y = "% que se identifica con su región"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

#Variacion por departamento por indicador
resultados_gob <- mod_gob_clean %>%
  group_by(nombre_departamento) %>%
  summarise(
    media_confianza = mean(ind_confianza, na.rm = TRUE),
    var_confianza = sum((ind_confianza - mean(ind_confianza, na.rm = TRUE))^2, na.rm = TRUE) / (sum(!is.na(ind_confianza)) - 1),
    sd_confianza = sqrt(var_confianza),
    
    media_evaluacion = mean(ind_evaluacion, na.rm = TRUE),
    var_evaluacion = sum((ind_evaluacion - mean(ind_evaluacion, na.rm = TRUE))^2, na.rm = TRUE) / (sum(!is.na(ind_evaluacion)) - 1),
    sd_evaluacion = sqrt(var_evaluacion),
    
    media_democracia = mean(ind_democracia, na.rm = TRUE),
    var_democracia = sum((ind_democracia - mean(ind_democracia, na.rm = TRUE))^2, na.rm = TRUE) / (sum(!is.na(ind_democracia)) - 1),
    sd_democracia = sqrt(var_democracia)
  )

