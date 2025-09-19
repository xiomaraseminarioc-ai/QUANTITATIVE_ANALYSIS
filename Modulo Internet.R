install.packages(c('tidyverse','dplyr','ggplot2','readr',
                   'tidyr','tibble','stringr'))

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(stringr)

getwd()

setwd("/Users/xiomaraseminario/Desktop/enaho_24/enaho_24/csvs/mod_internet")

mod_internet <- read_csv("Enaho01A-2024-300.csv", locale = locale(encoding = "Latin1"))

#Renombrar variables

mod_internet <- mod_internet %>% 
  rename(use_internet = P314A)

mod_internet <- mod_internet %>% 
  rename(use_internet_information_government = `P316$1` )

mod_internet <- mod_internet %>% 
  rename(use_internet_social_media = `P316$2`)

mod_internet <- mod_internet %>% 
  rename(use_internet_public_transactions= `P316$6`)

names(mod_internet)

#Crear un subset
mod_int_subset <- mod_internet %>%
  select(CONGLOME, VIVIENDA, HOGAR, CODPERSO, UBIGEO, DOMINIO, ESTRATO, use_internet, use_internet_information_government,
         use_internet_social_media, use_internet_public_transactions)

glimpse(mod_int_subset)

#Variable use_internet

mod_int_subset <- mod_int_subset %>%
  mutate(use_internet = ifelse(use_internet == 9, NA, use_internet))

sum(is.na(mod_int_subset$use_internet))

mod_int_subset %>%
  summarise(media = mean(use_internet, na.rm = TRUE))


#Quitar NA
mod_int_clean <- mod_int_subset %>%
  filter(use_internet %in% c(1, 2))

#Uso de internet - general
ggplot(mod_int_clean, aes(x = factor(use_internet))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = 'Uso de Internet',
    x = '¿Usó Internet en el mes anterior?',
    y ='Número de personas'
  ) +
  scale_x_discrete(labels = c('1' = 'Sí', '2' = 'No')) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#Filtrar solo los que usan internet para facilitar analisis descriptivo de otras variables
mod_int_filtered <- mod_int_clean %>%
  filter(use_internet == 1)

#Variable use_internet_information_government'
tabla_information_government <- mod_int_filtered %>%
  filter(use_internet_information_government %in% c(1, 2)) %>%
  group_by(use_internet_information_government) %>%
  summarise(
    frecuencia = n()
  ) %>%
  mutate(
    porcentaje = round((frecuencia / sum(frecuencia)) * 100, 1),
    etiqueta = case_when(
      use_internet_information_government == 1 ~ "Sí realizó transacciones",
      use_internet_information_government == 2 ~ "No realizó transacciones"
    )
  )

ggplot(mod_int_filtered, aes(x = factor(use_internet_information_government))) +
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Uso de internet para informacion gubernamental',
    x = ' Obtener información (sobre bienes y servicios, salud, 
    organizaciones gubernamentales)',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels = c('1' = 'Si', '2'= 'No')) +
  theme_classic()+
  theme(plot.title =element_text(hjust = 0.5))

#Variable use_internet_social_media
tabla_social_media <- mod_int_filtered %>%
  filter(use_internet_social_media %in% c(1, 2)) %>%
  group_by(use_internet_social_media) %>%
  summarise(
    frecuencia = n()
  ) %>%
  mutate(
    porcentaje = round((frecuencia / sum(frecuencia)) * 100, 1),
    etiqueta = case_when(
      use_internet_social_media == 1 ~ "Sí",
      use_internet_social_media == 2 ~ "No"
    )
  )

ggplot(mod_int_filtered, aes(x = factor(use_internet_social_media))) +
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Uso de internet para redes sociales',
    x = ' Comunicarse (e-mail, chat, llamadas por Skype, 
    WhatsApp, Facebook, Twitter, etc.)?',
    y = 'Numero de personas'
  ) +
  scale_x_discrete(labels = c('1' = 'Si', '2'= 'No')) +
  theme_classic()+
  theme(plot.title =element_text(hjust = 0.5))

#Variable use_internet_public_transactions
tabla_transactions <- mod_int_filtered %>%
  filter(use_internet_public_transactions %in% c(1, 2)) %>%
  group_by(use_internet_public_transactions) %>%
  summarise(
    frecuencia = n()
  ) %>%
  mutate(
    porcentaje = round((frecuencia / sum(frecuencia)) * 100, 1),
    etiqueta = case_when(
      use_internet_public_transactions == 1 ~ "Sí realizó transacciones",
      use_internet_public_transactions == 2 ~ "No realizó transacciones"
    )
  )

ggplot(mod_int_filtered, aes(x = factor(use_internet_public_transactions)))+
  geom_bar(fill = 'steelblue')+
  labs(
    title = 'Transacciones con autoridades u organizaciones publicas',
    x= '¿Realizo transacciones?',
    y= 'Número de personas'
  )+
  scale_x_discrete(labels = c('1' = 'Si', '2' = 'No'))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#Diferencia en uso de internet

mod_int_filtered <- mod_int_filtered %>%
  mutate(
    info_gob = ifelse(use_internet_information_government == 1, 1, 0),
    redes = ifelse(use_internet_social_media == 1, 1, 0),
    transacciones = ifelse(use_internet_public_transactions == 1, 1, 0)
  )

porcentajes_df <- data.frame(
  tipo_uso = c("Obtener información", "Redes sociales", "Transacciones públicas"),
  porcentaje = c(
    mean(mod_int_filtered$info_gob, na.rm = TRUE) * 100,
    mean(mod_int_filtered$redes, na.rm = TRUE) * 100,
    mean(mod_int_filtered$transacciones, na.rm = TRUE) * 100
  )
)

ggplot(porcentajes_df, aes(x = reorder(tipo_uso, -porcentaje), y = porcentaje)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.3, size = 4) +
  labs(
    title = "Formas de uso del Internet",
    x = "Tipo de uso",
    y = "Porcentaje (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1)
  ) +
  ylim(0, max(porcentajes_df$porcentaje) + 10)

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

mod_int_filtered <- mod_int_filtered %>%
  mutate(departamento = substr(UBIGEO, 1, 2))

mod_int_filtered <- mod_int_filtered %>%
  mutate(nombre_departamento = departamentos[departamento])

#Crear un indicador compuesto para el analisis de la varianza
mod_int_filtered <- mod_int_filtered %>%
  mutate(
    uso_internet_total = info_gob + redes + transacciones
  )

resumen_departamentos <- mod_int_filtered %>%
  group_by(nombre_departamento) %>%
  summarise(
    promedio_uso = mean(uso_internet_total, na.rm = TRUE),
    desviacion = sd(uso_internet_total, na.rm = TRUE),
    varianza = var(uso_internet_total, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(promedio_uso))

resumen_departamentos %>% 
  arrange(desc(varianza)) %>%
  knitr::kable(digits = 2, caption = "Resumen estadístico del uso de internet por departamento")

print(resumen_departamentos)

ggplot(resumen_departamentos, aes(x = reorder(nombre_departamento, -promedio_uso), y = promedio_uso)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = promedio_uso - desviacion, ymax = promedio_uso + desviacion), width = 0.3) +
  labs(
    title = "Promedio de formas de uso del internet por departamento",
    x = "Departamento",
    y = "Promedio de uso (0 a 3)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


