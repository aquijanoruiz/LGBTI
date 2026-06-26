# Librerías
library(readxl)
library(tidyverse)
library(srvyr)

# ---- Base de datos y diseño muestral ----
# Base de datos
base_lgbti <- read_excel("rawdata/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx")

# Diseño muestral
base_lgbti_fexp <- base_lgbti %>%
  as_survey_design(weights=fexp)

# ---- Quiénes conocen sobre la orientación sexual o identidad de género ---- 

tab <- function(var, grupo) {
  base_lgbti_fexp %>% 
    filter({{ var }} != "No aplica") %>% # aplica a preguntas s08_p01_X
    filter({{ var }} != "No sabe") %>% # aplica a preguntas s08_p01_X_1a
    group_by(respuesta = {{ var }}) %>%
    summarize(proportion = survey_prop(vartype = NULL)) %>%
    mutate(
      grupo = grupo,
      porcentaje = proportion * 100
    ) %>%
    select(grupo, respuesta, porcentaje)
}

tab_conocen <- bind_rows(
  tab(s08_p01_1, "Madre"),
  tab(s08_p01_2, "Padre"),
  tab(s08_p01_5, "Hermanas/os"),
  tab(s08_p01_7, "Amigas/os"),
  tab(s08_p01_8, "Compañeras/os de estudio/trabajo")
)

plot_conocen <- tab_conocen %>%
  filter(respuesta == "sí") %>%
  ggplot(aes(x = reorder(grupo, porcentaje), y = porcentaje)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Personas que conocen la orientación sexual o identidad de género"
  )

# ---- Aceptación de la orientación sexual o identidad de género ---- 

tab_aceptacion <- bind_rows(
  tab(s08_p01_1_1a, "Madre"),
  tab(s08_p01_2_1a, "Padre"),
  tab(s08_p01_5_1a, "Hermanas/os"),
  tab(s08_p01_7_1a, "Amigas/os"),
  tab(s08_p01_8_1a, "Compañeras/os de estudio/trabajo")
)

plot_aceptacion_separada <- tab_aceptacion %>%
  ggplot(aes(x = grupo, y = porcentaje, fill = respuesta)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Porcentaje",
    fill = "Aceptación",
    title = "Aceptación de la orientación sexual o identidad de género"
  )

# ---- Experiencias de discriminación y violencia ----


# ---- Prácticas de conversión ----
