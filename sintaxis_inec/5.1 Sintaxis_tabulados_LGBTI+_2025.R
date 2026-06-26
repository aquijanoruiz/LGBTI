library(readr) # para importar base  read_csv, read_csv2, read_delim, read_table , read_rds
library(tidyverse) #  tibble:: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools)
library(readxl)
library(dplyr) # Librería necesaria para case_when
library(RDS)
library(lubridate)
library(openxlsx)
library(labelled)
library(stringr)
# si no funciona el RDS instalar lo siguiente:
# install.packages("purrr") # Ejecuta el siguiente comando en la consola:
# update.packages(ask = FALSE, checkBuilt = TRUE)

# Limpia espacio de trabajo, memoria (primer paso para un script) --------
rm(list = ls()) 

# Importa base de excel ---------------------------------------------------
# osig <- read_excel("C:/Users/waguirre/Desktop/LGBTI+_VF_r/Validación OSIG/REPORTE_OSIG AL 26-12-2025.xlsx") # Base preliminar
# base_estudio <- read_excel("Base_madre_actualizada_10.xlsx") # Base versión final en id_formulario
base_lgbti <- read_excel("Base_datos_ENCV_LGBTI+_2025_tratada_fexp_vf.xlsx", guess_max = 10000) # Base versión final en id_formulario

# "Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3.xlsx

# Filtro id y recruiter.id completos
base_lgbti <- base_lgbti[-which(is.na(base_lgbti$quien_refiere_unico)),]

# Cálculo de network.size.variable
a <- base_lgbti[,c("c_p03l","c_p03g","c_p03b","c_p03tm","c_p03tf","c_p03i","c_p03p","c_p03a","c_p03o")]
to_integer <- function(df) {
        df[] <- lapply(df, as.integer)
        return(df)
}
a <- to_integer(a)
suma <- function(x) sum(x,na.rm = TRUE)
network.size.variable <- apply(a, 1, suma)
base_lgbti <- cbind(base_lgbti,network.size.variable)
base_lgbti$network.size.variable[base_lgbti$network.size.variable == 0] <- 1 # error en el inverso de network.size.variable

# Datos anómales
#base$ola <- as.integer(base$ola)
class(base_lgbti$ola)
b <- 0
#d <- as.numeric(quantile(x = rds.data$network.size.variable, probs = 0.5, na.rm = TRUE, type = 1))
for (i in 0:20) {
        
        if (i == 0) {
                
                a <- base_lgbti$network.size.variable[base_lgbti$ola == i]
                c <- as.numeric(quantile(x = a, probs = 0.992, na.rm = TRUE, type = 1))
                d <- as.numeric(quantile(x = a, probs = 0.5, na.rm = TRUE, type = 1))
                base_lgbti$network.size.variable[base_lgbti$ola == i & base_lgbti$network.size.variable > c] <- d
                
                a <- base_lgbti$network.size.variable[base_lgbti$ola == i]
                b <- b + sum(a)
                
        } else {
                
                a <- base_lgbti$network.size.variable[base_lgbti$ola == i]
                c <- as.numeric(quantile(x = a, probs = 0.995, na.rm = TRUE, type = 1))
                d <- as.numeric(quantile(x = a, probs = 0.5, na.rm = TRUE, type = 1))
                base_lgbti$network.size.variable[base_lgbti$ola == i & base_lgbti$network.size.variable > c] <- d
                
                a <- base_lgbti$network.size.variable[base_lgbti$ola == i]
                b <- b + sum(a)
                
        }
        
}
b # 380263
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# 1. Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))
#============================================================================
#============= Cálculo de tabulados =========================================

library(dplyr)
library(purrr)
library(openxlsx)
library(srvyr) # Crucial para usar as_survey_design

# # vector de variables
variables <- c("s01_p02","s01_p03", "s01_p04", "s01_p05", # Sección 1
               "gr_edad","s03_p04","s03_p05","s03_p09", # seccion 3
               "s04_p02","niv_instruc", # sección 4
               "s05_p08","s05_p19","rama_activi","grupo_ocup", # sección 5
               "s06_p02b","s06_p03","identidad_genero", # sección 6
               "s07_p05","s07_p06","s07_p07","s07_p08","s07_p18", # sección 7
               "s08_p05","s08_p09", # sección 8
               "s09_p02","s09_p03","s09_p05", # sección 9
               "s10_p09", # sección 10
               "s11_p02","s11_p03") # sección 11
               
# 2. Ahora aplicamos la recodificación buscando palabras clave

#============================================================================
factor_exp <- base_lgbti %>%
        as_survey_design(weights=fexp)

# función de frecuencias
frecuencias <- function(data, variable, factor_exp) {
        data %>%
                filter(!is.na(.data[[variable]])) %>%
                group_by(categoria = .data[[variable]]) %>%
                summarise(
                        n_muestral = n(),
                        n_ponderado = sum(.data[[factor_exp]], na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                mutate(
                        porcentaje_muestral = round(n_muestral / sum(n_muestral) *100, 4),
                        porcentaje_ponderado = round(n_ponderado / sum(n_ponderado) *100, 4)
                )
}
# Resultados.
resultados <- map(variables, ~ frecuencias(base_lgbti, .x, "fexp"))
names(resultados) <- variables
print(resultados)


##Excel
wb <- createWorkbook()
for (i in seq_along(resultados)) {
        addWorksheet(wb, names(resultados)[i])
        writeData(wb, sheet = names(resultados)[i], resultados[[i]])
}

saveWorkbook(wb, "C:/Users/PC/Desktop/Otros/Rstudio_otros_ENCV_LGBTI/frecuencias.xlsx", overwrite = TRUE)
# =============================================================================

#======================== Variables con filtro ===============================
# Migración interna ==========================================================

base_lgbti <- subset(base_lgbti,s03_p10 == "En otro lugar del país")

variables <- c("s03_p11") 
# 2. Ahora aplicamos la recodificación buscando palabras clave

factor_exp <- base_lgbti %>%
        as_survey_design(weights=fexp)

# función de frecuencias
frecuencias <- function(data, variable, factor_exp) {
        data %>%
                filter(!is.na(.data[[variable]])) %>%
                group_by(categoria = .data[[variable]]) %>%
                summarise(
                        n_muestral = n(),
                        n_ponderado = sum(.data[[factor_exp]], na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                mutate(
                        porcentaje_muestral = round(n_muestral / sum(n_muestral) *100, 4),
                        porcentaje_ponderado = round(n_ponderado / sum(n_ponderado) *100, 4)
                )
}
# Resultados.
resultados <- map(variables, ~ frecuencias(base_lgbti, .x, "fexp"))
names(resultados) <- variables
print(resultados)


##Excel
wb <- createWorkbook()
for (i in seq_along(resultados)) {
        addWorksheet(wb, names(resultados)[i])
        writeData(wb, sheet = names(resultados)[i], resultados[[i]])
}

saveWorkbook(wb, "C:/Users/PC/Desktop/Otros/Rstudio_otros_ENCV_LGBTI/frecuencias.xlsx", overwrite = TRUE)
#======================== Variables con filtro ===============================

# *************** Nota técnica ************************************************

# Se debe volver a cargar la base para poder ejecutar el segundo fitro

#  Inmigración (extranjeros ==================================================

base_lgbti <- subset(base_lgbti,s03_p10 == "En otro país")

variables <- c("s03_p11","s03_p12") 

factor_exp <- base_lgbti %>%
        as_survey_design(weights=fexp)

# función de frecuencias
frecuencias <- function(data, variable, factor_exp) {
        data %>%
                filter(!is.na(.data[[variable]])) %>%
                group_by(categoria = .data[[variable]]) %>%
                summarise(
                        n_muestral = n(),
                        n_ponderado = sum(.data[[factor_exp]], na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                mutate(
                        porcentaje_muestral = round(n_muestral / sum(n_muestral) *100, 4),
                        porcentaje_ponderado = round(n_ponderado / sum(n_ponderado) *100, 4)
                )
}
# Resultados.
resultados <- map(variables, ~ frecuencias(base_lgbti, .x, "fexp"))
names(resultados) <- variables
print(resultados)


##Excel
wb <- createWorkbook()
for (i in seq_along(resultados)) {
        addWorksheet(wb, names(resultados)[i])
        writeData(wb, sheet = names(resultados)[i], resultados[[i]])
}

saveWorkbook(wb, "C:/Users/PC/Desktop/Otros/Rstudio_otros_ENCV_LGBTI/frecuencias.xlsx", overwrite = TRUE)

# =============================================================================

#======================== Variables con filtro ===============================

# *************** Nota técnica ************************************************

# Se debe volver a cargar la base para poder ejecutar el segundo fitro

# Año de registro matrimonio ================================================

base_lgbti <- subset(base_lgbti,s10_p06 >= "2019")

variables <- c("s10_p06") 
# 2. Ahora aplicamos la recodificación buscando palabras clave

factor_exp <- base_lgbti %>%
        as_survey_design(weights=fexp)

# función de frecuencias
frecuencias <- function(data, variable, factor_exp) {
        data %>%
                filter(!is.na(.data[[variable]])) %>%
                group_by(categoria = .data[[variable]]) %>%
                summarise(
                        n_muestral = n(),
                        n_ponderado = sum(.data[[factor_exp]], na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                mutate(
                        porcentaje_muestral = round(n_muestral / sum(n_muestral) *100, 4),
                        porcentaje_ponderado = round(n_ponderado / sum(n_ponderado) *100, 4)
                )
}
# Resultados.
resultados <- map(variables, ~ frecuencias(base_lgbti, .x, "fexp"))
names(resultados) <- variables
print(resultados)


##Excel
wb <- createWorkbook()
for (i in seq_along(resultados)) {
        addWorksheet(wb, names(resultados)[i])
        writeData(wb, sheet = names(resultados)[i], resultados[[i]])
}

saveWorkbook(wb, "C:/Users/PC/Desktop/Otros/Rstudio_otros_ENCV_LGBTI/frecuencias.xlsx", overwrite = TRUE)

#======================variable con cruces========================================

#==sintaxis pregunta quienes conocen su orientación sexual e identidad de género=


library(dplyr)
library(tidyr)

# 1. Función de redondeo ajustada para 1 DECIMAL (Garantiza suma 100.0)
rds2_round_1d <- function(x) {
        # Trabajamos con una escala de 1000 (100.0 * 10)
        scaled_x <- x * 10
        res <- floor(scaled_x)
        diff <- round(1000 - sum(res)) 
        
        if (diff > 0) {
                # Ordenamos por el resto decimal para repartir la diferencia
                orden <- order(scaled_x %% 1, decreasing = TRUE)
                res[orden[1:diff]] <- res[orden[1:diff]] + 1
        }
        
        # Devolvemos al formato original con 1 decimal
        return(res / 10)
}

# 2. Mapeo de columnas
mapeo_columnas <- c(
        "s08_p01_1_1a" = "Madre",
        "s08_p01_2_1a" = "Padre",
        "s08_p01_3_1a" = "Hijas/os",
        "s08_p01_4_1a" = "Pareja",
        "s08_p01_5_1a" = "Hermanas/os",
        "s08_p01_6_1a" = "Otros familiares",
        "s08_p01_7_1a" = "Amigas/os",
        "s08_p01_8_1a" = "Compañeras/os de estudio/trabajo",
        "s08_p01_9_1a" = "Personas de organizaciones LGBTI+"
)

# 3. Procesar tabla con 1 decimal
tabla_decimal <- base_lgbti %>%
        select(fexp, all_of(names(mapeo_columnas))) %>%
        pivot_longer(cols = -fexp, names_to = "Grupo", values_to = "Nivel_Aceptacion") %>%
        filter(!is.na(Nivel_Aceptacion), !(Nivel_Aceptacion %in% c("9999", "No aplica"))) %>%
        # Suma expandida
        group_by(Grupo, Nivel_Aceptacion) %>%
        summarise(poblacion_estimada = sum(fexp, na.rm = TRUE), .groups = 'drop') %>%
        # Cálculo del porcentaje con 1 decimal por columna
        group_by(Grupo) %>%
        mutate(
                porcentaje_raw = (poblacion_estimada / sum(poblacion_estimada)) * 100,
                porcentaje_final = rds2_round_1d(porcentaje_raw)
        ) %>%
        select(Grupo, Nivel_Aceptacion, porcentaje_final) %>%
        mutate(Grupo = mapeo_columnas[Grupo]) %>%
        pivot_wider(names_from = Grupo, values_from = porcentaje_final)

# 4. Ver resultado
print(tabla_decimal)

library(openxlsx)
# 1. Crear el libro
wb <- createWorkbook()
# 2. Hoja de Convivencia
addWorksheet(wb, "dis")
writeData(wb, sheet = "dis", x = tabla_decimal)
# 4. Guardar
saveWorkbook(wb, "Resultados_RDS_LGBTI.xlsx", overwrite = TRUE)
print("¡Archivo de Excel generado con éxito con ambas hojas!")







































