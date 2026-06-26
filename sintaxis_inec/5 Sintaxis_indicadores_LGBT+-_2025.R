
##Sintaxis Indicadores de la Encuesta Nacional de Condiciones de Vida de la Población LGBTI+
  
  #=======================================================================#
  #Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más según tipo de convivencia
  
  #Operación Estadística:
  #Encuesta Nacional de Condiciones de Vida de la Población LGBTI+
  
  #Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
  #Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)
  
  #Fecha de elaboración: Marzo 2026*/
  
  # Versión sintaxis: 1.0
  # Software: R 4.5.2
  
  
  # Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)


# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 


############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ----------------------------------------------------------

# Definir el denominador (Total de personas LGBTI+ de 18+)

TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo de los indicadores por categoría
Tipo_de_convivencia <- base_lgbti %>%
  select(network.size.variable,s01_p01a:s01_p01h) %>% 
  
  pivot_longer(cols = s01_p01a:s01_p01h, 
               names_to = "codigo_variable", 
               values_to = "respuesta") %>%
  
  group_by(codigo_variable) %>%
  summarise(
    # Numerador (Pconvx) 
    Pconvx = sum( tolower(respuesta) == "sí", na.rm = TRUE),
    Pconvx.fexp = sum( 1/network.size.variable[tolower(respuesta) == "sí"], na.rm = TRUE),
    
    # Denominador: Total de encuestados
    Total_Encuestados = TP,
    Total_Encuestados.fexp = Pconvx.fexp,
    
    V_Pconvx = (Pconvx / TP) * 100,
    V_Pconvx.fexp = (Pconvx.fexp / TP.fexp) * 100
  ) %>% 
  
  #Asignar etiquetas descriptivas
  mutate(tipo_convivencia = case_when(
    codigo_variable == "s01_p01a" ~ "Pareja",
    codigo_variable == "s01_p01b" ~ "Amigas/os",
    codigo_variable == "s01_p01c" ~ "Esposa/o",
    codigo_variable == "s01_p01d" ~ "Hijas/os",
    codigo_variable == "s01_p01e" ~ "Padre y/o madre",
    codigo_variable == "s01_p01f" ~ "Otros familiares",
    codigo_variable == "s01_p01g" ~ "Otros no familiares",
    codigo_variable == "s01_p01h" ~ "Solo/a"
  )) %>% 
  
  select(tipo_convivencia, V_Pconvx, V_Pconvx.fexp) %>%
  arrange(desc(V_Pconvx))

# Ver resultado
print(Tipo_de_convivencia)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con acceso a agua por red pública, según como llega a la vivienda

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2

# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)


# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir el denominador (Total de personas en viviendas particulares)
base_particulares <- base_lgbti %>%
  filter(grepl("vivienda particular", tolower(s01_p02)))

TPVP <- nrow(base_particulares)
TPVP.fexp <- sum(1/base_lgbti[base_lgbti$s01_p02 == "Vivienda particular","network.size.variable"],na.rm = TRUE)

# Cálculo de los indicadores
Agua_red_publica <- base_particulares %>%
  mutate(es_red_publica = ifelse(tolower(s01_p07) %in% c("empresa pública/municipio", 
                                                         "juntas de agua/organizaciones comunitarias/gad parroquial"), 
                                 "Red Pública", "Otra")) %>%
  
  filter(tolower(s01_p06) %in% c("por tubería, dentro de la vivienda", 
                                 "por tubería, fuera de la vivienda, pero dentro del edificio, lote o terreno", 
                                 "por tubería fuera del edificio, lote o terreno")) %>%
  
  group_by(s01_p06) %>%
  summarise(
    PARPx = sum(es_red_publica == "Red Pública", na.rm = TRUE),
    PARPx.fexp = sum( 1/network.size.variable[es_red_publica == "Red Pública"], na.rm = TRUE),
    
    Total_Viviendas_Particulares = TPVP,
    Total_Viviendas_Particulares.fexp = TPVP.fexp,
    
    V_PARPx = (PARPx / TPVP) * 100,
    V_PARPx.fexp = (PARPx.fexp / TPVP.fexp) * 100
  ) %>%                
  select(s01_p06,V_PARPx,V_PARPx.fexp) %>% 
  
  arrange(desc(V_PARPx))

# Ver resultado 
print(Agua_red_publica)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con acceso a  eliminación de excretas adecuado

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir el denominador (Total de personas en viviendas particulares)
base_particulares <- base_lgbti %>%
  filter(tolower(s01_p02) == "vivienda particular")

TPVP <- nrow(base_particulares)
TPVP.fexp <- sum(1/base_lgbti[base_lgbti$s01_p02 == "Vivienda particular","network.size.variable"],na.rm = TRUE)

# Cálculo del indicador 
eliminacion_escretas_adecuado <- base_particulares %>%
  mutate(es_adecuado = ifelse(tolower(s01_p08) %in% c(
    "inodoro o escusado conectado a red pública de alcantarillado",
    "inodoro o escusado conectado a pozo séptico",
    "inodoro o escusado conectado a biodigestor"
  ), 1, 0)) %>%
  
  summarise(
    PEEA = sum(es_adecuado, na.rm = TRUE),
    PEEA.fexp = sum( 1/network.size.variable[es_adecuado == 1], na.rm = TRUE),
    
    Total_Viviendas_Particulares = TPVP,
    Total_Viviendas_Particulares.fexp = TPVP.fexp,
    
    V_PEEA = (PEEA / TPVP) * 100,
    V_PEEA.fexp = (PEEA.fexp / TPVP.fexp) * 100
  ) %>%                
  select( V_PEEA, V_PEEA.fexp)

# Ver resultado 
print(eliminacion_escretas_adecuado)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con acceso a red pública de alcantarillado

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2

# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir el denominador 
base_particulares <- base_lgbti %>%
  filter(tolower(s01_p02) == "vivienda particular")

TPVP <- nrow(base_particulares)
TPVP.fexp <- sum(1/base_lgbti[base_lgbti$s01_p02 == "Vivienda particular","network.size.variable"],na.rm = TRUE)

# Cálculo del indicador PRPA
red_publica_alcantarillado <- base_particulares %>%
  
  summarise(
    PRPA = sum(s01_p08 == "Inodoro o escusado conectado a red pública de alcantarillado", na.rm = TRUE),
    PRPA.fexp = sum( 1/network.size.variable[s01_p08 == "Inodoro o escusado conectado a red pública de alcantarillado"], na.rm = TRUE),
    
    Total_Viviendas_Particulares = TPVP,
    Total_Viviendas_Particulares.fexp = TPVP.fexp,
    
    V_PRPA = (PRPA / TPVP) * 100,
    V_PRPA.fexp = (PRPA.fexp / TPVP.fexp) * 100
  ) %>%                
  select(  V_PRPA, V_PRPA.fexp)

# Ver Resultado
print(red_publica_alcantarillado)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más según la relación entre el número de habitantes y los dormitorios exclusivos para dormir

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(janitor) # round_half_up permite el redondeo aritmético



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Procesamiento de datos y cálculo 
indicador_habi_dormitorio <- base_lgbti %>%
  filter(tolower(s01_p02) == "vivienda particular") %>%
  
  mutate(
    # Imputación: Si s02_p03 es 0, lo tratamos como 1
    dormitorios_imp = ifelse(s02_p03 == 0, 1, s02_p03),
    
    indice_bruto = s02_p01 / dormitorios_imp,
    indice_redondeado = round_half_up(indice_bruto, 0),
    
    Personas_por_dormitorio = ifelse(indice_redondeado >= 3, 
                                     "Más de 3 personas por dormitorio", 
                                     "Menos de 3 personas por dormitorio")
  )

TPVP <- nrow(indicador_habi_dormitorio)
TPVP.fexp <- sum(1/indicador_habi_dormitorio$network.size.variable)

tabla_final <- indicador_habi_dormitorio %>%
  group_by(Personas_por_dormitorio) %>%
  summarise(
    PHDx = n(),
    PHDx.fexp = sum(1/network.size.variable),
    
    TPVP = TPVP,
    TPVP.fexp = TPVP.fexp,
    
    V_PHDx = (PHDx / TPVP) * 100,
    V_PHDx.fexp = (PHDx.fexp / TPVP.fexp) * 100
  ) %>% 
  select(Personas_por_dormitorio,V_PHDx,V_PHDx.fexp)

# Ver resultado
print(tabla_final)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con acceso a internet en el hogar

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir universo y denominador (TPVP)
base_particulares <- base_lgbti %>%
  filter(tolower(s01_p02) == "vivienda particular")

TPVP <- nrow(base_particulares)
TPVP.fexp <- sum(1/base_particulares$network.size.variable,na.rm = TRUE)

# Cálculo del indicador
indicador_internet <- base_particulares %>%
  summarise(
    
    PAI = sum(tolower(s02_p04) == "sí", na.rm = TRUE),
    PAI.fexp = sum(1/network.size.variable[tolower(s02_p04) == "sí"], na.rm = TRUE),
    
    TPVP = TPVP,
    TPVP.fexp = TPVP.fexp,
    
    H_PAI = (PAI / TPVP) * 100,
    H_PAI.fexp = (PAI.fexp / TPVP.fexp) * 100
  ) %>%                
  select( H_PAI, H_PAI.fexp)

# Ver resultado
print(indicador_internet)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que dispone de computadora y/o tablet en el hogar

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir universo y denominador (TPVP)
base_particulares <- base_lgbti %>%
  filter(tolower(s01_p02) == "vivienda particular")

TPVP <- nrow(base_particulares)
TPVP.fexp <- sum(1/base_particulares$network.size.variable,na.rm = TRUE)

# Cálculo  
indicador_tecnologia <- base_particulares %>%
  mutate(
    tiene_equipo = ifelse(tolower(s02_p05a) == "sí" | tolower(s02_p05b) == "sí", 1, 0)
  ) %>%
  summarise(
    PDCT = sum(tiene_equipo, na.rm = TRUE),
    PDCT.fexp = sum(1/network.size.variable[tiene_equipo == 1], na.rm = TRUE),
    
    TPVP = TPVP,
    TPVP.fexp = TPVP.fexp,
    
    H_PDCT = (PDCT / TPVP) * 100,
    H_PDCT.fexp = (PDCT.fexp / TPVP.fexp) * 100
  ) %>%                
  select( H_PDCT,H_PDCT.fexp)

# Ver resultado
print(indicador_tecnologia)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con teléfono inteligente (smartphone) activado

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir denominador (Total de encuestados TP)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo
indicador_smartphone_total <- base_lgbti %>%
  summarise(
    
    PSPA = sum(tolower(s03_p13) == "sí", na.rm = TRUE),
    PSPA.fexp = sum(1/network.size.variable[tolower(s03_p13) == "sí"], na.rm = TRUE),
    
    TP = TP,
    TP.fexp = TP.fexp,
    
    V_PSPA = (PSPA / TP) * 100,
    V_PSPA.fexp = (PSPA.fexp / TP.fexp) * 100
  ) %>%                
  select(  V_PSPA,V_PSPA.fexp)

# Ver resultado 
print(indicador_smartphone_total)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más según aplicaciones y funciones  utilizadas en su teléfono inteligente (Smartphone)

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir denominador
base_smartphone <- base_lgbti %>%
  filter(tolower(s03_p13) == "sí")

TPSPA <- nrow(base_smartphone)
TPSPA.fexp <- sum(1/base_smartphone$network.size.variable)

# Cálculo independiente para cada categoría de uso
indicador_uso_apps <- base_smartphone %>%
  
  select(network.size.variable,s03_p14a:s03_p14h) %>% 
  
  pivot_longer(cols = s03_p14a:s03_p14h, 
               names_to = "codigo_variable", 
               values_to = "respuesta") %>%
  
  # Agrupación y cálculo por cada categoría
  group_by(codigo_variable) %>%
  summarise(
    # Numerador (PUSPAx)
    PUSPAx = sum(tolower(respuesta) == "sí", na.rm = TRUE),
    PUSPAx.fexp = sum(1/network.size.variable[tolower(respuesta) == "sí"], na.rm = TRUE)
    
  ) %>%
  
  # Asignar etiquetas descriptivas y cálculo final
  mutate(
    aplicaciones_funciones = case_when(
      codigo_variable == "s03_p14a" ~ "Datos móviles",
      codigo_variable == "s03_p14b" ~ "Redes sociales",
      codigo_variable == "s03_p14c" ~ "Bluetooth",
      codigo_variable == "s03_p14d" ~ "Correo electrónico",
      codigo_variable == "s03_p14e" ~ "Google maps/GPS",
      codigo_variable == "s03_p14f" ~ "Descarga de juegos/música",
      codigo_variable == "s03_p14g" ~ "Cámara",
      codigo_variable == "s03_p14h" ~ "Video conferencia"
    ),                
    
    CS_PUSPAx = (PUSPAx / TPSPA) * 100,
    CS_PUSPAx.fexp = (PUSPAx.fexp / TPSPA.fexp) * 100
  ) %>%                
  
  select(aplicaciones_funciones, CS_PUSPAx, CS_PUSPAx.fexp) %>%
  arrange(desc(CS_PUSPAx))

# Ver resultado
print(indicador_uso_apps)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más según medios utilizados para informarse

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir denominador (Total de encuestados TP)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo de indicadores agrupados
indicador_medios_agrupado <- base_lgbti %>%
  summarise(
    
    # a. Medios tradicionales
    `Medios tradicionales.sin` = sum(
      tolower(s03_p15a) == "sí" | # Televisión
        tolower(s03_p15b) == "sí" | # Radio
        tolower(s03_p15c) == "sí",  # Diario/revistas
      na.rm = TRUE
    ),
    `Medios tradicionales.fexp` = sum(1/network.size.variable[tolower(s03_p15a) == "sí" | 
                                                                tolower(s03_p15b) == "sí" | 
                                                                tolower(s03_p15c) == "sí" ], na.rm = TRUE),
    
    # b. Redes sociales
    `Redes sociales.sin` = sum(
      tolower(s03_p15d) == "sí" | # Facebook
        tolower(s03_p15e) == "sí" | # Instagram
        tolower(s03_p15f) == "sí" | # Tik Tok
        tolower(s03_p15g) == "sí",  # Twitter/Red X
      na.rm = TRUE
    ),
    `Redes sociales.fexp` = sum(1/network.size.variable[tolower(s03_p15d) == "sí" | 
                                                          tolower(s03_p15e) == "sí" | 
                                                          tolower(s03_p15f) == "sí" | 
                                                          tolower(s03_p15g) == "sí"], na.rm = TRUE),
    
    # c. Comunicación directa
    `Comunicación directa.sin` = sum(
      tolower(s03_p15h) == "sí" | # WhatsApp
        tolower(s03_p15i) == "sí",  # Telegram
      na.rm = TRUE
    ),
    `Comunicación directa.fexp` = sum(1/network.size.variable[tolower(s03_p15h) == "sí" | 
                                                                tolower(s03_p15i) == "sí"], na.rm = TRUE),
    
    # d. Inteligencia artificial
    `Inteligencia artificial.sin` = sum(
      tolower(s03_p15j) == "sí",
      na.rm = TRUE
    ),
    `Inteligencia artificial.fexp` = sum(1/network.size.variable[tolower(s03_p15j) == "sí"], na.rm = TRUE)
  ) %>% 
  
  pivot_longer(cols = everything(), names_to = c("Categoria", ".value"), names_sep = "\\.") %>% 
  
  mutate(
    Denominador = TP,
    Denominador.fexp = TP.fexp,
    
    Porcentaje = (sin / Denominador) * 100,
    Porcentaje.fexp = (fexp / Denominador.fexp) * 100
  ) %>%                
  
  arrange(desc(Porcentaje)) %>% 
  select(Categoria,Porcentaje, Porcentaje.fexp)

# Ver resultado
print(indicador_medios_agrupado)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con número de cédula ecuatoriana

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir denominador (Total de encuestados TP)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo 
indicador_cedula <- base_lgbti %>%
  summarise(
    
    PNCE = sum(tolower(s03_p03) == "sí", na.rm = TRUE),
    PNCE.fexp = sum( 1/network.size.variable[tolower(s03_p03) == "sí"], na.rm = TRUE),
    
    TP = TP,
    TP.fexp = TP.fexp,
    
    CS_PNCE = (PNCE / TP) * 100,
    CS_PNCE.fexp = (PNCE.fexp / TP.fexp) * 100
  ) %>%                
  select( CS_PNCE,CS_PNCE.fexp)

# Ver resultado
print(indicador_cedula)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más afiliada o cubierta por un seguro

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir denominador (Total de encuestados TP)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo del indicador (PTACSx)
indicador_seguros <- base_lgbti %>%
  mutate(
    tiene_seguro = ifelse(tolower(s03_p06a) == "sí" | tolower(s03_p06b) == "sí", 1, 0)
  ) %>%
  summarise(
    # Numerador (PTACSx): Suma de individuos con seguro
    PTACSx = sum(tiene_seguro, na.rm = TRUE),
    PTACSx.fexp = sum(1/network.size.variable[tiene_seguro == 1], na.rm = TRUE),
    
    TP = TP,
    TP.fexp = TP.fexp,
    
    CS_PTACSx = (PTACSx / TP) * 100,
    CS_PTACSx.fexp = (PTACSx.fexp / TP.fexp) * 100
  ) %>%                
  select(CS_PTACSx,CS_PTACSx.fexp)

# Ver resultado
print(indicador_seguros)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más con alguna discapacidad calificada por el Ministerio de Salud Pública (MSP)

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definir denominador (Total de encuestados TP)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo 
indicador_discapacidad <- base_lgbti %>%
  summarise(
    # Numerador (PDCMSP)
    PDCMSP = sum(tolower(s03_p08) == "sí", na.rm = TRUE),
    PDCMSP.fexp = sum(1/network.size.variable[tolower(s03_p08) == "sí"], na.rm = TRUE),
    
    TP = TP,
    TP.fexp = TP.fexp,
    
    CS_PDCMSP = (PDCMSP / TP) * 100,
    CS_PDCMSP.fexp = (PDCMSP.fexp / TP.fexp) * 100
  ) %>%                
  select(CS_PDCMSP,CS_PDCMSP.fexp)

# Ver resultado
print(indicador_discapacidad)

#=======================================================================#
#Nombre del indicador: Años promedio de escolaridad de la población LGBTI+ de 24 años y más

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ----------------------------------------------------------

# Preparar y filtrar la base
base_estudio <- base_lgbti %>%
  mutate(
    s03_p02  = as.numeric(s03_p02),
    s04_p03k = as.numeric(s04_p03k),
    
    nivel_educativo = trimws(tolower(s04_p03))
  ) %>%
  filter(s03_p02 >= 24 & s03_p02 <= 98)

# Cálculo de los años de instrucción (Nombre estandarizado: anos_inst)
base_estudio <- base_estudio %>%
  mutate(anos_inst = case_when(
    nivel_educativo == "ninguno" ~ 0,
    nivel_educativo == "centro de alfabetización" ~ case_when(
      s04_p03k == 0  ~ 0,
      s04_p03k == 1  ~ 2,
      s04_p03k == 2  ~ 4,
      s04_p03k == 3  ~ 6,
      s04_p03k == 4  ~ 7,
      s04_p03k == 5  ~ 8,
      s04_p03k == 6  ~ 9,
      s04_p03k == 7  ~ 10,
      s04_p03k == 8  ~ 11,
      s04_p03k == 9  ~ 12,
      s04_p03k == 10 ~ 13,
      TRUE ~ 0
    ),
    nivel_educativo == "jardín de infantes/preescolar" ~ 1,
    nivel_educativo == "primaria" ~ 1 + s04_p03k,
    nivel_educativo == "educación general básica" ~ s04_p03k,
    nivel_educativo == "secundaria" ~ 7 + s04_p03k,
    nivel_educativo == "educación media/bachillerato" ~ 10 + s04_p03k,
    nivel_educativo == "superior no universitario" ~ 13 + s04_p03k,
    nivel_educativo == "superior universitario" ~ 13 + s04_p03k,
    nivel_educativo == "posgrado" ~ 18 + s04_p03k,
    TRUE ~ NA_real_
  ))

resultado_general <- base_estudio %>%
  summarise(
    Años_Promedio_Escolaridad = mean(anos_inst, na.rm = TRUE),
    Años_Promedio_Escolaridad.fexp = sum(anos_inst * 1/network.size.variable, na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE),
    
    Total_Encuestados = n(),
    Total_Poblacion.fexp = sum(network.size.variable, na.rm = TRUE)
  )

# Ver resultado
print(resultado_general)

#=======================================================================#
#Nombre del indicador: Tasa de analfabetismo de la población LGBTI+ de 18 años y más

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

base_lgbti <- base_lgbti %>%
  mutate(
    s03_p02  = as.numeric(s03_p02)) %>%
  filter(s03_p02 >= 18 & s03_p02 <= 98)

# Definir denominador (Total de encuestados de 18 años y más)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable)

# Cálculo 
indicador_analfabetismo <- base_lgbti %>%
  mutate(
    es_analfabeto = case_when(
      # Caso 1: Respondieron la pregunta y dijeron que NO
      tolower(s04_p04) == "no" ~ 1,                
      
      # Caso 2: Respondieron la pregunta y dijeron que SÍ
      tolower(s04_p04) == "sí" | tolower(s04_p04) == "si" ~ 0,
      
      # Caso 3: Salto lógico (NA en s04_p04), asumimos alfabetismo
      # para personas con niveles educativos superiores.
      TRUE ~ 0
    )
  ) %>%
  summarise(
    PA = sum(es_analfabeto, na.rm = TRUE),
    PA.fexp = sum(1/network.size.variable[es_analfabeto == 1], na.rm = TRUE),
    
    TP = TP,
    TP.fexp = TP.fexp,
    
    E_PA = (PA / TP) * 100,
    E_PA.fexp = (PA.fexp / TP.fexp) * 100
  ) %>%                
  select( E_PA,E_PA.fexp)

# Ver resultado
print(indicador_analfabetismo)

#=======================================================================#
#Nombre del indicador: Tasa global de participación en la fuerza de trabajo de la población LGBTI+ de 18 años y más

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(haven) # Permite leer y escribir archivos de datos provenientes de SPSS (.sav), Stata (.dta) y SAS (.sas7bdat)     
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

setDT(base_lgbti)

# Población en edad de trabajar (PET) 

base_lgbti[, PET := 0]
base_lgbti[s03_p02 >= 18 & s03_p02 <= 98, PET := 1]

# Inicialización de variables de mercado laboral
cols_lab <- c("E_CIET13", "DE_CIET13", "AUT", "AUT_SBD", "AUT_NBD", "O_CIET19", "DO_CIET19", "FT_CIET19")
base_lgbti[, (cols_lab) := 0]

# CIET-13 
categorias_empleo <- c(
  "trabajo al menos una hora para generar un ingreso",
  "realizo algun trabajo ocasional (cachueloo chaucha) por un pago",
  "atendio un negocio propio",
  "ayudo en algun negocio o empleo de algun miembro de su hogar",
  "no trabajo, pero si tiene un trabajo al que seguro va a volver",
  "hizo o ayudo en labores agricolas, cria de animales o pesca"
)

norm_l <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

# Identificación de Empleados (E_CIET13)
base_lgbti[PET == 1 & (
  s05_p01 %in% 1:6 | 
    as.character(s05_p01) %in% as.character(1:6) |
    norm_l(s05_p01) %in% norm_l(categorias_empleo)
), E_CIET13 := 1]

# Identificación de Desempleados (DE_CIET13)
base_lgbti[PET == 1 & 
             (s05_p01 == 7 | norm_l(s05_p01) == "no trabajo") & 
             (s05_p04 == 1 | norm_l(s05_p04) == "si"), 
           DE_CIET13 := 1]

# Autoconsumo (AUT)

base_lgbti[PET == 1 & (
  ((s05_p01 %in% 2:5 | grepl("ocasional|negocio|ayudo|volvera", norm_l(s05_p01))) & 
     (s05_p02 == 1 | norm_l(s05_p02) == "si") & 
     (s05_p03 %in% 3:4 | grepl("consumo del hogar", norm_l(s05_p03)))) | 
    
    ((s05_p01 == 6 | grepl("agricola|pesca", norm_l(s05_p01))) & 
       (s05_p03 %in% 3:4 | grepl("consumo del hogar", norm_l(s05_p03))))
), AUT := 1]

# Clasificación por búsqueda (SBD vs NBD)
base_lgbti[AUT == 1 & (s05_p04 == 1 | norm_l(s05_p04) == "si"), AUT_SBD := 1]
base_lgbti[AUT == 1 & (s05_p04 == 2 | norm_l(s05_p04) == "no"), AUT_NBD := 1]

# Fuerza de trabajo
# O_CIET19  = E_CIET13 - AUT
# DO_CIET19 = DE_CIET13 + AUT_SBD
# FT_CIET19 = E_CIET13 + DE_CIET13 - AUT_NBD

# A. Ocupados
base_lgbti[E_CIET13 == 1 & AUT == 0, O_CIET19 := 1]

# B. Desocupados
base_lgbti[DE_CIET13 == 1 | AUT_SBD == 1, DO_CIET19 := 1]

# C. Fuerza de Trabajo
base_lgbti[O_CIET19 == 1 | DO_CIET19 == 1, FT_CIET19 := 1]

# Cálculo
base_lgbti[, fexp := 1 / network.size.variable]

resultado_final <- base_lgbti[PET == 1, .(
  # A. Totales Crudos (Sin ponderar)
  Num_FT_crudo = sum(FT_CIET19, na.rm = TRUE),
  Den_PET_crudo = sum(PET, na.rm = TRUE),
  Tasa_cruda     = (sum(FT_CIET19, na.rm = TRUE) / sum(PET, na.rm = TRUE)) * 100,
  
  # B. Totales Ponderados (Con factor de expansión)
  Num_FT_fexp    = sum(FT_CIET19 * fexp, na.rm = TRUE),
  Den_PET_fexp   = sum(PET * fexp, na.rm = TRUE),
  Tasa_fexp      = (sum(FT_CIET19 * fexp, na.rm = TRUE) / sum(PET * fexp, na.rm = TRUE)) * 100
)]


# Ver Resultado

cat("\n--- RESULTADOS CIET-19 (SIN PONDERAR) ---\n")
print(paste("Numerador (Fuerza Trabajo):", resultado_final$Num_FT_crudo))
print(paste("Denominador (PET):", resultado_final$Den_PET_crudo))
print(paste0("Tasa Global de Participación: ", resultado_final$Tasa_cruda, "%"))

cat("\n--- RESULTADOS CIET-19 (CON FACTOR DE EXPANSIÓN) ---\n")
print(paste("Numerador Ponderado (FT x Fexp):", resultado_final$Num_FT_fexp))
print(paste("Denominador Ponderado (PET x Fexp):", resultado_final$Den_PET_fexp))
print(paste0("Tasa Global de Participación Ponderada: ", resultado_final$Tasa_fexp, "%"))

#=======================================================================#
#Nombre del indicador: Ingreso promedio mensual de la población ocupada LGBTI+ de 18 años y más

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

setDT(base_lgbti)

# Definir Ocupados (Denominador TPO) 
base_lgbti[s03_p02 >= 18 & s03_p02 <= 98, PET := 1]

cols_lab <- c("E_CIET13", "AUT", "O_CIET19", "TPO")
base_lgbti[, (cols_lab) := 0]

# CIET-13 (Población empleada Categorías 1 a 6)
categorias_empleo <- c(
  "trabajo al menos una hora para generar un ingreso",
  "realizo algun trabajo ocasional (cachueloo chaucha) por un pago",
  "atendio un negocio propio",
  "ayudo en algun negocio o empleo de algun miembro de su hogar",
  "no trabajo, pero si tiene un trabajo al que seguro va a volver",
  "hizo o ayudo en labores agricolas, cria de animales o pesca"
)

# Función de normalización
norm_l <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

base_lgbti[PET == 1 & (
  s05_p01 %in% 1:6 | 
    as.character(s05_p01) %in% as.character(1:6) |
    norm_l(s05_p01) %in% norm_l(categorias_empleo)
), E_CIET13 := 1]

# Autoconsumo (AUT) 
base_lgbti[PET == 1 & (
  ((s05_p01 %in% 2:5 | grepl("ocasional|negocio|ayudo|volvera", norm_l(s05_p01))) & 
     (s05_p02 == 1 | norm_l(s05_p02) == "si") & 
     (s05_p03 %in% 3:4 | grepl("consumo del hogar", norm_l(s05_p03)))) | 
    
    ((s05_p01 == 6 | grepl("agricola|pesca", norm_l(s05_p01))) & 
       (s05_p03 %in% 3:4 | grepl("consumo del hogar", norm_l(s05_p03))))
), AUT := 1]

# Poblaciones CIET-19 y Ocupados
base_lgbti[E_CIET13 == 1 & AUT == 0, O_CIET19 := 1]

# Carga y Limpieza de ingresos
cols_ingreso <- c("s05_p09", "s05_p10c", "s05_p11", "s05_p12", "s05_p13", "s05_p14c", "s05_p18")

for (col in cols_ingreso) {
  base_lgbti[, (col) := as.numeric(get(col))]
  base_lgbti[get(col) %in% c(999, 9999, 99999, 999999), (col) := NA]
}

# Cálculo de Componentes por Individuo (IP)
base_lgbti[, `:=` (
  I_NEGOCIO = (fcoalesce(s05_p09, 0) + fcoalesce(s05_p10c, 0)) - fcoalesce(s05_p11, 0),
  I_LABORAL_NETO = fcoalesce(s05_p12, 0) + fcoalesce(s05_p13, 0),
  I_OTROS = fcoalesce(s05_p14c, 0) + fcoalesce(s05_p18, 0)
)]

base_lgbti[, IP := I_NEGOCIO + I_LABORAL_NETO + I_OTROS]

# Cálculos Finales

# --- A. Sin Factor de Expansión ---
num_crudo  <- sum(base_lgbti$IP, na.rm = TRUE)
den_crudo  <- sum(base_lgbti$O_CIET19, na.rm = TRUE)
res_crudo  <- num_crudo / den_crudo

# --- B. Con Factor de Expansión (RDS) ---
base_lgbti[, fexp := 1 / network.size.variable]

num_fexp   <- sum(base_lgbti$IP * base_lgbti$fexp, na.rm = TRUE)
den_fexp   <- sum(base_lgbti$O_CIET19 * base_lgbti$fexp, na.rm = TRUE)
res_fexp   <- num_fexp / den_fexp

# Ver resultados 
print(paste("Numerador (Suma total de ingresos):", num_crudo))
print(paste("Denominador (Total de ocupados):", den_crudo))
print(paste("Resultado Final (Ingreso Promedio):", res_crudo))

print(paste("Numerador Ponderado (Ingresos x Fexp):", num_fexp))
print(paste("Denominador Ponderado (Ocupados x Fexp):", den_fexp))
print(paste("Resultado Final Ponderado:", res_fexp))

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que realiza/ó trabajo sexual

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ----------------------------------------------------------

Indicador_ts <- base_lgbti %>%
  mutate(
    trabajo_sexual_bin = if_else(tolower(trimws(as.character(s05_p15))) == "sí", 1, 0, missing = 0)
  ) %>%
  summarise(
    PTS = sum(trabajo_sexual_bin),
    PTS.fexp = sum(1/network.size.variable[trabajo_sexual_bin == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    CO_PTS = (PTS / TP) * 100,
    CO_PTS.fexp = (PTS.fexp / TP.fexp) * 100
  ) %>%
  select(CO_PTS,CO_PTS.fexp)

# Ver resultado
print(Indicador_ts)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que realiza/ó trabajo sexual, según el lugar/ámbito donde lo ejerce/ció

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Denominador
base_ts <- base_lgbti %>%
  filter(tolower(as.character(s05_p15)) == "sí")

denominador_tpts <- nrow(base_ts)
denominador_tpts.fexp <- sum(1/base_ts$network.size.variable, na.rm = TRUE)

# Cálculo
resultados_finales <- base_ts %>%
  select(network.size.variable,s05_p17a:s05_p17f) %>%                
  
  pivot_longer(cols = s05_p17a:s05_p17f, 
               names_to = "codigo_variable", 
               values_to = "respuesta") %>%
  
  filter(tolower(as.character(respuesta)) == "sí") %>%
  
  group_by(codigo_variable) %>%
  summarise(Conteos = n(), fexp = sum(1/network.size.variable, na.rm = TRUE)  ) %>%
  
  mutate(
    Lugar = case_when(
      codigo_variable == "s05_p17a" ~ "En su vivienda",
      codigo_variable == "s05_p17b" ~ "En la calle/ plaza",
      codigo_variable == "s05_p17c" ~ "Hotel/ hostal",
      codigo_variable == "s05_p17d" ~ "Centro de tolerancia",
      codigo_variable == "s05_p17e" ~ "De manera virtual/digital",
      codigo_variable == "s05_p17f" ~ "Otro",
      TRUE ~ "Otro"
    ),
    
    CO_PTSLx = (Conteos / denominador_tpts) * 100,
    CO_PTSLx.fexp = (fexp / denominador_tpts.fexp) * 100
  ) %>%                
  select(Lugar, CO_PTSLx, CO_PTSLx.fexp)

# Ver Resultado
print(resultados_finales)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más intersexual

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

indicador_intersex_final <- base_lgbti %>% 
  mutate(
    intersex_bin = if_else(tolower(trimws(as.character(s06_p02))) == "sí", 1, 0, missing = 0)
  ) %>%
  summarise(
    PI = sum(intersex_bin, na.rm = TRUE),
    PI.fexp = sum(1/network.size.variable[intersex_bin == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    DSG_PI = (PI / TP) * 100,
    DSG_PI.fexp = (PI.fexp / TP.fexp) * 100
  ) %>%
  select(DSG_PI,DSG_PI.fexp)

# Ver resultado
print(indicador_intersex_final)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ intersexual de 18 años y más, según la intervención que recibió para las variaciones sexuales

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Filtrar solo la población intersexual (Denominador TPI)
base_intersex <- base_lgbti %>%
  filter(tolower(trimws(as.character(s06_p02))) == "sí")

denominador_tpi <- nrow(base_intersex)
denominador_tpi.fexp <- sum(1/base_intersex$network.size.variable, na.rm = TRUE)

# Cálculo de porcentajes por categoría
indicador_variaciones <- base_intersex %>%
  select(network.size.variable, s06_p02aa, s06_p02ab, s06_p02ac, s06_p02ad) %>%
  
  pivot_longer(cols = c(s06_p02aa, s06_p02ab, s06_p02ac, s06_p02ad), 
               names_to = "codigo_variable", 
               values_to = "respuesta") %>%
  
  group_by(codigo_variable) %>%
  summarise(
    PIVSx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PIVSx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE),
  ) %>%
  
  mutate(
    TPI = denominador_tpi,
    TPI.fexp = denominador_tpi.fexp,
    
    DSG_PIVSx = (PIVSx / TPI) * 100,
    DSG_PIVSx.fexp = (PIVSx.fexp / TPI.fexp) * 100,
    
    Categoria = case_when(
      codigo_variable == "s06_p02aa" ~ "Variación de genitales y órganos reproductivos",
      codigo_variable == "s06_p02ab" ~ "Variación de cromosomas y/o patrones hormonales",
      codigo_variable == "s06_p02ac" ~ "Variación de características del cuerpo",
      codigo_variable == "s06_p02ad" ~ "Otro",
      TRUE ~ "Sin especificar"
    )
  ) %>%
  select(Categoria, DSG_PIVSx, DSG_PIVSx.fexp)

# Ver resultado
print(indicador_variaciones)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ intersexual de 18 años y más, según la intervención que recibió para las variaciones sexuales.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Denominador TPI
base_inter_intervencion <- base_lgbti %>%
  filter(tolower(trimws(as.character(s06_p02))) == "sí")

tpi_denominador <- nrow(base_inter_intervencion)
tpi_denominador.fexp <- sum(1/base_inter_intervencion$network.size.variable, na.rm = TRUE)

indicador_intervenciones <- base_inter_intervencion %>%
  select(network.size.variable, s06_p02ca, s06_p02cb, s06_p02cc) %>%
  
  pivot_longer(cols = c(s06_p02ca, s06_p02cb, s06_p02cc), 
               names_to = "codigo_var", 
               values_to = "respuesta") %>%
  
  group_by(codigo_var) %>%
  summarise(
    PIIVSx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PIIVSx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE)
  ) %>%
  
  mutate(
    TPI = tpi_denominador,
    TPI.fexp = tpi_denominador.fexp,
    
    DSG_PIIVSx = (PIIVSx / TPI) * 100,
    DSG_PIIVSx.fexp = (PIIVSx.fexp / TPI.fexp) * 100,
    
    Categoria = case_when(
      codigo_var == "s06_p02ca" ~ "Cirugía de reasignación sexual",
      codigo_var == "s06_p02cb" ~ "Tratamiento hormonal",
      codigo_var == "s06_p02cc" ~ "Otro",
      TRUE ~ "Sin respuesta"
    )
  ) %>%
  select(Categoria,  DSG_PIIVSx, DSG_PIIVSx.fexp)

# Ver resultado
print(indicador_intervenciones)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ intersexual de 18 años y más, según quien decidió sobre la asignación del sexo.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ----------------------------------------------------------

# Denominador TPI
base_inter_decision <- base_lgbti %>%
  filter(tolower(trimws(as.character(s06_p02))) == "sí")

tpi_denominador <- nrow(base_inter_decision)
tpi_denominador.fexp <- sum(1/base_inter_decision$network.size.variable, na.rm = TRUE)

# Procesamiento de las categorías de decisión
indicador_decision <- base_inter_decision %>%
  select(network.size.variable, s06_p02da, s06_p02db, s06_p02dc, s06_p02dd) %>%
  
  pivot_longer(cols = c(s06_p02da, s06_p02db, s06_p02dc, s06_p02dd), 
               names_to = "codigo_var", 
               values_to = "respuesta") %>%
  
  group_by(codigo_var) %>%
  summarise(
    PIDASx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PIDASx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE)
  ) %>%
  
  # Cálculo del indicador y etiquetas
  mutate(
    TPI = tpi_denominador,
    TPI.fexp = tpi_denominador.fexp,
    
    DSG_PIDASx = (PIDASx / TPI) * 100,
    DSG_PIDASx.fexp = (PIDASx.fexp / TPI.fexp) * 100,
    
    Actor_Decisor = case_when(
      codigo_var == "s06_p02da" ~ "Padres / Familiares",
      codigo_var == "s06_p02db" ~ "Médicos",
      codigo_var == "s06_p02dc" ~ "Usted mismo",
      codigo_var == "s06_p02dd" ~ "Otro",
      TRUE ~ "Sin respuesta"
    )
  ) %>%
  select(Actor_Decisor, DSG_PIDASx, DSG_PIDASx.fexp)

# Ver resultado
print(indicador_decision)

#=======================================================================#
#Nombre del indicador: Edad promedio en la que la población LGBTI+ de 18 años y más empezó a sentir atracción afectiva, física y/o sexual.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Cálculo del Promedio con limpieza de datos
indicador_promedio_atraccion <- base_lgbti %>%
  mutate(s06_p03aa = as.numeric(s06_p03aa)) %>%
  filter(!is.na(s06_p03aa), s06_p03aa < 90) %>%
  
  summarise(
    Suma_Edades = sum(s06_p03aa),    
    Total_Casos = n(),              
    Promedio_Edad = mean(s06_p03aa),
    Promedio_Edad.fexp = sum(s06_p03aa * 1/network.size.variable, na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE),
    
    Desviacion_Estandar = sd(s06_p03aa) # Para medir dispersión
  ) %>%
  select( Promedio_Edad,Promedio_Edad.fexp)

# Ver resultado
print(indicador_promedio_atraccion)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tiene o tuvo hijos/as.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

indicador_hijos <- base_lgbti %>%
  mutate(
    tiene_hijos = if_else(tolower(trimws(as.character(s07_p01))) == "sí", 1, 0, missing = 0)
  ) %>%
  summarise(
    PH = sum(tiene_hijos, na.rm = TRUE),
    PH.fexp = sum(1/network.size.variable[tiene_hijos == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SR_PH = (PH / TP) * 100,
    SR_PH.fexp = (PH.fexp / TP.fexp) * 100
  ) %>%
  select(SR_PH,SR_PH.fexp)

# Ver resultado
print(indicador_hijos)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tiene o tuvo hijos/as, según la relación parental

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.

setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Denominador TPH
base_padres <- base_lgbti %>%
  filter(tolower(trimws(as.character(s07_p01))) == "sí")

tph_denominador <- nrow(base_padres)
tph_denominador.fexp <- sum(1/base_padres$network.size.variable, na.rm = TRUE)

# Cálculo de porcentajes por relación parental
indicador_parental <- base_padres %>%
  
  select(network.size.variable, s07_p02a, s07_p02b, s07_p02c, s07_p02d) %>%
  
  pivot_longer(cols = c(s07_p02a, s07_p02b, s07_p02c, s07_p02d), 
               names_to = "codigo_var", 
               values_to = "respuesta") %>%
  
  group_by(codigo_var) %>%
  summarise(
    PHRPx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PHRPx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE)
  ) %>%
  
  # Cálculo del indicador y etiquetas
  mutate(
    TPH = tph_denominador,
    TPH.fexp = tph_denominador.fexp,
    
    SR_PHRPx = (PHRPx / TPH) * 100,
    SR_PHRPx.fexp = (PHRPx.fexp / TPH.fexp) * 100,
    
    Relacion_Parental = case_when(
      codigo_var == "s07_p02a" ~ "Hijas/os biológicos",
      codigo_var == "s07_p02b" ~ "Hijas/os de su pareja",
      codigo_var == "s07_p02c" ~ "Hijas/os de crianza",
      codigo_var == "s07_p02d" ~ "Por adopción",
      TRUE ~ "Otro"
    )
  ) %>%
  select(Relacion_Parental,  SR_PHRPx, SR_PHRPx.fexp)

# Ver resultado
print(indicador_parental)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha considerado la posibilidad de ser padre o madre nuevamente o a futuro.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ---------------------------------------------------------

indicador_parentalidad_futura <- base_lgbti %>%
  mutate(
    considera_parentalidad = if_else(
      tolower(trimws(as.character(s07_p04))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PPMF = sum(considera_parentalidad, na.rm = TRUE), 
    PPMF.fexp = sum(1/network.size.variable[considera_parentalidad == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SR_PPMF = (PPMF / TP) * 100,
    SR_PPMF.fexp = (PPMF.fexp / TP.fexp) * 100
  ) %>%
  
  select(SR_PPMF,SR_PPMF.fexp)

# Ver resultado
print(indicador_parentalidad_futura)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que recibió información en los últimos doce meses sobre prevención de VIH

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

resultado_info_vih <- base_lgbti %>%
  mutate(
    recibio_info = if_else(
      tolower(trimws(as.character(s07_p09a))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PIP_VIH = sum(recibio_info, na.rm = TRUE),
    PIP_VIH.fexp = sum(1/network.size.variable[recibio_info == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PIP_VIH = (PIP_VIH / TP) * 100,
    SS_PIP_VIH.fexp = (PIP_VIH.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PIP_VIH,SS_PIP_VIH.fexp)

# Ver resultado
print(resultado_info_vih)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que recibió información en los últimos doce meses sobre prevención de infecciones de transmisión sexual (ITS)

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

resultado_info_its <- base_lgbti %>%
  mutate(
    recibio_info_its = if_else(
      tolower(trimws(as.character(s07_p09b))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PIP_ITS = sum(recibio_info_its, na.rm = TRUE),
    PIP_ITS.fexp = sum(1/network.size.variable[recibio_info_its == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PIP_ITS = (PIP_ITS / TP) * 100,
    SS_PIP_ITS.fexp = (PIP_ITS.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PIP_ITS,SS_PIP_ITS.fexp)

# Ver resultado
print(resultado_info_its)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que recibió información en los últimos doce meses sobre el Uso profilaxis pre y post exposición (PrEP/PEP) 

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 


############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

resultado_info_prep_pep <- base_lgbti %>%
  mutate(
    recibio_info_prep = if_else(
      tolower(trimws(as.character(s07_p09c))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PIPrEP_PEP = sum(recibio_info_prep, na.rm = TRUE),
    PIPrEP_PEP.fexp = sum(1/network.size.variable[recibio_info_prep == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PIPrEP_PEP = (PIPrEP_PEP / TP) * 100,
    SS_PIPrEP_PEP.fexp = (PIPrEP_PEP.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PIPrEP_PEP,SS_PIPrEP_PEP.fexp)

# Ver resultado
print(resultado_info_prep_pep)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más, según quienes le hablaron u orientaron sobre temas de sexualidad

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

resultado_fuentes_orientacion <- base_lgbti %>%
  
  select(network.size.variable, s07_p10a:s07_p10j) %>%
  mutate(across(s07_p10a:s07_p10j,as.character)) %>% 
  
  pivot_longer(cols = s07_p10a:s07_p10j, 
               names_to = "codigo_variable", 
               values_to = "respuesta") %>%
  
  mutate(respuesta_clean = tolower(trimws(respuesta))) %>% 
  
  group_by(codigo_variable) %>%
  
  summarise(
    PHOTSx = sum(respuesta_clean == "sí" | respuesta_clean == "si", na.rm = TRUE),
    PHOTSx.fexp = sum(1/network.size.variable[respuesta_clean == "sí" | respuesta_clean == "si"], na.rm = TRUE),
    
    TP = n(), # Denominador total de la base
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PHOTSx = (PHOTSx / TP) * 100,
    SS_PHOTSx.fexp = (PHOTSx.fexp / TP.fexp) * 100
  ) %>%
  
  # Añadimos las etiquetas de las categorías 
  mutate(Fuente = case_when(
    codigo_variable == "s07_p10a" ~ "Padre y/o madre",
    codigo_variable == "s07_p10b" ~ "Pareja, esposo/a",
    codigo_variable == "s07_p10c" ~ "Otros familiares",
    codigo_variable == "s07_p10d" ~ "Amigas/os",
    codigo_variable == "s07_p10e" ~ "Maestra/o u orientador escolar",
    codigo_variable == "s07_p10f" ~ "Personal de salud",
    codigo_variable == "s07_p10g" ~ "Activistas o colectivos LGBTI+",
    codigo_variable == "s07_p10h" ~ "Promotores de salud/comunitarios",
    codigo_variable == "s07_p10i" ~ "Buscó información por su cuenta",
    codigo_variable == "s07_p10j" ~ "Otro",
    TRUE ~ "Sin especificar"
  )) %>%
  
  select(Fuente, SS_PHOTSx, SS_PHOTSx.fexp)

# Ver resultado
print(resultado_fuentes_orientacion)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más según los métodos/ productos que utilizan para sus relaciones sexuales

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2

# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

resultado_metodos_sexuales <- base_lgbti %>%
  select(network.size.variable,s07_p11a:s07_p11e) %>%
  
  pivot_longer(cols = s07_p11a:s07_p11e, 
               names_to = "codigo_variable", 
               values_to = "respuesta") %>%
  
  # Agrupación y cálculo sobre el Denominador Total (TP)
  group_by(codigo_variable) %>%
  summarise(
    PMPUx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PMPUx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE),
    
    TP = nrow(base_lgbti),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PMPUx = (PMPUx / TP) * 100,
    SS_PMPUx.fexp = (PMPUx.fexp / TP.fexp) * 100
  ) %>%
  
  # Asignación de etiquetas para el reporte
  mutate(Metodo_Producto = case_when(
    codigo_variable == "s07_p11a" ~ "Condón femenino",
    codigo_variable == "s07_p11b" ~ "Condón masculino",
    codigo_variable == "s07_p11c" ~ "Condón de dedos / Barrera de látex",
    codigo_variable == "s07_p11d" ~ "Lubricantes",
    codigo_variable == "s07_p11e" ~ "Otro",
    TRUE ~ "Sin especificar"
  )) %>%
  
  select(Metodo_Producto, SS_PMPUx, SS_PMPUx.fexp)

# Ver resultado
print(resultado_metodos_sexuales)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que se ha realizado pruebas de detección de Infecciones de Transmisión Sexual (ITS) en los últimos 12 meses

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

resultado_pruebas_its <- base_lgbti %>%
  mutate(
    se_realizo_prueba = if_else(
      tolower(trimws(as.character(s07_p12))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PPD_ITS = sum(se_realizo_prueba, na.rm = TRUE),
    PPD_ITS.fexp = sum(1/network.size.variable[se_realizo_prueba == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PPD_ITS = (PPD_ITS / TP) * 100,
    SS_PPD_ITS.fexp = (PPD_ITS.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PPD_ITS,SS_PPD_ITS.fexp)

# Ver resultado
print(resultado_pruebas_its)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que se realizaron pruebas de detección de VIH en los últimos 12 meses

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

resultado_pruebas_vih <- base_lgbti %>%
  mutate(
    realizo_test_vih = if_else(
      tolower(trimws(as.character(s07_p13))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PPD_VIH = sum(realizo_test_vih, na.rm = TRUE),
    PPD_VIH.fexp = sum(1/network.size.variable[realizo_test_vih == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PPD_VIH = (PPD_VIH / TP) * 100,
    SS_PPD_VIH.fexp = (PPD_VIH.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PPD_VIH,SS_PPD_VIH.fexp)

# Ver resultado
print(resultado_pruebas_vih)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más, según las razones por las que no se realizaron pruebas para detectar Infecciones de Transmisión Sexual (ITS) y VIH.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------
# Definición del Denominador (Personas que NO se hicieron pruebas)
base_no_pruebas <- base_lgbti %>%
  filter(tolower(trimws(as.character(s07_p12))) == "no" | 
           tolower(trimws(as.character(s07_p13))) == "no")

TPNPD_ITSVIH <- nrow(base_no_pruebas)
TPNPD_ITSVIH.fexp <- sum(1/base_no_pruebas$network.size.variable, na.rm = TRUE)

# Cálculo 
resultado_final <- base_no_pruebas %>%
  select(network.size.variable, s07_p14a, s07_p14b, s07_p14c, s07_p14d, s07_p14e) %>%
  pivot_longer(cols = c(s07_p14a, s07_p14b, s07_p14c, s07_p14d, s07_p14e), 
               names_to = "cat", 
               values_to = "res") %>%
  
  group_by(cat) %>%
  summarise(
    PRNPD_ITSVIHx = sum(tolower(trimws(as.character(res))) == "sí", na.rm = TRUE),
    PRNPD_ITSVIHx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(res))) == "sí"], na.rm = TRUE),
    
    SS_PRNPD_ITSVIHx  = (PRNPD_ITSVIHx / TPNPD_ITSVIH) * 100,
    SS_PRNPD_ITSVIHx.fexp  = (PRNPD_ITSVIHx.fexp / TPNPD_ITSVIH.fexp) * 100
  ) %>%
  mutate(Razon = case_when(
    cat == "s07_p14a" ~ "Por vergüenza",
    cat == "s07_p14b" ~ "Por miedo",
    cat == "s07_p14c" ~ "Porque pensó que era normal",
    cat == "s07_p14d" ~ "Pensó que no servía para nada",
    cat == "s07_p14e" ~ "Otro",
    TRUE ~ NA_character_
  )) %>%
  select(Razon, SS_PRNPD_ITSVIHx, SS_PRNPD_ITSVIHx.fexp)

# Ver resultado
print(resultado_final)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que toma profilaxis pre exposición PREp para prevenir el VIH

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

resultado_uso_prep <- base_lgbti %>%
  mutate(
    toma_prep = if_else(
      tolower(trimws(as.character(s07_p15))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PT_PREp = sum(toma_prep, na.rm = TRUE),
    PT_PREp.fexp = sum(1/network.size.variable[toma_prep == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PT_PREp = (PT_PREp / TP) * 100,
    SS_PT_PREp.fexp = (PT_PREp.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PT_PREp,SS_PT_PREp.fexp)

# Ver resultado
print(resultado_uso_prep)
#=======================================================================#


#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que toma profilaxis post exposición PEP para prevenir el VIH

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ---------------------------------------------------------

resultado_uso_pep <- base_lgbti %>%
  mutate(
    toma_pep = if_else(
      tolower(trimws(as.character(s07_p16))) == "sí", 1, 0, missing = 0
    )
  ) %>%
  summarise(
    PT_PEP = sum(toma_pep, na.rm = TRUE),
    PT_PEP.fexp = sum(1/network.size.variable[toma_pep == 1], na.rm = TRUE),
    
    TP = n(),
    TP.fexp = sum(1/network.size.variable, na.rm = TRUE),
    
    SS_PT_PEP = (PT_PEP / TP) * 100,
    SS_PT_PEP.fexp = (PT_PEP.fexp / TP.fexp) * 100
  ) %>%
  select(SS_PT_PEP,SS_PT_PEP.fexp)

# Ver resultado
print(resultado_uso_pep)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más, según los métodos de detección temprana para prevención del cáncer cervicouterino y de mama realizados en los últimos 12 meses.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador 
base_mujeres_nacer <- base_lgbti %>%
  filter(tolower(trimws(as.character(s06_p01))) == "mujer")

TPMU <- nrow(base_mujeres_nacer)
TPMU.fexp <- sum(1/base_mujeres_nacer$network.size.variable, na.rm = TRUE)

# Cálculo de los indicadores de detección temprana
resultado_deteccion <- base_mujeres_nacer %>%
  select(network.size.variable, s07_p17a, s07_p17b, s07_p17c, s07_p17d, s07_p17e) %>%
  pivot_longer(cols = c(s07_p17a, s07_p17b, s07_p17c, s07_p17d, s07_p17e), 
               names_to = "codigo", 
               values_to = "respuesta") %>%
  group_by(codigo) %>%
  summarise(
    PMuMPCCMx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PMuMPCCMx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE),
    
    SS_PMuMPCCMx = (PMuMPCCMx / TPMU) * 100,
    SS_PMuMPCCMx.fexp = (PMuMPCCMx.fexp / TPMU.fexp) * 100
  ) %>%
  mutate(Metodo = case_when(
    codigo == "s07_p17a" ~ "Papanicolaou",
    codigo == "s07_p17b" ~ "Mamografía",
    codigo == "s07_p17c" ~ "Ecografía de pechos",
    codigo == "s07_p17d" ~ "Ecografía vaginal",
    codigo == "s07_p17e" ~ "Autoexploración mamaria",
    TRUE ~ NA_character_
  )) %>%
  select(Metodo, SS_PMuMPCCMx, SS_PMuMPCCMx.fexp)

# Ver resultado
print(resultado_deteccion)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población Trans de 18 años y más que ha usado o está usando hormonas como parte de su cambio de sexo

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador 
base_trans_proc <- base_lgbti %>%
  mutate(s06_p04 = str_trim(s06_p04)) %>% 
  filter(
    s06_p04 %in% c("Trans masculina", "Trans femenina", "Trans - no binaria"),
    s07_p18 %in% c("Hacia lo femenino", "Hacia lo masculino")
  )

TPTPCS <- nrow(base_trans_proc)
TPTPCS.fexp <- sum(1/base_trans_proc$network.size.variable, na.rm = TRUE)
print(paste("Denominador (TPTPCS):", TPTPCS))

# Cálculo del Numerador y el Indicador Final

resultado_final <- base_trans_proc %>%
  summarise(
    
    PTUHCS = sum(s07_p19 == "sí", na.rm = TRUE),
    
    SST_PTUHCS = (PTUHCS / TPTPCS) * 100,
    SST_PTUHCS.fexp = (sum(1/network.size.variable[s07_p19 == "sí"], na.rm = TRUE) / TPTPCS.fexp) * 100
  )

# Ver resultado
print(resultado_final)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población Trans de 18 años y más, según quién administra o administró las hormonas

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador ---------------------------------------------------------

# Definición del Denominador (Población Trans con procesos que usan hormonas)

universo_hormonizacion <- base_lgbti %>%
  mutate(s06_p04 = str_trim(s06_p04)) %>% 
  filter(
    s06_p04 %in% c("Trans masculina", "Trans femenina", "Trans - no binaria"),
    s07_p18 %in% c("Hacia lo femenino", "Hacia lo masculino"),
    s07_p19 %in% c("sí")
  )

TPTUHCS <- nrow(universo_hormonizacion)
TPTUHCS.fexp <- sum(1/universo_hormonizacion$network.size.variable, na.rm = TRUE)

# Cálculo 
admi_hormonas <- universo_hormonizacion %>%
  select(network.size.variable, s07_p20a, s07_p20b, s07_p20c, s07_p20d, s07_p20e, s07_p20f) %>%
  pivot_longer(cols = c(s07_p20a, s07_p20b, s07_p20c, s07_p20d, s07_p20e, s07_p20f), 
               names_to = "codigo",
               values_to = "respuesta") %>%
  group_by(codigo) %>%
  summarise(
    PTAHCSx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PTAHCSx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE),
    
    SST_PTAHCSx = (PTAHCSx / TPTUHCS) * 100,
    SST_PTAHCSx.fexp = (PTAHCSx.fexp / TPTUHCS.fexp) * 100
  ) %>%
  mutate(Administra = case_when(
    codigo == "s07_p20a" ~ "Médica/o",
    codigo == "s07_p20b" ~ "Enfermera/o",
    codigo == "s07_p20c" ~ "Farmacéutica/o",
    codigo == "s07_p20d" ~ "Usted mismo/a",
    codigo == "s07_p20e" ~ "Amiga/o",
    codigo == "s07_p20f" ~ "Otro",
    TRUE ~ NA_character_
  )) %>%
  
  mutate(SST_PTAHCSx = (SST_PTAHCSx)) %>%
  select(Administra, SST_PTAHCSx, SST_PTAHCSx.fexp)

# Ver resultado
print(admi_hormonas)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población Trans 18 años y más que se ha inyectado alguna sustancia como aceites, polímeros, etc.

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

# Definición del Universo Trans con Procedimientos (Denominador)

base_trans_proc <- base_lgbti %>%
  mutate(s06_p04 = str_trim(s06_p04)) %>% 
  filter(
    s06_p04 %in% c("Trans masculina", "Trans femenina", "Trans - no binaria"),
    s07_p18 %in% c("Hacia lo femenino", "Hacia lo masculino")
  )

TPTPCS <- nrow(base_trans_proc)
TPTPCS.fexp <- sum(1/base_trans_proc$network.size.variable, na.rm = TRUE)

# Cálculo 
indicador_sustancias <- base_trans_proc %>%
  summarise(
    PTIS = sum(tolower(trimws(as.character(s07_p21))) == "sí", na.rm = TRUE),
    PTIS.fexp = sum(1/network.size.variable[tolower(trimws(as.character(s07_p21))) == "sí"], na.rm = TRUE),
    
    Denominador = TPTPCS,
    #Denominador.fexp = TPTPCS.fexp,
    
    SST_PTIS = (PTIS / TPTPCS) * 100,
    SST_PTIS.fexp = (PTIS.fexp / TPTPCS.fexp) * 100
  )%>%
  select(SST_PTIS,SST_PTIS.fexp)

# Ver resultado
print(indicador_sustancias)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población Trans de 18 años y más, según quién le inyectó las sustancias

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

# Definición del Denominador (Universo: Trans que se inyectaron sustancias)
universo_sustancias <- base_lgbti %>%
  mutate(s06_p04 = str_trim(s06_p04)) %>% 
  filter(
    s06_p04 %in% c("Trans masculina", "Trans femenina", "Trans - no binaria"),
    s07_p18 %in% c("Hacia lo femenino", "Hacia lo masculino"),
    s07_p21 %in% c("sí")
  )

TPTIS <- nrow(universo_sustancias)
TPTIS.fexp <- sum(1/universo_sustancias$network.size.variable, na.rm = TRUE)

# Cálculo del indicador por categoría
quien_inyecto <- universo_sustancias %>%
  select(network.size.variable, s07_p22a, s07_p22b, s07_p22c, s07_p22d, s07_p22e, s07_p22f) %>%
  pivot_longer(cols = c(s07_p22a, s07_p22b, s07_p22c, s07_p22d, s07_p22e, s07_p22f), 
               names_to = "codigo", 
               values_to = "respuesta") %>%
  group_by(codigo) %>%
  summarise(
    PTPISx = sum(tolower(trimws(as.character(respuesta))) == "sí" , na.rm = TRUE),
    PTPISx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"] , na.rm = TRUE),
    
    SST_PTPISx = (PTPISx / TPTIS) * 100,
    SST_PTPISx.fexp = (PTPISx.fexp / TPTIS.fexp) * 100
    
  ) %>%
  mutate(Categoria = case_when(
    codigo == "s07_p22a" ~ "Médica/o",
    codigo == "s07_p22b" ~ "Enfermera/o",
    codigo == "s07_p22c" ~ "Cosmetóloga/o o esteticista",
    codigo == "s07_p22d" ~ "Usted misma/o",
    codigo == "s07_p22e" ~ "Amiga/o",
    codigo == "s07_p22f" ~ "Otro",
    TRUE ~ NA_character_
  )) %>%
  select(Categoria, SST_PTPISx, SST_PTPISx.fexp)

# Ver resultado
print(quien_inyecto)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población Trans de 18 años y más, según el lugar o establecimiento en que le suministraron las sustancias

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (Universo: Trans que se inyectaron sustancias)
universo_lugar <- base_lgbti %>%
  mutate(s06_p04 = str_trim(s06_p04)) %>% 
  filter(
    s06_p04 %in% c("Trans masculina", "Trans femenina", "Trans - no binaria"),
    s07_p18 %in% c("Hacia lo femenino", "Hacia lo masculino"),
    s07_p21 %in% c("sí")
  )

TPTIS <- nrow(universo_lugar)
TPTIS.fexp <- sum(1/universo_lugar$network.size.variable, na.rm = TRUE)

# Cálculo del indicador por categoría 
lugar_suministro <- universo_lugar %>%
  select(network.size.variable, s07_p23a, s07_p23b, s07_p23c, s07_p23d, s07_p23e, s07_p23f) %>%
  pivot_longer(cols = c(s07_p23a, s07_p23b, s07_p23c, s07_p23d, s07_p23e, s07_p23f), 
               names_to = "codigo", 
               values_to = "respuesta") %>%
  group_by(codigo) %>%
  summarise(
    PTLSSx = sum(tolower(trimws(as.character(respuesta))) == "sí", na.rm = TRUE),
    PTLSSx.fexp = sum(1/network.size.variable[tolower(trimws(as.character(respuesta))) == "sí"], na.rm = TRUE),
    
    SST_PTLSSx = (PTLSSx / TPTIS) * 100,
    SST_PTLSSx.fexp = (PTLSSx.fexp / TPTIS.fexp) * 100
  ) %>%
  mutate(Lugar = case_when(
    codigo == "s07_p23a" ~ "Hospitales/Clínicas/Privados",
    codigo == "s07_p23b" ~ "Farmacias",
    codigo == "s07_p23c" ~ "Centros estéticos/SPA",
    codigo == "s07_p23d" ~ "Peluquerías",
    codigo == "s07_p23e" ~ "Casas (Propia o amigos)",
    codigo == "s07_p23f" ~ "Otro",
    TRUE ~ NA_character_
  )) %>%
  select(Lugar, SST_PTLSSx, SST_PTLSSx.fexp)

# Ver resultado
print(lugar_suministro)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población Trans de 18 años y más que se ha realizado alguna cirugía como parte de su cambio de sexo

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

# Definición del Universo Trans con Procedimientos (Denominador)
base_trans_proc <- base_lgbti %>%
  mutate(s06_p04 = str_trim(s06_p04)) %>% 
  filter(
    s06_p04 %in% c("Trans masculina", "Trans femenina", "Trans - no binaria"),
    s07_p18 %in% c("Hacia lo femenino", "Hacia lo masculino")
  )
TPTPCS <- nrow(base_trans_proc)
TPTPCS.fexp <- sum(1/base_trans_proc$network.size.variable, na.rm = TRUE)

# Cálculo del Indicador de Cirugía
indicador_cirugia <- base_trans_proc %>%
  summarise(
    PTCCS = sum(tolower(trimws(as.character(s07_p24))) == "sí", na.rm = TRUE),
    PTCCS.fexp = sum(1/network.size.variable[tolower(trimws(as.character(s07_p24))) == "sí"], na.rm = TRUE),
    
    Total_Denominador = TPTPCS,
    #Total_Denominador = TPTPCS.fexp,
    
    SST_PTCCS = (PTCCS / TPTPCS) * 100,
    SST_PTCCS.fexp = (PTCCS.fexp / TPTPCS.fexp) * 100
  )%>%
  select(SST_PTCCS,SST_PTCCS.fexp)

# Ver resultado
print(indicador_cirugia)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tuvo experiencias de discriminación y/o violencia a lo largo de su vida

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(tidyselect) #seleccionar variables (columnas) basándose en sus nombres, tipos o patrones



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

# Definir el vector con los nombres exactos de las variables
vars_violencia <- paste0("s08_p02_", 1:18)

# Cálculo  
indicador_discriminación_violencia <- base_lgbti %>%
  mutate(es_victima = if_any(all_of(vars_violencia), 
                             ~ tolower(trimws(as.character(.))) == "sí" )) %>%
  summarise(
    PDVLV = sum(es_victima, na.rm = TRUE), 
    TP = n(),                             
    DV_PDVLV = (PDVLV / TP) * 100,
    DV_PDVLV.fexp = ( sum(1/network.size.variable[es_victima == TRUE], na.rm = TRUE)/ sum(1/network.size.variable, na.rm = TRUE) ) * 100,
  )

# Ver resultado
print(indicador_discriminación_violencia)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tuvo experiencias de discriminación y/o violencia en los últimos 12 meses

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(tidyselect) #seleccionar variables (columnas) basándose en sus nombres, tipos o patrones



# Importar base de datos ----------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ---------------------------------------------------------

base_lgbti = base_lgbti %>%
  mutate(
    violencia_total = if_else(
      rowSums(across(matches("^s08_p02_[0-9]+$"), ~ . == "sí"),
              na.rm = TRUE) > 0,
      "sí", "no"
    )
  ) %>% 
  
  mutate(
    violencia_actual = if_else(
      rowSums(
        across(matches("^s08_p02_([1-9]|1[0-8])_2f$"), ~ . == "sí"),
        na.rm = TRUE
      ) > 0,
      "sí",
      "no"
    )
  ) 

violencia_ultimo_año <- base_lgbti %>%
  summarise(
    Casos_Si = sum(violencia_actual == "sí", na.rm = TRUE),
    Total_Base = n(),
    Proporcion_Exacta = sum(violencia_actual == "sí", na.rm = TRUE) / n() * 100,
    Proporcion_Exacta.fexp = sum(1/network.size.variable[violencia_actual == "sí"], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE) * 100
  )

# Ver resultado
print(violencia_ultimo_año)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tuvo experiencias de discriminación y/o violencia a lo largo de su vida por tipo de violencia

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2

# Proceso-------------------------- 


# Descargar bases de datos -------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías ----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ---------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############




# Cálculo Indicador --------------------------------------------------------

# Definir el Universo de Víctimas (Denominador)
base_victimas <- base_lgbti %>%
  mutate(es_victima = if_any(paste0("s08_p02_", 1:18), 
                             ~ tolower(trimws(as.character(.))) == "sí" | . == 1)) %>%
  filter(es_victima == TRUE)

TPDVLV <- nrow(base_victimas)
TPDVLV.fexp <- sum(1/base_victimas$network.size.variable, na.rm = TRUE)

# Crear las Dimensiones de Violencia (Numeradores)
indicador_tipos <- base_victimas %>%
  summarise(
    Psicologica = sum(if_any(paste0("s08_p02_", 1:6), ~ tolower(trimws(as.character(.))) == "sí" | . == 1), na.rm = TRUE),
    Perjuicio   = sum(if_any(paste0("s08_p02_", c(7, 13, 14, 17)), ~ tolower(trimws(as.character(.))) == "sí" | . == 1), na.rm = TRUE),
    Cibernetica = sum(tolower(trimws(as.character(s08_p02_8))) == "sí" | s08_p02_8 == 1, na.rm = TRUE),
    Sexual      = sum(tolower(trimws(as.character(s08_p02_9))) == "sí" | s08_p02_9 == 1, na.rm = TRUE),
    Economica   = sum(tolower(trimws(as.character(s08_p02_10))) == "sí" | s08_p02_10 == 1, na.rm = TRUE),
    Fisica      = sum(if_any(paste0("s08_p02_", 11:12), ~ tolower(trimws(as.character(.))) == "sí" | . == 1) | if_any(paste0("s08_p02_", 15:16), ~ tolower(trimws(as.character(.))) == "sí" | . == 1), na.rm = TRUE),
    Gineco_Obs  = sum(tolower(trimws(as.character(s08_p02_18))) == "sí" | s08_p02_18 == 1, na.rm = TRUE),
    
    
    Psicologica.fexp = sum(1/network.size.variable[if_any(paste0("s08_p02_", 1:6), ~ tolower(trimws(as.character(.))) == "sí" | . == 1)], na.rm = TRUE),
    Perjuicio.fexp   = sum(1/network.size.variable[if_any(paste0("s08_p02_", c(7, 13, 14, 17)), ~ tolower(trimws(as.character(.))) == "sí" | . == 1)], na.rm = TRUE),
    Cibernetica.fexp = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_8))) == "sí" | s08_p02_8 == 1], na.rm = TRUE),
    Sexual.fexp      = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_9))) == "sí" | s08_p02_9 == 1], na.rm = TRUE),
    Economica.fexp   = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_10))) == "sí" | s08_p02_10 == 1], na.rm = TRUE),
    Fisica.fexp      = sum(1/network.size.variable[if_any(paste0("s08_p02_", 11:12), ~ tolower(trimws(as.character(.))) == "sí" | . == 1) | if_any(paste0("s08_p02_", 15:16), ~ tolower(trimws(as.character(.))) == "sí" | . == 1)], na.rm = TRUE),
    Gineco_Obs.fexp  = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_18))) == "sí" | s08_p02_18 == 1], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Tipo_Violencia", values_to = "PDVLVTVx") %>%
  mutate(
    TPDVLV = c(rep(TPDVLV,7),rep(TPDVLV.fexp,7)),
    SST_DV_PDVLVTVx = (PDVLVTVx / TPDVLV) * 100
  ) %>% 
  select(Tipo_Violencia, SST_DV_PDVLVTVx)

# Ver resultado
print(indicador_tipos)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tuvo experiencias de discriminación y/o violencia en los últimos 12 meses por tipo de violencia

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------
# Definir Universo de Víctimas Recientes (Denominador)
base_lgbti = base_lgbti %>%
  mutate(
    violencia_total = if_else(
      rowSums(across(matches("^s08_p02_[0-9]+$"), ~ . == "sí"),
              na.rm = TRUE) > 0,
      "sí", "no"
    )
  ) %>% 
  
  mutate(
    violencia_actual = if_else(
      rowSums(
        across(matches("^s08_p02_([1-9]|1[0-8])_2f$"), ~ . == "sí"),
        na.rm = TRUE
      ) > 0,
      "sí",
      "no"
    )
  ) 

denominador_violencia <- base_lgbti %>%
  summarise(total = sum(violencia_actual == "sí", na.rm = TRUE)) %>%
  pull(total)
denominador_violencia.fexp <- sum(1/base_lgbti$network.size.variable[base_lgbti$violencia_actual == "sí"], na.rm = TRUE)


# Cálculo de Indicadores por Tipo
indicador_tipos_12m <- base_lgbti %>%
  summarise(
    Psicologica = sum(if_any(paste0("s08_p02_", 1:6, "_2f"), ~ tolower(trimws(as.character(.))) == "sí"), na.rm = TRUE),
    Perjuicio   = sum(if_any(paste0("s08_p02_", c(7, 13, 14, 17), "_2f"), ~ tolower(trimws(as.character(.))) == "sí"), na.rm = TRUE),
    Cibernetica = sum(tolower(trimws(as.character(s08_p02_8_2f))) == "sí", na.rm = TRUE),
    Sexual      = sum(tolower(trimws(as.character(s08_p02_9_2f))) == "sí", na.rm = TRUE),
    Economica   = sum(tolower(trimws(as.character(s08_p02_10_2f))) == "sí", na.rm = TRUE),
    Fisica      = sum(if_any(paste0("s08_p02_", 11:12, "_2f"), ~ tolower(trimws(as.character(.))) == "sí") | if_any(paste0("s08_p02_", 15:16, "_2f"), ~ tolower(trimws(as.character(.))) == "sí"), na.rm = TRUE),
    Gineco_Obs  = sum(tolower(trimws(as.character(s08_p02_18_2f))) == "sí", na.rm = TRUE),
    
    Psicologica.fexp = sum(1/network.size.variable[if_any(paste0("s08_p02_", 1:6, "_2f"), ~ tolower(trimws(as.character(.))) == "sí")], na.rm = TRUE),
    Perjuicio.fexp   = sum(1/network.size.variable[if_any(paste0("s08_p02_", c(7, 13, 14, 17), "_2f"), ~ tolower(trimws(as.character(.))) == "sí")], na.rm = TRUE),
    Cibernetica.fexp = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_8_2f))) == "sí"], na.rm = TRUE),
    Sexual.fexp      = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_9_2f))) == "sí"], na.rm = TRUE),
    Economica.fexp   = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_10_2f))) == "sí"], na.rm = TRUE),
    Fisica.fexp      = sum(1/network.size.variable[if_any(paste0("s08_p02_", 11:12, "_2f"), ~ tolower(trimws(as.character(.))) == "sí") | if_any(paste0("s08_p02_", 15:16, "_2f"), ~ tolower(trimws(as.character(.))) == "sí")], na.rm = TRUE),
    Gineco_Obs.fexp  = sum(1/network.size.variable[tolower(trimws(as.character(s08_p02_18_2f))) == "sí"], na.rm = TRUE)
  ) %>%
  
  pivot_longer(cols = everything(), names_to = "Tipo_Violencia", values_to = "PDVUATVx") %>%
  
  mutate(
    denominador_violencia = c(rep(denominador_violencia,7),rep(denominador_violencia.fexp,7)),
    DV_PDVUATVx = (PDVUATVx / denominador_violencia) * 100
  ) %>% 
  select(Tipo_Violencia, DV_PDVUATVx)

# Ver resultado
print(indicador_tipos_12m)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que tuvo experiencias de discriminación y/o violencia a lo largo de su vida por ámbito de ocurrencia

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Diccionario de Mapeo
actores <- list(
  familiar  = c("Madre/padre", "Hermanas/os", "Pareja/expareja", "Otro familiar", "Familia de pareja/expareja", "1", "2", "3", "4", "5"),
  educativo = c("Compañeros/as", "Profesora/or o directora/or", "Personal administrativo o de servicios", "Conductora/or de transporte escolar", "Bienestar estudiantil/DOBE/DECE", "1", "2", "3", "4", "5"),
  laboral   = c("Compañeros/as", "Jefa/e o superior", "Personal administrativo, de limpieza", "Clientes", "Otras/os", "1", "2", "3", "4", "5"),
  publico   = c("Personal de salud, pacientes", "Policía", "FFAA", "Agente de tránsito / municipal", "Servidores públicos", "1", "2", "3", "4", "5"),
  social    = c("Amigas/os", "Vecinas/os", "Conocida/o ó desconocida/o", "Pastor, sacerdote o líder religioso", "Personal de transporte", "Líder político, líder comunitario o autoridad local", "Personal de seguirdad", "1", "2", "3", "4", "5", "6", "7")
)

validar_ambito <- function(columna, lista_referencia) {
  datos_limpios <- tolower(trimws(as.character(columna)))
  ref_limpia    <- tolower(trimws(as.character(lista_referencia)))
  datos_limpios %in% ref_limpia
}

# Definición del Denominador 
base_victimas <- base_lgbti %>%
  mutate(es_victima = if_any(paste0("s08_p02_", 1:18), 
                             ~ tolower(trimws(as.character(.))) == "sí" | . == 1)) %>%
  filter(es_victima == TRUE)

TPDVLV <- nrow(base_victimas)
TPDVLV.fexp <- sum(1/base_victimas$network.size.variable[base_victimas$es_victima == 1], na.rm = TRUE)

# Cálculo de Numeradores por Ámbito
indicadores_ambitos <- base_victimas %>%
  summarise(
    Familiar    = sum(if_any(matches("_2a$"), ~ validar_ambito(., actores$familiar)), na.rm = TRUE),
    Educativo   = sum(if_any(matches("_2b$"), ~ validar_ambito(., actores$educativo)), na.rm = TRUE),
    Laboral     = sum(if_any(matches("_2c$"), ~ validar_ambito(., actores$laboral)), na.rm = TRUE),
    Sector_Pub  = sum(if_any(matches("_2d$"), ~ validar_ambito(., actores$publico)), na.rm = TRUE),
    Social_Com  = sum(if_any(matches("_2e$"), ~ validar_ambito(., actores$social)), na.rm = TRUE),
    
    Familiar.fexp    = sum(1/network.size.variable[if_any(matches("_2a$"), ~ validar_ambito(., actores$familiar))], na.rm = TRUE),
    Educativo.fexp   = sum(1/network.size.variable[if_any(matches("_2b$"), ~ validar_ambito(., actores$educativo))], na.rm = TRUE),
    Laboral.fexp     = sum(1/network.size.variable[if_any(matches("_2c$"), ~ validar_ambito(., actores$laboral))], na.rm = TRUE),
    Sector_Pub.fexp  = sum(1/network.size.variable[if_any(matches("_2d$"), ~ validar_ambito(., actores$publico))], na.rm = TRUE),
    Social_Com.fexp  = sum(1/network.size.variable[if_any(matches("_2e$"), ~ validar_ambito(., actores$social))], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Ambito", values_to = "PDVLVAx") %>%
  mutate(
    
    TPDVLV = c(rep(TPDVLV,5),rep(TPDVLV.fexp,5)),
    DV_PDVLVAx = (PDVLVAx / TPDVLV) * 100
  ) %>% 
  select(Ambito, DV_PDVLVAx)

# Ver resultado
print(paste("Total víctimas (Denominador):", TPDVLV))
print(as.data.frame(indicadores_ambitos))

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los últimos 12 meses tuvo experiencias de discriminación y/o violencia y como  consecuencia ha sentido la necesidad de recibir atención psicológica

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPDVUA)
base_victimas_12m <- base_lgbti %>%
  mutate(es_victima_12m = if_any(paste0("s08_p02_", 1:18, "_2f"), 
                                 ~ tolower(trimws(as.character(.))) == "sí" | . == 1)) %>%
  filter(es_victima_12m == TRUE)

TPDVUA <- nrow(base_victimas_12m)
TPDVUA.fexp <- sum(1/base_victimas_12m$network.size.variable[base_victimas_12m$es_victima_12m == 1])

# Definición del Numerador (PDVUANAP)
PDVUANAP <- base_victimas_12m %>%
  filter(tolower(trimws(as.character(s08_p03))) == "sí" | s08_p03 == 1) %>%
  nrow()
PDVUANAP.fexp <- sum(1/base_victimas_12m$network.size.variable[tolower(trimws(as.character(base_victimas_12m$s08_p03))) == "sí" | base_victimas_12m$s08_p03 == 1], na.rm = TRUE)

# Cálculo  
DV_PDVUANAP = (PDVUANAP / TPDVUA) * 100
DV_PDVUANAP.fexp = (PDVUANAP.fexp / TPDVUA.fexp) * 100

# Ver resultado
print(paste("Indicador Final (%):", DV_PDVUANAP, "; fexp", DV_PDVUANAP.fexp))

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los ultimos 12 meses tuvo experiencias de discriminación y/o violencia y como  consecuencia ha recibido atención en salud mental

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador 
base_lgbti = base_lgbti %>%
  mutate(
    violencia_total = if_else(
      rowSums(across(matches("^s08_p02_[0-9]+$"), ~ . == "sí"),
              na.rm = TRUE) > 0,
      "sí", "no"
    )
  ) %>% 
  
  mutate(
    violencia_actual = if_else(
      rowSums(
        across(matches("^s08_p02_([1-9]|1[0-8])_2f$"), ~ . == "sí"),
        na.rm = TRUE
      ) > 0,
      "sí",
      "no"
    )
  ) 

denominador_violencia <- base_lgbti %>%
  summarise(total = sum(violencia_actual == "sí", na.rm = TRUE)) %>%
  pull(total)
denominador_violencia.fexp <- sum(1/base_lgbti$network.size.variable[base_lgbti$violencia_actual == "sí"], na.rm = TRUE)


# Definición del Numerador (PDVUARASM)
PDVUARASM <- base_lgbti %>%
  filter(tolower(trimws(as.character(s08_p04))) == "sí" | s08_p04 == 1) %>%
  nrow()
PDVUARASM.fexp <- sum(1/base_lgbti$network.size.variable[tolower(trimws(as.character(base_lgbti$s08_p04))) == "sí" | base_lgbti$s08_p04 == 1], na.rm = TRUE)


# Cálculo 
DV_PDVUARASM = (PDVUARASM / denominador_violencia) * 100
DV_PDVUARASM.fexp = (PDVUARASM.fexp / denominador_violencia.fexp) * 100

# Ver resultado
print(paste("Indicador Final (%):", DV_PDVUARASM, "; fexp", DV_PDVUARASM.fexp))

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los últimos 12 meses tuvo experiencias de discriminación y/o violencia y como consecuencia ha pensado en quitarse la vida

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPDVUA)
base_victimas_12m <- base_lgbti %>%
  mutate(es_victima_12m = if_any(paste0("s08_p02_", 1:18, "_2f"), 
                                 ~ tolower(trimws(as.character(.))) == "sí" | . == 1)) %>%
  filter(es_victima_12m == TRUE)

TPDVUA <- nrow(base_victimas_12m)
TPDVUA.fexp <- sum( 1/base_victimas_12m$network.size.variable[base_victimas_12m$es_victima_12m == TRUE], na.rm = TRUE )

# Definición del Numerador (PPQV)
PPQV <- base_victimas_12m %>%
  filter(tolower(trimws(as.character(s08_p06))) == "sí" | s08_p06 == 1) %>%
  nrow()
PPQV.fexp <- sum(1/base_victimas_12m$network.size.variable[tolower(trimws(as.character(base_victimas_12m$s08_p06))) == "sí" | base_victimas_12m$s08_p06 == 1], na.rm = TRUE)

# Cálculo  
DV_PPQV = (PPQV / TPDVUA) * 100
DV_PPQV.fexp = (PPQV.fexp / TPDVUA.fexp) * 100

# Ver resultado
print(paste("Indicador Final (%):", DV_PPQV, "; fexp", DV_PPQV.fexp))

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que a lo largo de su vida  tuvo experiencias de discriminacióno y/o violencia y como consecuencia ha intentado quitarse la vida

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPDVLV)
base_victimas <- base_lgbti %>%
  mutate(es_victima = if_any(paste0("s08_p02_", 1:18), 
                             ~ tolower(trimws(as.character(.))) == "sí" | . == 1)) %>%
  filter(es_victima == TRUE)

TPDVLV <- nrow(base_victimas)
TPDVLV.fexp <- sum( 1/base_victimas$network.size.variable[base_victimas$es_victima == TRUE], na.rm = TRUE )

# Definición del Numerador (PIQV)
PIQV <- base_victimas %>%
  filter(tolower(trimws(as.character(s08_p07))) == "sí" | s08_p07 == 1) %>%
  nrow()
PIQV.fexp <- sum(1/base_victimas$network.size.variable[tolower(trimws(as.character(base_victimas$s08_p07))) == "sí" | base_victimas$s08_p07 == 1], na.rm = TRUE)

# Cálculo  
DV_PIQV = (PIQV / TPDVLV) * 100
DV_PIQV.fexp = (PIQV.fexp / TPDVLV.fexp) * 100

# Ver resultado
print(paste("Indicador Final (%):", DV_PIQV, "; fexp", DV_PIQV.fexp))
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha vivido prácticas de conversión

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TP)
TP <- nrow(base_lgbti)
TP.fexp <- sum(1/base_lgbti$network.size.variable, na.rm = TRUE)

# Construcción del Numerador (PVPC)
base_indicador <- base_lgbti %>%
  mutate(vivi_practicas = if_any(
    all_of(c("s08_p08a", "s08_p08b", "s08_p08c", "s08_p08d", "s08_p08e")),
    ~ tolower(trimws(as.character(.))) == "sí" | . == 1
  ))

PVPC <- sum(base_indicador$vivi_practicas, na.rm = TRUE)
PVPC.fexp <- sum(1/base_indicador$network.size.variable[base_indicador$vivi_practicas == TRUE], na.rm = TRUE)

# Cálculo
PCO_PVPC <- (PVPC / TP) * 100
PCO_PVPC.fexp <- (PVPC.fexp / TP.fexp) * 100

# Ver resultado
print(PCO_PVPC)
print(PCO_PVPC.fexp)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha vivido prácticas de conversión, según el tipo de práctica

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPVPC)
# Universo: Personas que vivieron al menos una práctica de conversión
base_victimas_pco <- base_lgbti %>%
  mutate(vivi_eco_sig = if_any(
    c(s08_p08a, s08_p08b, s08_p08c, s08_p08d, s08_p08e),
    ~ tolower(trimws(as.character(.))) == "sí" | . == 1
  )) %>%
  filter(vivi_eco_sig == TRUE)

TPVPC <- nrow(base_victimas_pco)
TPVPC.fexp <- sum(1/base_victimas_pco$network.size.variable, na.rm = TRUE)

# Cálculo del Numerador y Porcentaje por Categoría
resumen_tipos <- base_victimas_pco %>%
  summarise(
    `Hormonas/Medicamentos` = sum(tolower(trimws(as.character(s08_p08a))) == "sí" | s08_p08a == 1, na.rm = TRUE),
    `Rituales/Exorcismos`   = sum(tolower(trimws(as.character(s08_p08b))) == "sí" | s08_p08b == 1, na.rm = TRUE),
    `Violencia Sexual`      = sum(tolower(trimws(as.character(s08_p08c))) == "sí" | s08_p08c == 1, na.rm = TRUE),
    `Terapias Psi`          = sum(tolower(trimws(as.character(s08_p08d))) == "sí" | s08_p08d == 1, na.rm = TRUE),
    `Internamiento`         = sum(tolower(trimws(as.character(s08_p08e))) == "sí" | s08_p08e == 1, na.rm = TRUE),
    
    `Hormonas/Medicamentos.fexp` = sum(1/network.size.variable[tolower(trimws(as.character(s08_p08a))) == "sí" | s08_p08a == 1], na.rm = TRUE),
    `Rituales/Exorcismos.fexp`   = sum(1/network.size.variable[tolower(trimws(as.character(s08_p08b))) == "sí" | s08_p08b == 1], na.rm = TRUE),
    `Violencia Sexual.fexp`      = sum(1/network.size.variable[tolower(trimws(as.character(s08_p08c))) == "sí" | s08_p08c == 1], na.rm = TRUE),
    `Terapias Psi.fexp`          = sum(1/network.size.variable[tolower(trimws(as.character(s08_p08d))) == "sí" | s08_p08d == 1], na.rm = TRUE),
    `Internamiento.fexp`         = sum(1/network.size.variable[tolower(trimws(as.character(s08_p08e))) == "sí" | s08_p08e == 1], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Tipo_Practica", values_to = "PVPCTPx") %>%
  mutate(
    TPVPC = c(rep(TPVPC,5),rep(TPVPC.fexp,5)),
    PCO_PVPCTPx =(PVPCTPx / TPVPC) * 100
  ) %>% 
  select(Tipo_Practica,PCO_PVPCTPx)

# Ver resultado
print(as.data.frame(resumen_tipos))

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más, según cómo ocurrió el internamiento

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPICAC)
base_internados <- base_lgbti %>%
  filter(tolower(trimws(as.character(s08_p08e))) == "sí" | s08_p08e == 1)

TPICAC <- nrow(base_internados)
TPICAC.fexp <- sum(1/base_internados$network.size.variable, na.rm = TRUE)

# Cálculo por Forma de Internamiento 
resumen_internamiento <- base_internados %>%
  summarise(
    `Voluntad Propia` = sum(tolower(trimws(as.character(s08_p10a))) == "sí" | s08_p10a == 1, na.rm = TRUE),
    `Secuestro`       = sum(tolower(trimws(as.character(s08_p10b))) == "sí" | s08_p10b == 1, na.rm = TRUE),
    `Intimidación, Engaño o Amenaza` = sum(tolower(trimws(as.character(s08_p10c))) == "sí" | s08_p10c == 1, na.rm = TRUE),
    `Otro`            = sum(tolower(trimws(as.character(s08_p10d))) == "sí" | s08_p10d == 1, na.rm = TRUE),
    
    `Voluntad Propia.fexp` = sum(1/network.size.variable[tolower(trimws(as.character(s08_p10a))) == "sí" | s08_p10a == 1], na.rm = TRUE),
    `Secuestro.fexp`       = sum(1/network.size.variable[tolower(trimws(as.character(s08_p10b))) == "sí" | s08_p10b == 1], na.rm = TRUE),
    `Intimidación, Engaño o Amenaza.fexp` = sum(1/network.size.variable[tolower(trimws(as.character(s08_p10c))) == "sí" | s08_p10c == 1], na.rm = TRUE),
    `Otro.fexp`            = sum(1/network.size.variable[tolower(trimws(as.character(s08_p10d))) == "sí" | s08_p10d == 1], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Forma_Internamiento", values_to = "PICACCOx") %>%
  mutate(
    TPICAC = c(rep(TPICAC,4),rep(TPICAC.fexp,4)),
    PCO_PICACCOx = (PICACCOx / TPICAC) * 100) %>% 
  select(Forma_Internamiento,PCO_PICACCOx)

resumen_limpio <- resumen_internamiento %>%
  select(Forma_Internamiento, PCO_PICACCOx)

# Ver resultado
print(as.data.frame(resumen_limpio), row.names = FALSE)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más, según experiencias en el internamiento

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPICAC)
# Filtramos a quienes reportaron internamiento (s08_p08e)
base_experiencias <- base_lgbti %>%
  filter(tolower(trimws(as.character(s08_p08e))) == "sí" | s08_p08e == 1)

TPICAC <- nrow(base_experiencias)
TPICAC.fexp <- sum(1/base_experiencias$network.size.variable, na.rm = TRUE)

# Cálculo por Experiencia en el Internamiento 
resumen_experiencias <- base_experiencias %>%
  summarise(
    `Golpes o agresiones físicas` = sum(tolower(trimws(as.character(s08_p11a))) == "sí" | s08_p11a == 1, na.rm = TRUE),
    `Gritos, insultos o amenazas` = sum(tolower(trimws(as.character(s08_p11b))) == "sí" | s08_p11b == 1, na.rm = TRUE),
    `Acoso sexual`                = sum(tolower(trimws(as.character(s08_p11c))) == "sí" | s08_p11c == 1, na.rm = TRUE),
    `Abuso sexual`                = sum(tolower(trimws(as.character(s08_p11d))) == "sí" | s08_p11d == 1, na.rm = TRUE),
    `Violación sexual`            = sum(tolower(trimws(as.character(s08_p11e))) == "sí" | s08_p11e == 1, na.rm = TRUE),
    `Daños a pertenencias`        = sum(tolower(trimws(as.character(s08_p11f))) == "sí" | s08_p11f == 1, na.rm = TRUE),
    `Otro`                        = sum(tolower(trimws(as.character(s08_p11g))) == "sí" | s08_p11g == 1, na.rm = TRUE),
    
    `Golpes o agresiones físicas.fexp` = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11a))) == "sí" | s08_p11a == 1], na.rm = TRUE),
    `Gritos, insultos o amenazas.fexp` = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11b))) == "sí" | s08_p11b == 1], na.rm = TRUE),
    `Acoso sexual.fexp`                = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11c))) == "sí" | s08_p11c == 1], na.rm = TRUE),
    `Abuso sexual.fexp`                = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11d))) == "sí" | s08_p11d == 1], na.rm = TRUE),
    `Violación sexual.fexp`            = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11e))) == "sí" | s08_p11e == 1], na.rm = TRUE),
    `Daños a pertenencias.fexp`        = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11f))) == "sí" | s08_p11f == 1], na.rm = TRUE),
    `Otro.fexp`                        = sum(1/network.size.variable[tolower(trimws(as.character(s08_p11g))) == "sí" | s08_p11g == 1], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Experiencia", values_to = "PICACEx") %>%
  mutate(
    TPICAC = c(rep(TPICAC,7),rep(TPICAC.fexp,7)),
    PCO_PICACEx = (PICACEx / TPICAC) * 100) %>% 
  select(Experiencia, PCO_PICACEx)

resumen_limpio <- resumen_experiencias %>%
  select(Experiencia, PCO_PICACEx)

# Ver resultado
print(as.data.frame(resumen_limpio), row.names = FALSE)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los últimos 12 meses tuvo experiencias de discriminación y/o violencia y realizó alguna denuncia

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPDVUA)
base_victimas_12m <- base_lgbti %>%
  mutate(es_victima_12m = if_any(
    all_of(paste0("s08_p02_", 1:18, "_2f")), 
    ~ tolower(trimws(as.character(.))) == "sí" | . == 1
  )) %>%
  filter(es_victima_12m == TRUE)

TPDVUA <- nrow(base_victimas_12m)
TPDVUA.fexp <- sum(1/base_victimas_12m$network.size.variable, na.rm = TRUE)

# Cálculo del Numerador (PDVUAD)
base_denuncias <- base_victimas_12m %>%
  mutate(realizo_denuncia = tolower(trimws(as.character(s09_p01))) == "sí" | s09_p01 == 1)

PDVUAD <- sum(base_denuncias$realizo_denuncia, na.rm = TRUE)
PDVUAD.fexp <- sum(1/base_denuncias$network.size.variable[base_denuncias$realizo_denuncia == TRUE], na.rm = TRUE)

# Cálculo 
AJ_PDVUAD <- (PDVUAD / TPDVUA) * 100
AJ_PDVUAD.fexp <- (PDVUAD.fexp / TPDVUA.fexp) * 100

# Ver resultado
print(AJ_PDVUAD)
print(AJ_PDVUAD.fexp)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los últimos 12 meses tuvo experiencias de discriminación y/o violencia que continuó con el proceso de denuncia

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPDVUAD)
base_denunciantes_12m <- base_lgbti %>%
  mutate(es_victima_12m = if_any(
    all_of(paste0("s08_p02_", 1:18, "_2f")), 
    ~ tolower(trimws(as.character(.))) == "sí" | . == 1
  )) %>%
  filter(es_victima_12m == TRUE) %>%
  filter(tolower(trimws(as.character(s09_p01))) == "sí" | s09_p01 == 1)

TPDVUAD <- nrow(base_denunciantes_12m)
TPDVUAD.fexp <- sum(1/base_denunciantes_12m$network.size.variable, na.rm = TRUE)

# Cálculo del Numerador (PDVUADCP)
base_continuidad <- base_denunciantes_12m %>%
  filter(tolower(trimws(as.character(s09_p04))) == "sí" | s09_p04 == 1)

PDVUADCP <- nrow(base_continuidad)
PDVUADCP.fexp <- sum(1/base_continuidad$network.size.variable, na.rm = TRUE)

# Cálculo 
AJ_PDVUADCP <- (PDVUADCP / TPDVUAD) * 100
AJ_PDVUADCP.fexp <- (PDVUADCP.fexp / TPDVUAD.fexp) * 100

# Ver resultado
print(AJ_PDVUADCP)
print(AJ_PDVUADCP.fexp)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los últimos 12 meses tuvo experiencias de discriminación y/o violencia, según las razones por las que no continuó con el proceso de denuncia

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

# Definición del Denominador (TPDVUADNCP)
# Personas que: fueron víctimas (12m) -> denunciaron (s09p01) -> NO continuaron (s09p04)
base_no_continuo <- base_lgbti %>%
  mutate(es_victima_12m = if_any(
    all_of(paste0("s08_p02_", 1:18, "_2f")), 
    ~ tolower(trimws(as.character(.))) == "sí" | . == 1
  )) %>%
  filter(es_victima_12m == TRUE) %>%
  
  filter(tolower(trimws(as.character(s09_p01))) == "sí" | s09_p01 == 1) %>%
  
  filter(tolower(trimws(as.character(s09_p04))) == "no" | s09_p04 == 2)

TPDVUADNCP <- nrow(base_no_continuo)
TPDVUADNCP.fexp <- sum(1/base_no_continuo$network.size.variable, na.rm = TRUE)

# Función para calcular el valor puro por categoría
calcular_razon <- function(columna) {
  num <- sum(tolower(trimws(as.character(base_no_continuo[[columna]]))) == "sí" | 
               base_no_continuo[[columna]] == 1, na.rm = TRUE)
  num.fexp <- sum(1/base_no_continuo[["network.size.variable"]][tolower(trimws(as.character(base_no_continuo[[columna]]))) == "sí" | base_no_continuo[[columna]] == 1], na.rm = TRUE)
  
  return( c(sin = (num / TPDVUADNCP) * 100, fexp = (num.fexp / TPDVUADNCP.fexp) * 100 )   )
}

# Cálculo de todas las categorías
razones_lista <- list(
  Atencion_Inadecuada = calcular_razon("s09_p06a"),
  Aconsejo_No_Seguir  = calcular_razon("s09_p06b"),
  Revictimizacion     = calcular_razon("s09_p06c"),
  Desconfianza_Sist   = calcular_razon("s09_p06d"),
  Amenazas_Represalia = calcular_razon("s09_p06e"),
  Falta_Abogado       = calcular_razon("s09_p06f"),
  Falta_Recursos      = calcular_razon("s09_p06g"),
  Otro                = calcular_razon("s09_p06h")
)

# Ver resultado
print(razones_lista)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que conoce de leyes y normativas de protección de derechos y sanción a la discriminación

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

setDT(base_lgbti)

norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")


ind_legal <- base_lgbti[, .(
  PCLNPDSD = sum(s10_p01 == 1 | as.character(s10_p01) == "1" | norm_txt(s10_p01) == "si", na.rm = TRUE),
  
  TP = .N,
  
  DIF_PCLNPDSD.fexp = (sum(1/network.size.variable[s10_p01 == 1 | as.character(s10_p01) == "1" | norm_txt(s10_p01) == "si"], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]
ind_legal[, DIF_PCLNPDSD := (PCLNPDSD / TP) * 100 ]

# Ver resultado
print("Indicador: Conocimiento Marco Legal")
print(ind_legal)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que conoce sobre la modificación para el reconocimiento del género y/o sexo en documentos oficiales

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

setDT(base_lgbti)

norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

ind_cedula_conoc <- base_lgbti[, .(
  PCMRGS = sum(s10_p02 == 1 | as.character(s10_p02) == "1" | norm_txt(s10_p02) == "si", na.rm = TRUE),
  TP = .N,
  
  DIF_PCMRGS.fexp = (sum(1/network.size.variable[s10_p02 == 1 | as.character(s10_p02) == "1" | norm_txt(s10_p02) == "si"], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]
ind_cedula_conoc[, DIF_PCMRGS := (PCMRGS / TP) * 100]

# Ver resultado
print("Indicador: Conocimiento Modificación Cédula")
print(ind_cedula_conoc)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha realizado el cambio de datos en la cédula en el  Registro Civil, según el tipo de modificación

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

setDT(base_lgbti)

norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

base_filtro <- base_lgbti[s10_p02 == 1 | as.character(s10_p02) == "1" | norm_txt(s10_p02) == "si"]

# Calculamos para cada categoría (Nombre, Sexo, Género)
ind_ejercicio <- data.table(
  Categoria = c("Nombre", "Sexo", "Género", "Nombre.fexp", "Sexo.fexp", "Género.fexp"),
  PCDRCx = c(
    sum(base_filtro$s10_p03a == 1 | as.character(base_filtro$s10_p03a) == "1" | norm_txt(base_filtro$s10_p03a) == "si", na.rm = TRUE),
    sum(base_filtro$s10_p03b == 1 | as.character(base_filtro$s10_p03b) == "1" | norm_txt(base_filtro$s10_p03b) == "si", na.rm = TRUE),
    sum(base_filtro$s10_p03c == 1 | as.character(base_filtro$s10_p03c) == "1" | norm_txt(base_filtro$s10_p03c) == "si", na.rm = TRUE),
    
    sum(1/base_filtro$network.size.variable[base_filtro$s10_p03a == 1 | as.character(base_filtro$s10_p03a) == "1" | norm_txt(base_filtro$s10_p03a) == "si"], na.rm = TRUE),
    sum(1/base_filtro$network.size.variable[base_filtro$s10_p03b == 1 | as.character(base_filtro$s10_p03b) == "1" | norm_txt(base_filtro$s10_p03b) == "si"], na.rm = TRUE),
    sum(1/base_filtro$network.size.variable[base_filtro$s10_p03c == 1 | as.character(base_filtro$s10_p03c) == "1" | norm_txt(base_filtro$s10_p03c) == "si"], na.rm = TRUE)
  ),
  
  TPCMRGS = c( rep(nrow(base_filtro),3), rep(sum(1/base_filtro$network.size.variable, na.rm = TRUE),3)  )
)

ind_ejercicio[, DIF_PCDRCx := (PCDRCx / TPCMRGS) * 100]

# Ver resultado
print("Indicador: Ejercicio Efectivo del Cambio (según categoría)")
print(ind_ejercicio)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que conoce sobre el derecho al matrimonio civil igualitario o unión de hecho

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos ------------------------------------------------


# Establecer directorio ---------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías --------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos --------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador -------------------------------------------------------

setDT(base_lgbti)

norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

# Definición del Numerador (PCDMCI)
base_lgbti[, PCDMCI := 0]
base_lgbti[s10_p04 == 1 | 
             as.character(s10_p04) == "1" | 
             norm_txt(s10_p04) == "si", 
           PCDMCI := 1]

# Definición del Denominador (TP)
base_lgbti[, TP_GENERAL := 1]

# Cálculo 
ind_matrimonio <- base_lgbti[, .(
  PCDMCI = sum(PCDMCI, na.rm = TRUE),
  TP = .N,
  DIF_PCDMCI = (sum(PCDMCI, na.rm = TRUE) / .N) * 100,
  DIF_PCDMCI.fexp = (sum(1/network.size.variable[PCDMCI == 1], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]

# Ver resultado
print("Indicador: Conocimiento sobre Matrimonio Igualitario y Uniones de Hecho")
print(ind_matrimonio)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha inscrito a sus hijos en el Registro Civil

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos --------------------------------------------------


# Establecer directorio ----------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías -----------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador ----------------------------------------------------------

setDT(base_lgbti)

norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

base_padres <- base_lgbti[s07_p01 == 1 | 
                            as.character(s07_p01) == "1" | 
                            norm_txt(s07_p01) == "si"]

ind_inscripcion_hijos <- base_padres[, .(
  PIHRC = sum(s10_p07 == 1 | as.character(s10_p07) == "1" | norm_txt(s10_p07) == "si", na.rm = TRUE),
  TPH = .N,
  
  DIF_PIHRC.fexp = (sum(1/network.size.variable[s10_p07 == 1 | as.character(s10_p07) == "1" | norm_txt(s10_p07) == "si"], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]

ind_inscripcion_hijos[, DIF_PIHRC := (PIHRC / TPH) * 100]

# Ver resultado
print("Indicador: Inscripción de hijos en el Registro Civil")
print(ind_inscripcion_hijos)
#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que vive con sus hijos/as

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos ------------------------------------------------


# Establecer directorio ---------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías --------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador -------------------------------------------------------

setDT(base_lgbti)
norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

base_progenitores <- base_lgbti[s07_p01 == 1 | 
                                  as.character(s07_p01) == "1" | 
                                  norm_txt(s07_p01) == "si"]

ind_convivencia_hijos <- base_progenitores[, .(
  PVH = sum(s10_p08 == 1 | as.character(s10_p08) == "1" | norm_txt(s10_p08) == "si", na.rm = TRUE),
  TPH = .N,
  
  DIF_PVH.fexp = (sum(1/network.size.variable[s10_p08 == 1 | as.character(s10_p08) == "1" | norm_txt(s10_p08) == "si"], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]

ind_convivencia_hijos[, DIF_PVH := (PVH / TPH) * 100]

# Ver resultado
print("Indicador: Porcentaje de población LGBTI+ que vive con sus hijos")
print(ind_convivencia_hijos)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que en los últimos 12 meses ha participado en un grupo, colectivo u organización social LGBTI+

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos ------------------------------------------------


# Establecer directorio ---------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías --------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador -------------------------------------------------------

setDT(base_lgbti)
norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

# Definición del Numerador (PPGCS)
base_lgbti[, PPGCS := 0]
base_lgbti[s11_p01 == 1 | 
             as.character(s11_p01) == "1" | 
             norm_txt(s11_p01) == "si", 
           PPGCS := 1]

# Definición del Denominador (TP)
base_lgbti[, TP_PARTICIPACION := 1]

# Cálculo  
ind_participacion <- base_lgbti[, .(
  PPGCS = sum(PPGCS, na.rm = TRUE),
  TP = .N,
  PC_PPGCS = (sum(PPGCS, na.rm = TRUE) / .N) * 100,
  
  PC_PPGCS.fexp = (sum(1/network.size.variable[PPGCS == 1], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]

# Ver resultado
print("Indicador: Participación en Colectivos u Organizaciones LGBTI+")
print(ind_participacion)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que conoce de los mecanismos de participación ciudadana

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos ------------------------------------------------


# Establecer directorio ---------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías --------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador -------------------------------------------------------

setDT(base_lgbti)
norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

# Definición del Numerador (PCMPC)
base_lgbti[, PCMPC := 0]
base_lgbti[s11_p04 == 1 | 
             as.character(s11_p04) == "1" | 
             norm_txt(s11_p04) == "si", 
           PCMPC := 1]

# Definición del Denominador (TP)
base_lgbti[, TP_CIUDADANIA := 1]

# Cálculo  
ind_conoc_mecanismos <- base_lgbti[, .(
  Numerador_Conocen = sum(PCMPC, na.rm = TRUE),
  Denominador_Total = .N,
  PC_PCMPC = (sum(PCMPC, na.rm = TRUE) / .N) * 100,
  
  PC_PCMPC.fexp = (sum(1/network.size.variable[PCMPC == 1], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]

# Ver resultado
print("Indicador: Conocimiento de Mecanismos de Participación Ciudadana")
print(ind_conoc_mecanismos)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha hecho uso de mecanismos de participación ciudadana

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos ------------------------------------------------


# Establecer directorio ---------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías --------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador -------------------------------------------------------

setDT(base_lgbti)
norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

# Definición del Denominador (PCMPC)
base_conocimiento <- base_lgbti[s11_p04 == 1 | 
                                  as.character(s11_p04) == "1" | 
                                  norm_txt(s11_p04) == "si"]

# Cálculo del Numerador (PUMPC) 
ind_uso_efectivo <- base_conocimiento[, .(
  PUMPC = sum(s11_p05 == 1 | as.character(s11_p05) == "1" | norm_txt(s11_p05) == "si", na.rm = TRUE),
  PCMPC = .N,
  
  PC_PUMPC.fexp = (sum(1/network.size.variable[s11_p05 == 1 | as.character(s11_p05) == "1" | norm_txt(s11_p05) == "si"], na.rm = TRUE)/sum(1/network.size.variable, na.rm = TRUE)) * 100
)]

# Cálculo 
ind_uso_efectivo[, PC_PUMPC := (PUMPC / PCMPC) * 100]

# Ver resultado
print("Indicador: Uso de Mecanismos entre quienes tienen conocimiento")
print(ind_uso_efectivo)

#=======================================================================#
#Nombre del indicador: Porcentaje de la población LGBTI+ de 18 años y más que ha recibido capacitaciones en los últimos 12 meses, según temática

#Operación Estadística:
#Encuesta Nacional de Condiciones de Vida de la Población LGBTI+

#Autor de la sintaxis: Instituto Nacional de Estadística y Censos (INEC)
#Dirección Técnica: Dirección de Estadísticas Sociodemográficas (DIES)

#Fecha de elaboración: Marzo 2026*/

# Versión sintaxis: 1.0
# Software: R 4.5.2


# Proceso-------------------------- 


# Descargar bases de datos ------------------------------------------------


# Establecer directorio ---------------------------------------------------
#Establecer directorio donde se encuentran las bases descargadas y en el cual se guardarán los resultados.
setwd(“D:/Users/Base_datos_ENCV_LGBTI+_2025_tratada”)


#Cargar librerías --------------------------------------------------------

library(readr) # leer archivos de texto plano como .csv o .txt.
library(tidyverse) # tibble: librería se  utilizar rownames_to_column para renombrar la columna de las filas, frecuencias
library(summarytools) # Genera tablas de frecuencias y resúmenes estadísticos muy visuales y fáciles de leer
library(readxl) # leer archivos de Excel (.xls y .xlsx)
library(RDS) # Sirve para guardar y cargar objetos nativos de R conservando todas sus propiedades originales
library(lubridate) # para cambio de formatos
library(openxlsx) # para escribir y dar formato a archivos Excel
library(labelled) # Permite trabajar con "etiquetas" (labels) en las variables
library(stringr) # Permite buscar patrones, reemplazar palabras o recortar espacios en blanco
library(dplyr) # Librería necesaria para case_when
library(tidyr) # Se usa para pivotar tablas (pasar de formato ancho a largo)
library(data.table)# Manipulación de bases de datos masivas



# Importar base de datos ------------------------------------------------------
base_lgbti <- read_excel("C:/Users/Base_datos_ENCV_LGBTI+_2025_tratada", guess_max = 10000) 



############-----------------------------------------------#############

#Cálculo factor de expansión (network.size.variable)y tratamiento de anómalos (Ejecución Única)--------------------------

### NOTA TÉCNICA: Esta sección del código DEBE APLICARSE EN UNA SOLA OCASIÓN por sesión de trabajo tras la carga de la base cruda-----###

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

# Datos anómales ### NOTA TÉCNICA: Esta sintaxis modifica los valores de network.size.variable directamente. Si se corre dos veces sobre la misma base, se calcularán nuevos percentiles sobre datos ya corregidos, eliminando variabilidad real necesaria para el análisis estadístico###

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


b 
#plot(base$ola, base$network.size.variable)

base_lgbti <- cbind(base_lgbti,fexp=1/base_lgbti$network.size.variable)
# openxlsx::write.xlsx(base_lgbti,"Data/LGBTI/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Definir la ruta de la carpeta
openxlsx::write.xlsx(base_lgbti, "LGBTI_FINALES/21. Base_datos_ENCV_LGBTI+_2025_tratada_V3 - fexp.xlsx")

# Limpia espacio de trabajo --------
rm(list = setdiff(ls(), c("base_lgbti")))

############-----------------------------------------------#############



# Cálculo Indicador -------------------------------------------------------

setDT(base_lgbti)
norm_txt <- function(x) iconv(tolower(trimws(as.character(x))), to = "ASCII//TRANSLIT")

# Definición del Denominador (TP)
denominador_tp <- nrow(base_lgbti)
denominador_tp.fexp <- sum(1/base_lgbti$network.size.variable, na.rm = TRUE)

# Calcular cada categoría de forma independiente
calc_capacitacion <- function(columna, etiqueta) {
  num <- base_lgbti[get(columna) == 1 | as.character(get(columna)) == "1" | norm_txt(get(columna)) == "si", .N]
  
  a <- base_lgbti[get(columna) == 1 | as.character(get(columna)) == "1" | norm_txt(get(columna)) == "si", "network.size.variable"]
  num.fexp <- sum(1/a, na.rm = TRUE)
  
  return(data.table(
    Tematica = c(etiqueta, paste(etiqueta,".fexp",sep = "")),
    PRCTx = c(num, num.fexp),
    TP = c(denominador_tp, denominador_tp.fexp),
    PC_PRCTx = c( (num / denominador_tp) * 100, (num.fexp / denominador_tp.fexp) * 100 ) 
  ))
}

# Mapeo de todas las variables s11_p06
res_capacitacion <- rbind(
  calc_capacitacion("s11_p06a", "Participación ciudadana"),
  calc_capacitacion("s11_p06b", "Empleabilidad"),
  calc_capacitacion("s11_p06c", "Emprendimiento/Finanzas"),
  calc_capacitacion("s11_p06d", "Motivación/Autoestima"),
  calc_capacitacion("s11_p06e", "Derechos Humanos"),
  calc_capacitacion("s11_p06f", "Salud sexual y reproductiva"),
  calc_capacitacion("s11_p06g", "Formación artesanal/oficios"),
  calc_capacitacion("s11_p06h", "Otro")
)

# Ver resultado
print("Indicador: Capacitaciones recibidas por temática (Últimos 12 meses)")
print(res_capacitacion)
#=======================================================================#

