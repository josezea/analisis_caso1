library(dplyr)
library(readr) # arcivos textos

# Objetivo: comprender en el territorio en donde están los hogares con pobreza monetaria
# o pobreza multidimensional


# Paso 1: Ubicar la carpeta donde están los datos
ruta <- "C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/Semestre2/Análisis de casos/clase2"
setwd(ruta)
# dir() # Listar archivos de la carpeta en donde nos ubicamos


# Paso 2: Leer los datos en el formato específico (text, txt, csv,
# .sav, dta, sasbd7, odt, xlsx)

# Leer los archivos
hog <- readr::read_delim( "variables_adicionales_hogar_v3.txt", 
                          delim = ";")
viv <- read_csv("viviendas_2017_v2_03092018.txt")

Encoding(viv$ESTRATO_TEX) <- "latin1"
Encoding(viv$LOCALIDAD_TEX) <- "latin1"

# Paso 3: Exploración rapida de los datos
# es DIRECTORIO_HOG la llave o ID unico de la tabla hog??
# table(duplicated(hog$DIRECTORIO_HOG))
# names(hog)
# dim(hog)
# setwd("~/Laboral2020/Konrad Lorenz/Semestre2/Análisis de casos/clase2")
# str(hog)
summary(hog)

# Variables comunes en ambas tablas:
intersect(names(hog), names(viv) )
viv$FEX_C <- NULL
intersect(names(hog), names(viv) )

# table(viv$ESTRATO_TEX)
# length(table(viv$ESTRATO_TEX))

# Paso 4: Seleccionar variables relevantes (features)
# y hacer los filtros relevantes
# class(viv$DPTOMPIO)
# table(viv$CLASE == 1) # 1 es la clase urbano
viv <- filter(viv, DPTOMPIO == 11001 & CLASE == 1)
viv <- select(viv, DIRECTORIO, ESTRATO_TEX,  LOCALIDAD_TEX)
viv <- arrange(viv, LOCALIDAD_TEX,  ESTRATO_TEX)

summary(hog$IPM)

hog <- select(hog, DIRECTORIO, DIRECTORIO_HOG, INGRESOS_HOG,
              INGRESOS_PER_CAPITA, HOGAR_LP,
              IPM, HOGAR_POBRE_IPM, ESTRATO_VIV)

hogviv <- right_join(hog, viv, by = "DIRECTORIO")

# table(viv$ESTRATO_TEX)
# length(table(viv$ESTRATO_TEX))

# Reemplaza los símbolos más interpretados por nuestro 
# sistema operativa como ñ y tildes  por símbolos entendibles

# Paso 5. Transformacion de variables

# Ver tamaño de los hogares: reconstruirlo a partir
# del ingreso y el ingreso percapita del hogar
# Ingresoperc = ingreso / numIntegrantesHog
# numIntegrantesHog = ingreso / Ingresoperc

hogviv$NumPersHog <- hogviv$INGRESOS_HOG / hogviv$INGRESOS_PER_CAPITA
table(is.na(hogviv$NumPersHog))

# Otra forma de hacerlo con el verbo mutate
hogviv <- mutate(hogviv, NumPersHog = INGRESOS_HOG / INGRESOS_PER_CAPITA)

# Paso 6: EDA 
# Tarea: 
# 1) calcular por localidad y upz el promedio del ingreso 
# 2) Calcular por localidad y upz la proporción de hogares por debajo de la linea de pobreza
# 3) Calcular por localidad y UPZ la proporción de hogares con pobreza multidim.

# BONUS:
# Hacer un mapa por localidad (UPZ) de la pobreza monetaria (IPM)

