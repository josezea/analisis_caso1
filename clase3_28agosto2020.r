library(dplyr)
library(readr) # arcivos textos
library(reshape2)
# Objetivo: comprender en el territorio en donde están los hogares con pobreza monetaria
# o pobreza multidimensional


# Paso 1: Ubicar la carpeta donde están los datos
ruta <- "/home/jose/Documentos/"
setwd(ruta)
# dir() # Listar archivos de la carpeta en donde nos ubicamos


# Paso 2: Leer los datos en el formato específico (text, txt, csv,
# .sav, dta, sasbd7, odt, xlsx)

# Leer los archivos
hog <- readr::read_delim( "Variables_Adicionales_hogar.txt" , 
                          delim = ";")
viv <- read_csv("viviendas_2017_v2_03092018.txt" )

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

# Solución: Otros verbos
#group_by, summarise
# %>% : operador pipe
table(hogviv$ESTRATO_VIV, useNA = "always")
hogviv$ESTRATO_VIV %>% table(useNA = "always")

# f(x,y) equivalente a x %>% f(y)

# 1) calcular por localidad y upz el promedio del ingreso 

consulta <- group_by(hogviv, LOCALIDAD_TEX, ESTRATO_TEX) 
consulta <- summarise(consulta, prom_ingreso = mean(INGRESOS_HOG)) 
consulta <- arrange(consulta, desc(prom_ingreso)) 

# Sin group_by
# consulta <- summarise(ho gviv, prom_ingreso = mean(INGRESOS_HOG)) 
  
#  magrit, pipe
consulta_preliminar <- hogviv %>% group_by(LOCALIDAD_TEX, ESTRATO_TEX) %>%
                       summarise(prom_ingreso = mean(INGRESOS_HOG)) %>%
                       arrange(desc(prom_ingreso))

consulta_preliminarB <- aggregate(INGRESOS_HOG ~ LOCALIDAD_TEX + ESTRATO_TEX , 
                                 data = hogviv, FUN = mean)

consulta_preliminarC <- aggregate(hogviv$INGRESOS_HOG, list(hogviv$LOCALIDAD_TEX, 
                                                           hogviv$ESTRATO_TEX) , 
                                 FUN = mean)
names(consulta_preliminarC) <- c("LOCALIDAD_TEX", "ESTRATO_TEX", "prom_ingreso")

# Una forma flexible no muy util en esta situación para presentar la información:
consulta <- dcast(data = hogviv, formula = LOCALIDAD_TEX ~ ESTRATO_TEX, 
      value.var =  "INGRESOS_HOG", fun.aggregate = mean)



# Parentesis : estrato sociecon
table(hogviv$ESTRATO_VIV)
# Para contar faltantes
table(hogviv$ESTRATO_VIV, useNA = "always")
a <- c(NA, 2, 4, NA)
sum(a)
sum(a, na.rm = T)

# 2) Calcular por localidad y upz ecl conteo y la  proporción de hogares por debajo de la linea de pobreza

# Frecuencias absolutas (conteos)
# Bogota
table(hogviv$HOGAR_LP)
sum(hogviv$HOGAR_LP)


consulta2 <- hogviv %>% group_by(LOCALIDAD_TEX, ESTRATO_TEX) %>%
  summarise(total_LP = sum(HOGAR_LP)) %>%
  arrange(desc(total_LP))

consulta2B <- hogviv %>% group_by(LOCALIDAD_TEX, ESTRATO_TEX, HOGAR_LP) %>%
  summarise(Cuenta = n()) %>%
  arrange(desc(HOGAR_LP), desc(Cuenta))
# Mismos resultados pero con dos enfoques diferentes


# Frecuencias relativas (proporción de personas pobres) 
consulta3 <- hogviv %>% group_by(LOCALIDAD_TEX, ESTRATO_TEX) %>%
  summarise(prop_LP = mean(HOGAR_LP)) %>%
  arrange(desc(prop_LP))

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, decimal.mark = ","), "%")
}

consulta3$prop_LP  <- percent(consulta3$prop_LP) 

#paste0("Juan", "Perez")
  
  consulta3B <- hogviv %>% group_by(LOCALIDAD_TEX, ESTRATO_TEX, HOGAR_LP) %>%
    summarise(Cuenta = n()) %>% ungroup() %>% group_by(LOCALIDAD_TEX, ESTRATO_TEX) %>%
    mutate(TotalHog_UPZ = sum(Cuenta)) %>% arrange(LOCALIDAD_TEX,  ESTRATO_TEX ) %>%
    mutate(Prop_condicion = Cuenta / TotalHog_UPZ) # %>% filter(HOGAR_LP == 1)
    


# 3) Calcular por localidad y UPZ la proporción de hogares con pobreza multidim.

# BONUS:
# Hacer un mapa por localidad (UPZ) de la pobreza monetaria (IPM)
