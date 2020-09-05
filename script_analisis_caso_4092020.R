library(readr)
library(dplyr)
library(lubridate)
setwd("~/Documentos/datos")
dir()
datos <- read_csv( "SECOP_II_-_Procesos_de_Contrataci_n.csv", n_max = 19674)
# [1] 609381     57

datos <- data.table::fread( "SECOP_II_-_Procesos_de_Contrataci_n.csv")
# [1] 609381     57

# Universon: Procesos contractuales
table(datos$Adjudicado)
df <- subset(datos,   Adjudicado == "Si")
class(df)

df <- dplyr::filter(datos,   Adjudicado == "Si")
class(df)

# ID DEL PROCESO es una llave primaria (identifica de manera única cada registro)

# Ejemplo jugeute
df_juguete <- data.frame(ID = c(1,4, 2, 4, 5, 4, 7, 6, 7, 9, 10, 10, 12), y = rnorm(13))
# iDENTIFICAR LOS duplicados de abajo para arriba

# Identificar los duplicados de arriba para abajo
df_juguete$ID 
duplicated(df_juguete$ID) # Duplicados evaluando de arriba para abajo
duplicated(df_juguete$ID, fromLast = T) # duplicados evaluando de abajo para arriba
duplicated(df_juguete$ID) | duplicated(df_juguete$ID, fromLast = T) # conjunto de valores que tienen duplicados
class(duplicated(df_juguete$ID))

TRUE
5 == 6
!c(F, T) # >Negación

# Identificar los NO duplicados
!duplicated(df_juguete$ID)
!duplicated(df_juguete$ID, fromLast = T)
!(duplicated(df_juguete$ID) | duplicated(df_juguete$ID, fromLast = T))

# Cuantos duplicados hay
table(duplicated(df_juguete$ID))
table(duplicated(df_juguete$ID, fromLast = T))
table(duplicated(df_juguete$ID) | duplicated(df_juguete$ID, fromLast = T)) 

# Eliminar duplicados
df_juguete[c(1,2),]
df_juguete[c(T,T, rep(F, 11)),]
#df[vctrfilas, vctrcolumnas]
#df[vctrfilas, ] # Dejo todas las columnas

# unique(df_juguete$ID)
# df_juguete$ID

df_juguete[!duplicated(df_juguete$ID),]

df_juguete[!duplicated(df_juguete$ID, fromLast = T),]
df_juguete[!(duplicated(df_juguete$ID) | duplicated(df_juguete$ID, fromLast = T)),]

# Ejercicio identificar duplicados por la variable ???
# datos$`ID del Proceso`
table(duplicated(datos$`ID del Proceso`))
prop.table(table(duplicated(datos$`ID del Proceso`)))

names(datos)
class(datos$`Fecha de Publicacion del Proceso`)
datos$fecha_pubProc <- as.Date(datos$`Fecha de Publicacion del Proceso`, format = "%m/%d/%Y")
datos$year <- lubridate::year(datos$fecha_pubProc)  
table(datos$year, useNA = "always")

# Entidad Año , Id del proceso
paste0("Juan", " Perez")
datos$ID <- paste0(datos$Entidad,"_", datos$year, "_", datos$`ID del Proceso`)
table(duplicated(datos$ID))

df_revisionDup <- datos[duplicated(datos$`ID del Proceso`) | 
                          duplicated(datos$`ID del Proceso`, fromLast = T),] 