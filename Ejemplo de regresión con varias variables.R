setwd("~/Laboral2020/Konrad Lorenz/Semestre2/AnálisisCasos/Ejemplo Regresión Lineal")
dir()

datos <- read.table("s11_2019I.txt", sep = "|", header = T)

summary(datos$PUNT_GLOBAL)

boxplot(PUNT_GLOBAL ~ FAMI_TIENEINTERNET, data = datos)

boxplot(PUNT_GLOBAL ~ COLE_JORNADA, data = datos)


table(datos$FAMI_TIENEINTERNET, useNA = "always")
datos$internet <- ifelse(datos$FAMI_TIENEINTERNET == "-", "NS/NR", 
                         datos$FAMI_TIENEINTERNET)
table(datos$internet, useNA = "always")
table(datos$COLE_JORNADA, useNA = "always")


modelo1 <- lm(PUNT_GLOBAL ~ FAMI_TIENEINTERNET, data = datos)
summary(modelo1)
              
table(datos$COLE_JORNADA, useNA = "always")
modelo2 <- lm(PUNT_GLOBAL ~ FAMI_TIENEINTERNET + COLE_JORNADA, data = datos)
summary(modelo2)

datos$pron_modelo2 <-  predict(modelo2, datos)


# Otro ejemplo
cor(datos$PUNT_MATEMATICAS, datos$PUNT_C_NATURALES)
modelo3 <- lm(PUNT_MATEMATICAS ~ FAMI_TIENEINTERNET + COLE_JORNADA +
                PUNT_C_NATURALES,  data = datos)
summary(modelo3)
datos$pron_modelo3 <- predict(modelo3, datos)
