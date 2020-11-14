# Introducción al modelamiento de datos
# Introducción al despliegue (shiny)

library(ggplot2)
library(TeachingSampling)
data("Lucy")

# aesthetic parameters: x: y: color: variable categórica
ggplot(data = Lucy, aes(x = Taxes, y = Income)) + geom_point() +
  geom_smooth(method = "lm")
proportions(table(Lucy$Level))
ggplot(data = Lucy, aes(x = Taxes, y = Income, colour = Level)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = Lucy, aes(x = Taxes, y = Income, colour = Zone)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(Lucy$Income)
# Y ~ X1 + X2 + X3
# Y es explicado (modelado) por X1, X2, ...
modelo1 <- lm(Income ~ Taxes, data = Lucy)
summary(modelo1)

# Ejercicio: realizar la nube de puntos y la linea recta de Ingreso vs Num Empleados
# realizar la regresión líneal simple Ingres ~ Num Empleados

modelo2 <- lm(Income ~ Taxes + Employees, data = Lucy)
summary(modelo2)


modelo3 <- lm(Income ~ Taxes + I(Taxes ^ 2), data = Lucy)
summary(modelo3)

# Pronostico
yhat <- predict(modelo3, Lucy)
plot(Lucy$Taxes, Lucy$Income, pch = 20)
points(Lucy$Taxes, yhat, col = "red", pch = "*")

# Modelo cúbico

modelo4 <- lm(Income ~ Taxes + I(Taxes ^ 2) + I(Taxes ^ 3), data = Lucy)
summary(modelo4)

# Pronostico
yhat <- predict(modelo4, Lucy) # En busca en Lucy la columna Taxes
plot(Lucy$Taxes, Lucy$Income, pch = 20)
points(Lucy$Taxes, yhat, col = "red", pch = "*")

