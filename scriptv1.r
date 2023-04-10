### ### ### ### ### ### ### ###
#         Alejandro GL        #
#           Decopling         #
### ### ### ### ### ### ### ###

# Limpiamos nuestro entorno
rm(list=ls())

# Paquetes 
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}
required.packages <- c('tidyverse', 'readxl', 'stringr', 'tseries', 'lmtest', 'vars', 'fBasics', 
                       'zoo', 'xts', 'ARDL', 'normtest', 'urca', 'dynamac', 'forecast', 'TSstudio',
                       'seasonal')
install(required.packages)

#se carga la base que tiene los datos trimestrales de 2000 a 2021 de PIB, GDP y X_manuf
Base2000 <- read_excel("C:/Users/Rolando Cordera/Dropbox/Desacoplamiento/V1/Base2000.xlsx")
attach(Base2000)


#Se le da formato de serie de tiempo, se reescribe sobre la variable 
pib <- ts(Base2000$pib, start = c(2000, 1), frequency = 4)
x_manuf <- ts(Base2000$x_manuf, start = c(2000, 1), frequency = 4)
gdp <- ts(Base2000$gdp, start = c(2000, 1), frequency = 4)


plot(pib, main = "Gráfica de las tres variables",
     ylab = "Valores", col = "red")
lines(x_manuf, col = "blue")
lines(gdp, col = "green")
legend("topright", legend = c("PIB", "X_Manuf", "GDP"),
       col = c("red", "blue", "green"), lty = 1)


modelo1 <- auto_ardl(pib ~ gdp, data= Base2000, max_order = 5)
names(modelo1)
modelo1$top_orders