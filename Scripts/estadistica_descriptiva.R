library("openxlsx")
library("tidyverse")
library("magrittr")
library("modeest")
library("fdth")
library("prettyR")

library(readr)
dataset <- read_csv("Data/dataset.csv")
View(dataset)

data_banco <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data")
data_sucursal <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data_Sucursal")

tbl_data_banco <- as_tibble( data_banco )


# Medidas de Tendencia Central --------------------------------------------

tbl_data_banco %$% mean(Tiempo_Servicio_seg, na.rm = TRUE)
tbl_data_banco %$% mean(Tiempo_Servicio_seg, trim=0.05, na.rm = TRUE)

tbl_data_banco %$% median(Tiempo_Servicio_seg, na.rm = TRUE)

tbl_data_banco %$% 
  MASS::huber(Tiempo_Servicio_seg) %>% 
  as.data.frame() %>%
  select(mu)

library(modeest)

mlv(tbl_data_banco$Tiempo_Servicio_seg)

tbl_data_banco %$% min(Tiempo_Servicio_seg)
min(tbl_data_banco$Tiempo_Servicio_seg, na.rm = TRUE)

tbl_data_banco %$% max(Tiempo_Servicio_seg, na.rm = TRUE)
#Quartiles
tbl_data_banco %$% quantile(Tiempo_Servicio_seg, probs=c(0.25, 0.50, 0.75))
#Deciles
tbl_data_banco %$% quantile(Tiempo_Servicio_seg, probs=seq(from=0.1, to=1, by=0.1))

tbl_data_banco %$% boxplot(Tiempo_Servicio_seg)
tbl_data_banco %$% boxplot(Tiempo_Servicio_seg, main="Boxplot para tiempo de servicio (seg).", ylab="Tiempo")

library(ggplot2)

ggplot(data = tbl_data_banco, aes(x="", y=Tiempo_Servicio_seg)) +
  geom_boxplot() +
  coord_flip()

# Medidas de Dispersión ---------------------------------------------------

tbl_data_banco %$% var(Tiempo_Servicio_seg, na.rm=TRUE)

desarrolladores_latinos = c(1200, 1200, 800, 800, 600, 1000, 1000, 600, 1000, 600, 800, 1200, 10000)
mean(desarrolladores_latinos, na.rm = TRUE)
var(desarrolladores_latinos, na.rm = TRUE)
sd(desarrolladores_latinos, na.rm = TRUE)
boxplot(desarrolladores_latinos)

desarrolladores_latinos_sin = c(1200, 1200, 800, 800, 600, 1000, 1000, 600, 1000, 600, 800, 1200)
mean(desarrolladores_latinos_sin, na.rm = TRUE)
var(desarrolladores_latinos_sin, na.rm = TRUE)
sd(desarrolladores_latinos_sin, na.rm = TRUE)
boxplot(desarrolladores_latinos_sin)

desarrolladores_canadienses = c(9000, 8000, 10000, 12000, 11000, 13000, 10000, 8000, 10000, 800, 9000)
mean(desarrolladores_canadienses, na.rm = TRUE)
var(desarrolladores_canadienses, na.rm = TRUE)
sd(desarrolladores_canadienses, na.rm = TRUE)
boxplot(desarrolladores_canadienses)

desarrolladores_canadienses_sin = c(9000, 8000, 10000, 12000, 11000, 13000, 10000, 8000, 10000, 9000)
mean(desarrolladores_canadienses_sin, na.rm = TRUE)
var(desarrolladores_canadienses_sin, na.rm = TRUE)
sd(desarrolladores_canadienses_sin, na.rm = TRUE)
boxplot(desarrolladores_canadienses_sin)
