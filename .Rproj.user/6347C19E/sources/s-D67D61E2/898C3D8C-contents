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

tbl_data_banco <- tbl_data_banco %>%
  mutate(Monto = str_replace(Monto, pattern=",", replacement=".") ) %>%
  mutate(Sucursal = as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion, 
                                     levels = c('Muy Malo', 'Malo', 'Regular', 'Bueno', 'Muy Bueno')),
         Monto = parse_number(Monto, locale=locale(decimal_mark = "."))) %>%
  glimpse


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

tbl_data_banco %>% 
  summarise(
    MEDIA = mean(Tiempo_Servicio_seg, na.rm=TRUE),
    MEDIA_ACOTADA = mean(Tiempo_Servicio_seg, na.rm=TRUE, trim=0.05),
    DESV_ESTANDAR = sd(Tiempo_Servicio_seg, na.rm=TRUE),
    VARIANZA = var(Tiempo_Servicio_seg, na.rm=TRUE),
    CANTIDAD = n()
  )

glimpse(tbl_data_banco)

tbl_data_banco %>% 
    summarise_at(
      vars(Tiempo_Servicio_seg, Monto),
      list(
        MEDIA = ~mean(., na.rm=TRUE),
        MEDIA_ACOTADA = ~mean(., na.rm=TRUE, trim=0.05),
        DESV_ESTANDAR = ~sd(., na.rm=TRUE),
        VARIANZA = ~var(., na.rm=TRUE),
        CANTIDAD = ~n()
      )
    ) %>%
  view()


tbl_data_banco %>% 
  summarise_if(
    is.numeric,
    list(
      MEDIA = ~mean(., na.rm=TRUE),
      MEDIA_ACOTADA = ~mean(., na.rm=TRUE, trim=0.05),
      DESV_ESTANDAR = ~sd(., na.rm=TRUE),
      VARIANZA = ~var(., na.rm=TRUE),
      CANTIDAD = ~n()
    )
  ) %>%
  view()

  
tbl_data_banco %>%
  filter(Sucursal == 62) %>%
  group_by(Transaccion, Satisfaccion) %>%
  summarise(
      MEDIA = mean(Tiempo_Servicio_seg, na.rm=TRUE),
      MEDIA_ACOTADA = mean(Tiempo_Servicio_seg, na.rm=TRUE, trim=0.05),
      DESV_ESTANDAR = sd(Tiempo_Servicio_seg, na.rm=TRUE),
      VARIANZA = var(Tiempo_Servicio_seg, na.rm=TRUE),
      CANTIDAD = n()
  )



tbl_data_banco %>%
  group_by(Satisfaccion) %>%
  summarise(
    tibble(
      Quartil=c("Min", "Q1", "Mediana", "Q3", "Max"),
      Valor = quantile(Tiempo_Servicio_seg, c(0, 0.25, 0.5, 0.75, 1))
    )
  )

hist(tbl_data_banco$Tiempo_Servicio_seg, breaks="Sturges", main = "Histograma para la variable Tiempo de Respuesta.")

ggplot(data=tbl_data_banco, aes(x=Tiempo_Servicio_seg)) +
  geom_histogram(aes(y=..count..)) +
  labs(title = "Histograma para la variable Tiempo de Respuesta.")

table(data_banco$Transaccion)
table(data_banco$Satisfaccion)

barplot( table(tbl_data_banco$Transaccion) )

ggplot(data=tbl_data_banco, aes(x=Satisfaccion)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Gráfico de barras para nivel de Satisfacción", y="Cantidad", x="Nivel de Satisfacción")

library(prettyR)

describe( tbl_data_banco, num.desc = c("mean", "sd", "median", "min", "max", "valid.n") )



