library("openxlsx")
library("tidyverse")
library("magrittr")
library("modeest")
library("fdth")
library("prettyR")

data_banco <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data")
data_sucursal <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data_Sucursal")

tbl_data_banco <- as_tibble( data_banco )

tbl_data_banco <- tbl_data_banco %>%
  mutate(Monto = str_replace(Monto, pattern=",", replacement=".") ) %>%
  mutate(Sucursal = as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion, 
                                     levels = c('Muy Malo', 'Malo', 'Regular', 'Bueno', 'Muy Bueno')),
         Monto = parse_number(Monto, locale=locale(decimal_mark = "."))) 

data_sucursal <- data_sucursal %>%
  mutate(ID_Sucursal = as.character(ID_Sucursal))

data_banco <- data_banco %>%
  mutate(Sucursal = as.character(Sucursal))

data_banco <- data_banco %>%
  rename("ID_Sucursal"="Sucursal") %>%
  left_join(data_sucursal, by = c("ID_Sucursal"))

glimpse(data_sucursal)
glimpse(data_banco)

tbl_data_banco <- as_tibble( data_banco )
tb_contingencia <- table(tbl_data_banco$Sucursal, tbl_data_banco$Satisfaccion)
addmargins(tb_contingencia)

# Probabilidades de la tabla de contingencia

tb_contigencia_pro <- prop.table(tb_contingencia)
addmargins(tb_contigencia_pro*100)

# Probabilidades condicionales
tb_contingencia_pro_cond <- prop.table( tb_contingencia, 1 ) * 100
addmargins(tb_contingencia_pro_cond)

plot(tb_contingencia, xlab="Sucursal", ylab = "Satisfacción",
     main="Gráfico de Barras de Sucursal vs. Satisfacción")
