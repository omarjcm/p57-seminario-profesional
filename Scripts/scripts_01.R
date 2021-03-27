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

str(data_banco)
head(data_banco, n=5)
names(data_banco)
names(data_sucursal)

tbl_data_banco <- as_tibble( data_banco )
tbl_data_banco

select(tbl_data_banco, Transaccion, Tiempo_Servicio_seg)

tbl_data_banco %>% names 

tbl_data_banco %>% dim

tbl_data_banco %>% names %>% length 
length(names(data_banco))

tbl_data_banco %>% head(., n=5)

tbl_data_banco %>% select(Transaccion, Tiempo_Servicio_seg) 

tbl_data_banco %>% select(Tiempo_Servicio_seg) %>% boxplot

tbl_data_banco %>% select(Transaccion, Tiempo_Servicio_seg) %>% View
tbl_data_banco %>% select(-Cajero) %>% View
tbl_data_banco %>% select( contains("ID") ) %>% View
tbl_data_banco %>% select( starts_with("S") ) %>% View

tbl_data_banco %>% select( matches("r?sa") ) %>% View

tbl_data_banco %>% filter( Sucursal == 62 ) %>% View
tbl_data_banco %>% filter( Sucursal == 62 & Tiempo_Servicio_seg > 120 ) %>% View
tbl_data_banco %>% filter( Sucursal == 62 & Tiempo_Servicio_seg > 120 ) %>% View


tbl_data_banco %>% 
  select(Transaccion, Tiempo_Servicio_seg) %>% 
  filter( Sucursal == 62 ) %>% 
  View

tbl_data_banco %>% 
  filter( Sucursal == 62 ) %>% 
  select(Transaccion, Tiempo_Servicio_seg) %>% 
  View

glimpse(tbl_data_banco)

tbl_data_banco %>% 
  filter( Sucursal == 85 ) %$% 
  cor(Tiempo_Servicio_seg, as.numeric(Monto) ) %>% 
  View

tbl_data_banco %>%
  arrange(Satisfaccion) %>%
  View

tbl_data_banco %>%
  arrange(Satisfaccion, desc(Tiempo_Servicio_seg)) %>%
  View

tbl_data_banco_temp <- tbl_data_banco %>%
  mutate( Tiempo_Servicio_Min = Tiempo_Servicio_seg/60 )

glimpse(tbl_data_banco_temp)

tbl_data_banco %>%
  transmute(Tiempo_Servicio_Min = Tiempo_Servicio_seg/60)

glimpse(tbl_data_banco)


# Manipulación de Datos ---------------------------------------------------

tbl_data_banco %>%
  mutate(Monto = str_replace(Monto, pattern=",", replacement=".") ) %>%
  mutate(Sucursal = as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion, 
                                     levels = c('Muy Malo', 'Malo', 'Regular', 'Bueno', 'Muy Bueno')),
         Monto = parse_number(Monto, locale=locale(decimal_mark = "."))) %>%
  glimpse


