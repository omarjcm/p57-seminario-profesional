library(openxlsx)
library(tidyverse)
library(scales)
library(cowplot)
library(ggplot2)

data_banco <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data")
data_sucursal <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data_Sucursal")
data_cajero <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data_Cajero")

tbl_data_banco <- as_tibble( data_banco )
tbl_data_sucursal <- as_tibble( data_sucursal )
tbl_data_cajero <- as_tibble( data_cajero )

tbl_data_banco <- tbl_data_banco %>%
  mutate(Monto = str_replace(Monto, pattern=",", replacement=".") ) %>%
  mutate(Sucursal = as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion, 
                                     levels = c('Muy Malo', 'Malo', 'Regular', 'Bueno', 'Muy Bueno')),
         Monto = parse_number(Monto, locale=locale(decimal_mark = ".")))

tbl_data_sucursal <- tbl_data_sucursal %>% 
  mutate(ID_Sucursal = as.character(ID_Sucursal))

tbl_data_banco <- tbl_data_banco %>% 
  mutate(Sucursal = as.character(Sucursal))

tbl_data_banco <- tbl_data_banco %>% 
  rename("ID_Sucursal"="Sucursal") %>% 
  left_join(tbl_data_sucursal, by=c("ID_Sucursal"))

glimpse(tbl_data_banco)
glimpse(tbl_data_sucursal)
glimpse(tbl_data_cajero)

tbl_data_banco %>% 
  ggplot(aes(x=Sucursal)) +
  geom_bar() +
  labs(title = "Cantidad de transacciones por Sucursal", y="Cantidad")

tbl_data_banco %>% 
  ggplot(aes(x=Sucursal)) +
  geom_bar() +
  geom_text(aes(label=stat(count)), stat = "count", nudge_y = 500) +
  labs(title = "Cantidad de transacciones por Sucursal", y="Cantidad")

tbl_data_banco %>% 
  ggplot(aes(x=forcast::fct_infreq(Sucursal))) +
  geom_bar() +
  geom_text(aes(label=stat(count)), stat = "count", nudge_y = 500) +
  labs(title = "Cantidad de transacciones por Sucursal", y="Cantidad")

tbl_data_banco %>% 
  group_by(Sucursal) %>% 
  summarise(Frec=n()) %>% 
  ggplot(aes(x=reorder(Sucursal, -Frec), y=Frec)) +
  geom_bar(stat="identity") +                              
  labs(title = "Cantidad de transacciones por Sucursal", y="Cantidad")

tbl_data_banco %>% 
  group_by(Sucursal) %>% 
  summarise(Frec=n()) %>% 
  ggplot(aes(x=reorder(Sucursal, -Frec), y=Frec)) +
  geom_bar( aes(fill=Sucursal))  +
  coord_flip() +
  labs(title = "Cantidad de transacciones por Sucursal", y="Cantidad") +
  theme_bw()

tbl_data_banco %>% 
  ggplot(aes(x=forcats::fct_infreq(Sucursal))) +
  geom_bar( aes(fill=Sucursal) ) +
  labs(title = "Cantidad de transacciones por Sucursal", y="Cantidad", x="Sucursal") 

tbl_data_banco %>% 
  group_by(Sucursal) %>% 
  summarise(Frec=n()) %>% 
  mutate( Prop=Frec/sum(Frec) ) %>% 
  ggplot( aes(x="", y=Prop, fill=Sucursal) ) +
  geom_bar(stat="identity", width = 1, color="white") +
  coord_polar("y", start = 0) +
  theme_void()

tbl_data_banco %>% 
  group_by(Sucursal) %>% 
  summarise( Frec=n() ) %>% 
  arrange(-Frec) %>% 
  mutate(
    Prop=round( Frec/sum(Frec)*100, 2 ),
    PosLab= cumsum(Prop) - 0.45*Prop
  ) %>% 
  ggplot( aes(x="", y=Prop, fill=reorder(Sucursal, Frec)) ) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y=PosLab, label=Prop), color="white", size=3) +
  labs(title = "Proporción de transacciones por Sucursal", fill="Sucursal")
