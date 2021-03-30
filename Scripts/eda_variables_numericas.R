library(openxlsx)
library(tidyverse)
library(scales)
library(cowplot)

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
  ggplot(aes(x=Tiempo_Servicio_seg)) +
  geom_histogram() +
  labs(title = "Histograma para Tiempo de Servicio (seg).", y="Proporción", x="Tiempo")

tbl_data_banco %>% 
  ggplot(aes(x="", y=Monto)) +
  geom_violin(fill="steelblue") +
  coord_flip() +
  labs(title="Gráfico de Violín para el Monto.", y="Monto")

tbl_data_banco %>% 
  ggplot(aes(x=Monto, y="")) +
  geom_bin2d()

tbl_data_banco %>% 
  ggplot(aes(x="", y=Monto)) +
  geom_boxplot(fill="steelblue") +
  coord_flip() + 
  labs(title="Gráficas de cajas para Monto de transacción", y = "Dolares")

tbl_data_banco %>% 
  ggplot(aes(x="", y=Monto)) +
  geom_jitter(alpha=0.05, color="gray") +
  geom_boxplot(fill="steelblue", alpha=0.1) +
  coord_flip() + 
  labs(title="Gráficas de cajas para Monto de transacción", y = "Dolares") +
  theme_light()

tbl_data_banco %>% 
  ggplot(aes(x="", y=Monto)) +
  geom_violin(fill="steelblue", alpha=0.1) +
  geom_boxplot(fill="steelblue", alpha=0.05) +
  coord_flip() +
  labs(title="Gráfico de Violín para el Monto.", y="Monto")

tbl_data_banco %>% 
  ggplot(aes(x="", y=Monto)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.005, fill="orange", alpha=0.1) +
  geom_boxplot(fill="steelblue", alpha=0.5) +
  coord_flip() +
  labs(title="Gráfico de Violín para el Monto.", y="Monto") +
  theme_light()

g1 <- ggplot(tbl_data_banco, aes(x=Monto)) +
  geom_histogram( aes(y=stat(count)/sum(count)) ) +
  labs(title="Histograma para el monto de transacción", y = "Porcentaje", x = "Dólares") +
  scale_y_continuous( labels = percent_format() ) +
  theme_bw()

g2 <- ggplot(tbl_data_banco, aes(x="", y=Monto)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom = "point", shape=18, size=5, color="orange") +
  coord_flip() + 
  theme_void()

plot_grid(g1, g2, ncol=1, rel_heights=c(9,1), align="v")
