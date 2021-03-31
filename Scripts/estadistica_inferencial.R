library(openxlsx)
library(tidyverse)
library(scales)
library(cowplot)
library(ggplot2)
library(nortest)

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

# Pregunta de investigación: Si los tiempos de demora de cada transacción
# del cliente depende de la sucursal en donde se realiza el trámite.

# Supuesto de normalidad - Prueba de Lilliefors (Kolmogorov - Smirnov)
# Ho: los datos tienen una distribución normal
lillie.test(tbl_data_banco$Tiempo_Servicio_seg)

# Supuesto de homocedasticidad - Prueba de Anova
# Ho: Todas las medias son iguales.
resultados <- aov( Tiempo_Servicio_seg ~ Sucursal, data = tbl_data_banco)
summary(resultados)

tbl_data_banco %>% 
  group_by(Sucursal) %>% 
  summarise( 
    MEDIA = median(Tiempo_Servicio_seg, na.rm = TRUE),
    DESV = sd(Tiempo_Servicio_seg, na.rm = TRUE) ) %>% 
  arrange(-MEDIA)

# Pregunta de investigación: ¿Cuál sucursal es diferente?
# Ho: las medias son iguales
# Centro - Alborada -> Se rechaza Ho, son diferentes.
# Riocentro Sur - Alborada -> Se rechaza Ho, son diferentes.
# Via Daule - Alborada -> Se rechaza Ho, son diferentes.
# Centro - Mall del Sol -> Se rechaza Ho, son diferentes.
# Centro - Riocentro Sur -> Se rechaza Ho, son diferentes.
# Centro - Via Daule -> Se rechaza Ho, son diferentes.
# Mall del Sol - Riocentro Sur -> son diferentes.
# Mall del Sol - Via Daule -> son diferentes.
# Riocentro Sur - Via Daule -> son iguales.
# Mall del Sol - Alborada -> Se acepta Ho, son iguales.
pairwise.t.test(tbl_data_banco$Tiempo_Servicio_seg, tbl_data_banco$Sucursal)

# Pregunta de Investigación 2: Analizar si existen transacciones que demoran
# mas en una sucursal que en otra.
resultados <- aov(Tiempo_Servicio_seg ~ Sucursal * Transaccion, data = tbl_data_banco)
summary(resultados)

# Prueba Tukey Honest Simulation Test
tukey_objeto <- TukeyHSD(resultados, conf.level = 0.95)
str(tukey_objeto)
as.data.frame(tukey_objeto$Sucursal)
as.data.frame(tukey_objeto$Transaccion)
View(as.data.frame(tukey_objeto$`Sucursal:Transaccion`))
# Si existen transacciones que demoran mas en una sucursal que en otra.

# Pregunta de Invesigación 3: Si los niveles de satisfacción reportados dependen 
# del tipo de transacción realizada
# Prueba de hipótesis de Chi-Cuadrado (variables categóricas)
# Ho: las variables son independientes.
tb_contingencia <- table(tbl_data_banco$Satisfaccion, tbl_data_banco$Transaccion)
# Prueba de Hipótesis
chisq.test( tb_contingencia )
# Se rechaza la Ho; por lo tanto, los niveles de satisfacción si dependen del 
# tipo de transacción realizada.





