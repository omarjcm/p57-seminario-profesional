library(WRS2)

dataset <- read_csv("Data/dataset.csv")
glimpse(dataset)

datos <- dataset %>% 
  mutate(obp_algorithm = parse_factor(as.character(obp_algorithm), 
                                      levels = c("G01", "G02", "G03")),
         prp_algorithm = parse_factor(as.character(prp_algorithm), 
                                      levels = c("LGAP", "SSHAPE")),
         ls_algorithm = parse_factor(as.character(ls_algorithm), 
                                     levels = c("LS_1x0", "LS_1x1", "LS_1x2", "LS_2x2")),
         num_orders = parse_factor(as.character(num_orders),
                                   levels = c('20', '30', '40', '50', '60', '70', '80', '90', '100')),
         capacity_device = parse_factor(as.character(capacity_device),
                                        levels = c('30', '45', '60', '75')))

glimpse(datos)
summary(datos)

datos <- as_tibble(datos)

datos %>% 
  ggplot( aes(x=log(time)) ) +
  geom_density()

datos %>% 
  ggplot( aes(x=obp_algorithm, y=log(time)) ) +
  geom_boxplot() +
  facet_grid(~prp_algorithm)

datos %>% 
  ggplot( aes(x=obp_algorithm, y=before_distance) ) +
  geom_boxplot() +
  facet_grid(~prp_algorithm)

# Preguntas de Investigación 1: ¿Existen diferencias entre las medias
# de las distancias con respecto a los algoritmos de agrupamiento?

# ANÁLISIS DE ESTADÍSTICA DESCRIPTIVA

datos %>% 
  ggplot( aes(x=before_distance) ) +
  geom_density()

datos %>% 
  ggplot( aes(x=obp_algorithm, y=before_distance) ) +
  stat_boxplot(geom = 'errorbar', width=0.6) +
  geom_boxplot(width = 0.6, fill="steelblue") +
  theme_bw()
  
datos %>% 
  group_by(obp_algorithm) %>% 
  summarise_at(
    vars(before_distance),
    list(
      MEAN = ~mean(., na.rm=TRUE),
      MEDIA_ACOTADA = ~mean(., na.rm=TRUE, trim=0.05),
      SD = ~sd(., na.rm=TRUE),
      MIN = ~min(., na.rm=TRUE),
      MAX = ~max(., na.rm=TRUE),
      CANTIDAD = ~n()
    )
  )

# ANÁLISIS DE ESTADÍSTICA INFERENCIAL

# Prueba con datos que tienen distribución normal:
# 1.- Supuesto de independencia.
# 2.- Supuesto de Normalidad.
# 3.- Supuesto de Homocedasticidad.

# Prueba de Kolmogorov-Smirnov
# Ho los datos tienen distribución normal
# Conclusión: los datos no tienen distribución normal.
ks.test(datos$after_distance, pnorm, mean=mean(datos$after_distance), sd=sd(datos$after_distance) )

# Prueba de Hipótesis de Lilliefors (Kolmogorov-Smirnov)
# Ho los datos tienen distribución normal
# Conclusión: los datos no tienen distribución normal.
lillie.test(datos$after_distance)

# Si no se cumple el supuesto de normalidad, se debe utilizar una prueba no paramétrica
# 1. Supuesto de Homocedasticidad
# Ho existe homogeneidad en la varianza por grupos
# Conclusión: No existe homogeneidad en la varianza por grupos.
bartlett.test(data=datos, before_distance ~ obp_algorithm)

# Prueba de Hipótesis Robusta
# Ho: las medias de cada grupo son iguales
# Conclusión: Las medias de cada grupo considerando los algoritmos de agrupamiento, 
# son diferentes.
t1way(data=datos, before_distance ~ obp_algorithm)

# Conclusion: G01 y G03 son iguales.
lincon(data = datos, before_distance ~ obp_algorithm)

# Preguntas de Investigación 2: ¿Existen diferencias entre las medias
# de las distancias con respecto a los algoritmos de recogida de pedidos?

# Análisis descriptivo e inferencial

# Preguntas de Investigación 3: ¿Existen diferencias entre las medias
# de las distancias con respecto a los algoritmos de búsqueda local y sus variaciones?

# Análisis descriptivo e inferencial

# Preguntas de Investigación 4: ¿La aplicación de los algoritmos heurísticos 
# de LS inciden en la minimización de la distancia total recorrida con respecto
# a la distancia total obtenida mediante los algoritmos de agrupamiento y recogida
# de pedidos?

datos %>% 
  summarise_at(
    vars(before_distance, after_distance),
    list(
      MEAN = ~mean(., na.rm=TRUE),
      MEDIA_ACOTADA = ~mean(., na.rm=TRUE, trim=0.05),
      SD = ~sd(., na.rm=TRUE),
      MIN = ~min(., na.rm=TRUE),
      MAX = ~max(., na.rm=TRUE),
      CANTIDAD = ~n()
    )
  ) %>% 
  view

library(ggplotly)

g1 <- datos %>% 
  ggplot( aes(x="", y=before_distance) ) +
  stat_boxplot(geom = 'errorbar', width=0.6) +
  geom_boxplot(width = 0.6, fill="steelblue") +
  theme_bw()

g2 <- datos %>% 
  ggplot( aes(x="", y=after_distance) ) +
  stat_boxplot(geom = 'errorbar', width=0.6) +
  geom_boxplot(width = 0.6, fill="steelblue") +
  theme_bw()



# Prueba de dos muestras dependientes
# Ho: las medias son iguales
# Conclusión: 
# - Las medias no son iguales.
# - dada la diferencia de las medias acotadas que sea positiva, me dice que si se está
#   optimizando la minimización de las distancias aplicando Local Search
yuend(datos$before_distance, datos$after_distance)




