

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

# ANÁLISIS DE LA ESTADÍSTICA DESCRIPTIVA



# ANÁLISIS DE LA ESTADÍSTICA INFERENCIAL




