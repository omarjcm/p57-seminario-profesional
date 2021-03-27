library(tidyverse)

parse_number( c('$1,234.5', '$3,000.5') )
parse_double( c('1234.5', '3000.5') )
parse_integer( c('$1,234.5', '$3,000.5') )
parse_integer( c('1234.5', '3000.5') )

as.character(lubridate::now())

factor(c('Alto', 'Bajo', 'Medio', 'Alto'), 
       levels = c('Alto', 'Medio', 'Bajo'),
       ordered = T)

# No se debe considerar, se deben especifcar todos los factores.
factor(c('Alto', 'Bajo', 'Medio Alto', 'Alto'), 
       levels = c('Alto', 'Medio', 'Bajo'))


parse_date("2021-03-23")
parse_date("2021/03/23")


