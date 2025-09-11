# military and civilian
# started 11/09/2025
# last edit 11/09/2025
# last ran 11/09/2025
library(dplyr)
library(data.table)

# data
LIRE_votives <- read.csv("output_data/votives/LIRE_votives.csv")

# military vs civilian
LIRE_civ_votives <- LIRE_votives %>% 
  dplyr::filter(!status_notation %like% "milites")
write.csv(LIRE_civ_votives,
          file = "output_data/votives/LIRE_civ_votives.csv")

LIRE_mil_votives <- LIRE_votives %>% 
  dplyr::filter(status_notation %like% "milites")
write.csv(LIRE_mil_votives,
          file = "output_data/votives/LIRE_mil_votives.csv")

