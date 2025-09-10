# votives from datasets
# started 10/09/2025
# last edit 10/09/2025
# last ran 10/09/2025
library(dplyr)
library(sqldf)
library(data.table)

# EDH
EDH_Dalmatia <- read.csv("output_data/dalmatia/EDH_Dalmatia.csv")
EDH_votives <- EDH_Dalmatia %>%
  filter(i_gattung_sort_de %in% c("22"))
write.csv(EDH_votives,
          file = "output_data/votives/EDH_votives.csv")

# EDCS
EDCS_Dalmatia <- read.csv("output_data/dalmatia/EDCS_Dalmatia.csv")
EDCS_votives <- EDCS_Dalmatia %>% dplyr::filter(status %like% "tituli sacri")
write.csv(EDCS_votives,
          file = "output_data/votives/EDCS_votives.csv")

# LIRE
LIRE_Dalmatia <- read.csv("output_data/dalmatia/LIRE_Dalmatia.csv")
LIRE_votives <- LIRE_Dalmatia %>%
  filter(type_of_inscription_auto %in% c("votive inscription"))
write.csv(LIRE_votives,
          file = "output_data/votives/LIRE_votives.csv")

# count
EDH_votives_count <- count(EDH_votives)
EDCS_votives_count <- count(EDCS_votives)
LIRE_votives_count <- count(LIRE_votives)

EDH_votives_count$database <- "EDH"
EDCS_votives_count$database <- "EDCS"
LIRE_votives_count$database <- "LIRE"

count_list <- list(LIRE_votives_count, EDCS_votives_count, EDH_votives_count)      

database_votives_count <- Reduce(function(x, y) merge(x, y, all=TRUE), count_list) 

write.csv(database_votives_count,
          file = "output_data/votives/database_votives_count.csv")
