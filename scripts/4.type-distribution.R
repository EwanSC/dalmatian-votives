# monument kde analysis
# started 11/09/2025
# last edit 11/09/2025
# last ran 11/09/2025
library(dplyr)
library(datplot)
library(ggplot2)
library(tidyverse)

# data
LIRE_votives <- read.csv("output_data/votives/LIRE_votives.csv")
LIRE_votives$count<- 1

LIRE_votives_types_count <- LIRE_votives %>%
  count(type_of_monument_clean, sort = TRUE)

write.csv(LIRE_votives_types_count,
          file = "output_data/dalmatia/LIRE_votives_types.csv")

# filter
LIRE_votives_filtered <- filter(LIRE_votives,
                                type_of_monument_clean == "altar"|
                                  type_of_monument_clean == "tabula"|
                                  type_of_monument_clean == "statue base"|
                                  type_of_monument_clean == "relief")

LIRE_votives_altar <- filter(LIRE_votives,
                                type_of_monument_clean == "altar")
LIRE_votives_altar_count <- count(LIRE_votives_altar)

LIRE_votives_tabula <- filter(LIRE_votives,
                                type_of_monument_clean == "tabula")
LIRE_votives_tabula_count <- count(LIRE_votives_tabula)

LIRE_votives_statue_base <- filter(LIRE_votives,
                                   type_of_monument_clean == "statue base")
LIRE_votives_statue_base_count <- count(LIRE_votives_statue_base)

LIRE_votives_relief <- filter(LIRE_votives,
                              type_of_monument_clean == "relief")
LIRE_votives_relief_count <- count(LIRE_votives_relief)

# clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls
LIRE_votives_dates <- na.omit(LIRE_votives_filtered %>%
                                select(`LIST.ID`,type_of_monument_clean,not_before,not_after))

LIRE_votives_altered <- type.convert(LIRE_votives_dates, as.is = TRUE)    

LIRE_votives_clean <- na.omit(LIRE_votives_altered)

LIRE_votives_types <- LIRE_votives_clean %>%
  rename(variable = type_of_monument_clean)

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_votives_types_scaled <- scaleweight(datsteps(LIRE_votives_types,
                                                        stepsize = 15),
                                               var = "all")

plot1 <-
 ggplot(data = LIRE_votives_types_scaled,
       aes(x = DAT_step, linetype = variable, weight = weight)) +
  scale_linetype(name = "Monument type") +
  geom_density(alpha = 10) +
  labs(x = "Date (BCE/CE)", y = "Relative density of monument type",
       caption = paste("Altar n = ",
                       LIRE_votives_altar_count$n,
                       "; tabula n = ",
                       LIRE_votives_tabula_count$n,
                       "; statue base n = ",
                       LIRE_votives_statue_base_count$n,
                       "; relief n = ",
                       LIRE_votives_relief_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021."),
       title = "Temporal distribution of Dalmatian votive inscription monnument types",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_votives_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

plot(plot1)

ggsave("output_images/6.LIRE_votives_types_temporal.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
