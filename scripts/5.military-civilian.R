# military and civilian
# started 11/09/2025
# last edit 11/09/2025
# last ran 11/09/2025
library(dplyr)
library(data.table)
library(datplot)
library(ggplot2)
library(tidyverse)

# data
LIRE_votives <- read.csv("output_data/votives/LIRE_votives.csv")

# military vs civilian
LIRE_civ_votives <- LIRE_votives %>% 
  dplyr::filter(!status_notation %like% "milites")
write.csv(LIRE_civ_votives,
          file = "output_data/votives/LIRE_civ_votives.csv")
LIRE_civ_votives$variable <- "Civilian"

LIRE_mil_votives <- LIRE_votives %>% 
  dplyr::filter(status_notation %like% "milites")
write.csv(LIRE_mil_votives,
          file = "output_data/votives/LIRE_mil_votives.csv")
LIRE_mil_votives$variable <- "Military"

# clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls
LIRE_civ_dates <- na.omit(LIRE_civ_votives %>%
                                select(`LIST.ID`,variable,not_before,not_after))
LIRE_civ_altered <- type.convert(LIRE_civ_dates, as.is = TRUE)    
LIRE_civ_clean <- na.omit(LIRE_civ_altered)
LIRE_civ_count <- count(LIRE_civ_clean)

LIRE_mil_dates <- na.omit(LIRE_mil_votives %>%
                            select(`LIST.ID`,variable,not_before,not_after))
LIRE_mil_altered <- type.convert(LIRE_mil_dates, as.is = TRUE)    
LIRE_mil_clean <- na.omit(LIRE_mil_altered)
LIRE_mil_count <- count(LIRE_mil_clean)

LIRE_both <- rbind(LIRE_mil_clean, LIRE_civ_clean)

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_both_scaled <- scaleweight(datsteps(LIRE_both, stepsize = 15),var = "all")

plot1 <-
  ggplot(data = LIRE_both_scaled, 
         aes(x = DAT_step, linetype = variable, weight = weight)) +
  scale_linetype(name = "Category") +
  geom_density(alpha = 10)+
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("'Civilian' n = ",
                       LIRE_civ_count$n,
                       "; 'Military' n = ",
                       LIRE_mil_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021."),
       title = "Temporal distribution of Dalmatian votive inscriptions: Civilian vs Military",
       subtitle = paste("Using the weighted output of 'datsteps()' function ",
                        "with stepsize of ",
                        attributes(LIRE_both_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot(plot1)

ggsave("output_images/7.LIRE_civilian_military_temporal.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)