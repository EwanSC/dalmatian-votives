# votivie kde analysis
# started 11/09/2025
# last edit 11/09/2025
# last ran 11/09/2025
library(dplyr)
library(datplot)
library(ggplot2)
library(tidyverse)

# data
LIRE_votives <- read.csv("output_data/votives/LIRE_votives.csv")
LIRE_votives$variable <- "All"

# clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls
LIRE_votives_dates <- na.omit(LIRE_votives %>%
                                        select(`LIST.ID`,variable,not_before,not_after))
LIRE_votives_altered <- type.convert(LIRE_votives_dates, as.is = TRUE)    
LIRE_votives_clean <- na.omit(LIRE_votives_altered)
LIRE_votives_clean_count <- count(LIRE_votives_clean)

# check distribution of monuments using mean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
plot1 <-
 LIRE_votives_clean %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Dalmatian votive inscriptions",
       subtitle = "Date ranges (LIRE)",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_votives_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

plot(plot1)

ggsave("output_images/1.LIRE_votives_dating.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_votives_scaled <- scaleweight(datsteps(LIRE_votives_clean, stepsize = 15),var = "all")

plot2 <-
  ggplot(data = LIRE_votives_scaled,
         aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 5)+
  labs(x = "Date (BC/AD)", y = "Density",
       caption = paste("n = ",
                       LIRE_votives_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021."),
       title = "LIRE: Temporal distribution of votives",
       subtitle = paste("Weighted output of 'datsteps()' ",
                        "with stepsize of ",
                        attributes(LIRE_votives_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot(plot2)

ggsave("output_images/5.LIRE_votives_temporal.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#Compare with EDCS
# data
EDCS_votives <- read.csv("output_data/votives/EDCS_votives.csv")
EDCS_votives$variable <- "All"

# clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls
EDCS_votives_dates <- na.omit(EDCS_votives %>%
                                select(`EDCS.ID`,variable,dating_from,dating_to))
EDCS_votives_altered <- type.convert(EDCS_votives_dates, as.is = TRUE)    
EDCS_votives_clean <- na.omit(EDCS_votives_altered)
EDCS_votives_clean_count <- count(EDCS_votives_clean)

EDCS_votives_scaled <- scaleweight(datsteps(EDCS_votives_clean, stepsize = 15),var = "all")

plot3 <-
  ggplot(data = EDCS_votives_scaled,
         aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 5)+
  labs(x = "Date (BC/AD)", y = "Density",
       caption = paste("n = ",
                       EDCS_votives_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = EDCS.\n",
                       "Method = Steinmann & Weissova 2021."),
       title = "EDCS: Temporal distribution of votives",
       subtitle = paste("Weighted output of 'datsteps()' ",
                        "with stepsize of ",
                        attributes(EDCS_votives_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot(plot3)

ggsave("output_images/6.EDCS_votives_temporal.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#Compare with EDH
# data
EDH_votives <- read.csv("output_data/votives/EDH_votives.csv")
EDH_votives$variable <- "All"

# clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls
EDH_votives_dates <- na.omit(EDH_votives %>%
                                select(hd_nr,variable,dat_jahr_a,dat_jahr_e))
EDH_votives_altered <- type.convert(EDH_votives_dates, as.is = TRUE)    
EDH_votives_clean <- na.omit(EDH_votives_altered)
EDH_votives_clean_count <- count(EDH_votives_clean)

EDH_votives_scaled <- scaleweight(datsteps(EDH_votives_clean, stepsize = 15),var = "all")

plot4 <-
  ggplot(data = EDH_votives_scaled,
         aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 5)+
  labs(x = "Date (BC/AD)", y = "Density",
       caption = paste("n = ",
                       EDH_votives_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY).\n",
                       "Method = Steinmann & Weissova 2021."),
       title = "EDH: Temporal distribution of votives",
       subtitle = paste("Weighted output of 'datsteps()' ",
                        "with stepsize of ",
                        attributes(EDH_votives_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot(plot4)

ggsave("output_images/7.EDH_votives_temporal.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

# all together
library("gridExtra")

corpus_dalmatia_both <- grid.arrange(plot2, plot3, plot4, ncol = 1, nrow = 3)

ggsave("output_images/5-7.combined-votives-temporal.jpeg",
       corpus_dalmatia_both, width = 240, height = 270, unit = 'mm', dpi = 600)
