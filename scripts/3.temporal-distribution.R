# votivie kde analysis
# started 11/09/2025
# last edit 11/09/2025
# last ran 11/09/2025
library(dplyr)
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)
library("gridExtra")

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

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
plot1 <-
 LIRE_votives_clean %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Dlmatian votive inscriptions",
       subtitle = "Date ranges",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_votives_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

plot(plot1)

ggsave("output_images/LIRE_votives_dating.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_votives_scaled <- scaleweight(datsteps(LIRE_votives_clean, stepsize = 5),var = "all")

plot2 <-
  ggplot(data = LIRE_votives_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 5)+
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_votives_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of votive inscriptions",
       subtitle = paste("Using the weighted output of 'datsteps()' function ",
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

ggsave("output_images/LIRE_votives_temporal.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)