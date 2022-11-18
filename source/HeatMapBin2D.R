
library(tidyverse)


snow_crab_df <-read.csv("~/Documents/INFO201/project-emilynakaone/data/mfsnowcrab.csv")

snow_crab_df <- snow_crab_df %>%
  mutate(cpue = (cpue - min(cpue)) / (max(cpue) - min(cpue)))

 


spatial_heat_map_frequency <- ggplot(snow_crab_df, aes(x=latitude, y=longitude)) +
  geom_bin2d(bins = 15) +
  scale_fill_gradient(low = "light yellow", high = "red") +
  ggtitle("Frequency of Commercial Snow Crab Landings") +
  labs(x = "Latitude", y = "Longitude", subtitle = "Alaskan Eastern Bering Sea, 1975-2018", caption = "Data Source: The Resource Assessment and \n Conservation Engineering Division (RACE) of \n the Alaska Fisheries Science Center (AFSC)") +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(face="italic"), 
    plot.caption = element_text(size = 6)
  )
