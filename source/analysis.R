library(tidyverse)
library(usdata)


# The functions might be useful for A4
source("~/Documents/INFO201/assignments/a4-dtmsyi/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

incarceration_df <- read.csv("~/Documents/INFO201/assignments/a4-dtmsyi/incarceration-trends/incarceration_trends.csv")
WA_2018_incarceration_by_county_df <- incarceration_df %>% filter(year=="2018")  %>% filter(state=="WA")
WA_urban_incarceration <- WA_2018_incarceration_by_county_df  %>% filter(!is.na(county_name)) %>% group_by(county_name) %>% filter(urbanicity!="rural") %>% filter(total_pop>200000)


WA_urban_incarceration_df <- data.frame(WA_urban_incarceration)
WA_urban_incarceration_df$county_name <- gsub("County","", WA_urban_incarceration_df$county_name)

total_population_15to64_by_county_above_200000 <- ggplot(WA_urban_incarceration_df, aes(county_name, total_pop_15to64))  + geom_col()

WA_2018_incarceration_by_county_analysis <- WA_2018_incarceration_by_county_df  %>% group_by(county_name) %>% select(state, urbanicity, county_name, total_pop_15to64, black_pop_15to64, black_jail_pop, total_jail_pop)  %>% drop_na() %>%
   mutate(inmate_prop_to_general_pop=((black_pop_15to64)/(total_pop_15to64))/((black_jail_pop)/(total_jail_pop)))

WA_2018_incarceration_by_county_analysis <- do.call(data.frame,lapply(WA_2018_incarceration_by_county_analysis, function(x) replace(x, is.infinite(x), 0)))

WA_county_summary <- WA_2018_incarceration_by_county_analysis %>% group_by(county_name) %>% arrange(inmate_prop_to_general_pop) %>% select(state, county_name, urbanicity, inmate_prop_to_general_pop)


WA_urbanicity_summary <-WA_2018_incarceration_by_county_analysis %>% group_by(urbanicity) %>% summarise(mean_black_inmate_to_total_population = mean(inmate_prop_to_general_pop),
                                                                               median_black_inmate_to_total_population = median(inmate_prop_to_general_pop))




#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  
incr <- incarceration_df %>% filter(!is.na(total_jail_pop)) %>% group_by(year) %>% summarise(total_jail_pop = sum(total_jail_pop))
return(incr)   
  
  
}
get_year_jail_pop()

# This function ... <todo:  update comment>

plot_jail_pop_for_us <- 
  function()  {
  plot <- ggplot(get_year_jail_pop(), aes(year, total_jail_pop)) + 
    geom_col() + 
    scale_y_continuous(name="Total Jail Population", labels = scales::comma) + 
    labs(title = "Increase of Jail Population in the U.S. (1970 - 2018)", x = "Year")
  return(plot)   
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>

get_jail_pop_by_states <- function(states) {
  
  incr <- incarceration_df %>% filter(!is.na(total_jail_pop)) %>% group_by(state, year) %>% filter(state %in% c(states)) %>% summarise(total_jail_pop = sum(total_jail_pop))
  return(incr)
  
}

get_jail_pop_by_states(states = "WA")

plot_jail_pop_by_states <- function(states) {

 plot <- ggplot(get_jail_pop_by_states(c(states)), aes(x= year, y=total_jail_pop, group=state)) +
 geom_line(aes(linetype=state)) +
 scale_y_continuous(name="Total Jail Population", labels = scales::comma) + 
 labs(title = "Increase of Jail Population in the U.S. by State (1970 - 2018)", x = "Year") +
 geom_point(aes(shape=state))
 
 return(plot) 
}

plot_jail_pop_by_states(c("WA","CA"))
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>

grouped_by_counties <- incarceration_df %>% group_by(county_name) %>% drop_na() %>% summarize(max_pop = max(total_pop_15to64), 
                                                                                max_black_pop = max(black_pop_15to64),
                                                                                max_black_jail_pop = max(black_jail_pop),
                                                                                max_total_jail_pop = max(total_jail_pop))

ggplot(grouped_by_counties, aes(x=(max_black_pop/max_pop), y=(max_black_jail_pop/max_total_jail_pop))) + 
  geom_point(na.rm = TRUE) +
  ggtitle("Comparing Proportions of Black Population within the General Population to Black Inmate Population within the Total Inmate Population (1970-2018)", subtitle = "*Each Point Represents Data given a Specific County Jurisdiction") +
  labs(x = "Proportion of Total Black Population to Total Population ", y= "Proportion of Black Inmate Population to Total Inmate Population", caption = "The Red Line Represents the Diagonal in which the Proportion of Black Population Incarceration is Equal to Total Black Population \n Given that an Overwhelming Majority of the Data Points are Above the Diagonal, Strongly Indicates Systemic Inequity") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.caption = element_text(hjust = 0, face = "italic"), plot.subtitle = element_text(face = "italic", size = 8)) +
  xlim(0,1.0) +
  ylim(0,1.0) +
  geom_abline(slope = 1, intercept = 0, color = "red") 

  
# 1) Question(s) that I looked to gain insight into: What metrics should we compare in order to get a sense of equity/inequity in the Criminal Justice system? How does the demographic inmate proportion compare to the general demographic population with respect to Black individuals. 
# 2) Succinct Answer to prior question(s): Given that an overwhelming majority of the county data points fall above the red diagonal, there is significant cause to believe that there is inequity in the Criminal Justice system with respect to incarceration of black individuals. 
# 3) Brief Summary Paragraph: 
#I looked through the incarceration data set and found that comparing inmate demographic populations to each other might not necessarily yield the insight that I am looking for. 
# The reasoning behind this is that comparing innate demographics does not account for external contexts or confounding variables that are important when considering potential inequity. 
# For example, just because there is an overwhelming amount of black inmates proportionally in a certain county, is not necessarily a decisive indication of inequity -- especially if the general population of the county is also overwhelmingly black dominated with respect to population proportion. 
# With that in mind, I looked to find the relationship between the total proportion of black population to the general population of individuals from 15 to 64 (age) as compared to the proportion of Black inmate population.
# According to my scatterplot I rendered, it turns out that there is an overwhelming amount of counties across the U.S. in which the black inmate population proportion is larger than would be predicted assuming equitable conditions.  
  

# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas


converted_to_full_state <- incarceration_df %>% mutate(abbr2state(incarceration_df$state))


grouped_by_state <- incarceration_df %>% group_by(state, county_name) %>% select(year, state, county_name, total_pop_15to64, black_pop_15to64, black_jail_pop, total_jail_pop)  %>% drop_na() %>% 
  summarize(max_pop = sum(total_pop_15to64), 
            max_black_pop = sum(black_pop_15to64),
            max_black_jail_pop = sum(black_jail_pop),
            max_total_jail_pop = sum(total_jail_pop)) %>% 
 
  ungroup(county_name) %>% 
  summarize(max_pop = sum(max_pop), 
            max_black_pop = sum(max_black_pop),
            max_black_jail_pop = sum(max_black_jail_pop),
            max_total_jail_pop = sum(max_total_jail_pop)) 

grouped_by_state <- grouped_by_state %>% mutate(state = state.name[match(grouped_by_state$state,state.abb)]) %>% mutate(state = tolower(state))
grouped_by_state <- grouped_by_state %>% mutate(inmate_prop_to_general_pop=((grouped_by_state$max_black_jail_pop)/(grouped_by_state$max_total_jail_pop))/((grouped_by_state$max_black_pop)/(grouped_by_state$max_pop)))

# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")

# Create a blank map of U.S. states
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() # use a map-based coordinate system


# Join eviction data to the U.S. shapefile
state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(grouped_by_state, by="state") # join eviction data

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Draw the map setting the `fill` of each state using its eviction rate
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = inmate_prop_to_general_pop),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  ggtitle("Comparing Proportions of Black Population within the General Population to Black Inmate Population within the Total Inmate Population (1970-2018)", subtitle = "*Grey States Omitted from Analysis") +
  labs(caption = "Legend Indicates Hue/Color Corresponding to Respective Proportion of Black Inmate Population Proportion relative to Black General Population Proportion. Areas in Red indicate Significant Systemic Inequity") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.caption = element_text(hjust = 0, face = "italic"), plot.subtitle = element_text(face = "italic", size = 8)) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "firebrick1", high = "firebrick4") +
  labs(fill = "Legend") +
  blank_theme 

# Summary Paragraph: ]

# The Key Question I wanted to find an answer to and then visualize was: Which Regions/States have the highest Ratios of Black Inmate/Total Inmate Population relative to Black General/Total General Population? 
# After rendering the map of the U.S. overlaid with data that I wrangled in prior steps, I found that certain areas that are more rural and known to have a lower black population such as Montana, Colorado, and New Hampshire have the most extreme disparity between black inmate proportion to general black proportion of the total population.   
# However, I also found that even in areas of higher black population also had a problem with respect to this metric of systemic racial inequity. The whole country is colored in red and black...
#----------------------------------------------------------------------------#

## Load data frame ---- 


