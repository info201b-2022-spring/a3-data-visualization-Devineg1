install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)


library(tidyverse)
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", 
                 stringsAsFactors = FALSE)
incarceration_data <- incarceration_data %>% 
  drop_na()

# max black female incarceration
femaleblackmax <- incarceration_data %>% 
  select(black_female_prison_pop, year) %>% 
  filter(black_female_prison_pop == max (black_female_prison_pop)) %>% 
  pull (black_female_prison_pop) 

# black filter recent year
filteredblackmaxyear <- incarceration_data %>% 
select(black_female_prison_pop, year) %>% 
  filter(year==max(year)) %>% 
  filter(black_female_prison_pop == max (black_female_prison_pop)) %>% 
  pull (black_female_prison_pop)

#black filter for year 2000 
filteredblackminyear <- incarceration_data %>% 
  select(black_female_prison_pop, year) %>% 
  filter(year==min(year)) %>% 
  filter(black_female_prison_pop == max (black_female_prison_pop)) %>% 
  pull (black_female_prison_pop)

#max white female incarceration
femalewhitemax <- incarceration_data %>% 
  select(white_female_prison_pop, year) %>% 
  filter( white_female_prison_pop == max (white_female_prison_pop)) %>% 
  pull (white_female_prison_pop)

#filtered white female incarceration for 2000
filteredwhiteyear <- incarceration_data %>% 
  select(white_female_prison_pop, year) %>% 
  filter(year==min(year)) %>% 
  filter(white_female_prison_pop == max (white_female_prison_pop)) %>% 
  pull (white_female_prison_pop)

#female latinx max 
femalelatinxmax <- incarceration_data %>% 
  select(latinx_female_prison_pop, year) %>% 
  filter(latinx_female_prison_pop == max (latinx_female_prison_pop)) %>% 
  pull (latinx_female_prison_pop)

#filtered latinex for 2000
femalelatinxyear <- incarceration_data %>% 
  select(latinx_female_prison_pop, year) %>% 
  filter(year==min(year)) %>% 
  filter(latinx_female_prison_pop == max (latinx_female_prison_pop)) %>% 
  pull (latinx_female_prison_pop)



wb_incarmax <- incarceration_data %>% 
  select(black_female_prison_pop, white_female_prison_pop, latinx_female_prison_pop, year) %>% 
  group_by(year) %>% 
  summarize(year = year, black_female_prison_pop = sum(black_female_prison_pop), white_female_prison_pop = sum(white_female_prison_pop)
            , latinx_female_prison_pop = sum(latinx_female_prison_pop))
together <- wb_incarmax %>% 
    select(black_female_prison_pop, white_female_prison_pop, latinx_female_prison_pop, year) %>% 
    gather(key = fwlprison, value= amount, -year) %>% 
    group_by(fwlprison, amount)
 

for (i in 1:length(together$fwlprison)) {
  if(together$fwlprison[i]== "black_female_prison_pop"){
    together$fwlprison[i]= "Black Female Prison Population"
  }  else if (together$fwlprison[i]== "white_female_prison_pop"){
    together$fwlprison[i]= "White Female Prison Population"
  } else {
    together$fwlprison[i]= "Latinx Female Prison Population"
  }
}

Female_race_plot <- ggplot(data = together )+
    geom_line(mapping = aes(x = year, y= amount, color = fwlprison), size = 3)+ 
    labs(title="Female Black, White, and Latinx Incarceration Over Time", x= "Year ", y= "Number of Females Incarcerated", color= "Races Incarcerated")
  

  Race_trend <- incarceration_data %>% 
    select(female_jail_pop, year)
  
  Create_trend_chart <- ggplot(data = Race_trend) +
    geom_col(mapping = aes( x= year, y= female_jail_pop))+ 
    labs(title= "Total Female Incarceration Population Over Time", 
         x= "Year", y="Female Jail Population")
      

#MAPTIMEEE 
  Filter <- incarceration_data %>%

    filter(year == "2016")
  
  ## join the map date and county to state
  black_jail_pop_map <- map_data("county") %>%
    unite(polyname, region, subregion, sep = ",") %>%
    left_join(county.fips, by = "polyname")
  
  map_data <- black_jail_pop_map %>%
    left_join(incarceration_data, by = "fips") 
  blank_theme <- theme_bw() + 
    
    theme(
      
      axis.line = element_blank(), # remove axis lines
      axis.text = element_blank(), # remove axis labels
      axis.ticks = element_blank(), # remove axis ticks
      axis.title = element_blank(), # remove axis titles
      plot.background = element_blank(), # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank() # remove border around plot
      
    )
  Map <- ggplot(map_data) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop), color = "grey",
      size = 0.3
    ) +
    coord_map() +
    scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", high = "red", low = "yellow")+
    labs(fill = "Black Jail Population Total")
  

  
