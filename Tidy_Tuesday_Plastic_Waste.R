#Libraries
library(tidyverse)
library(janitor)
library(gganimate)
library(maptools)
library(maps)
library(ggmap)
library(mapproj)
library(gridExtra)

#Directory

#Data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

#Gapminder data (life expectancy)
life_exp <- readr::read_csv2("life_expectancy_at_birth.csv")
life_exp <- gather(life_exp, -c(1), key = "anio", value = "life_exp")

#Merging datasets
mismanaged_vs_gdp_life_exp <-
  merge(
    mismanaged_vs_gdp,
    life_exp,
    by.x = c("Entity", "Year"),
    by.y = c("Life expectancy", "anio"),
    all.x = TRUE,
    all.y = FALSE
  )

mismanaged_vs_gdp_life_exp <-
  merge(
    mismanaged_vs_gdp_life_exp,
    waste_vs_gdp,
    by=c("Entity", "Year", "Code"))


#cleanning names
mismanaged_vs_gdp_life_exp<-clean_names(mismanaged_vs_gdp_life_exp)
mismanaged_vs_gdp_life_exp <- mismanaged_vs_gdp_life_exp %>% filter(year==2010)
mismanaged_vs_gdp_life_exp$entity[mismanaged_vs_gdp_life_exp$entity=="United States"] <- "USA"

#Map data
world_map <- map_data("world") %>% filter(region!="Antarctica")
map_waste <- mismanaged_vs_gdp_life_exp %>% right_join(world_map,by = c("entity" ="region"))
  
map_waste <- map_waste %>% select(entity,
                                  code,
                                  year,
                                  long, 
                                  lat,
                                  group,
                                  order, 
                                  subregion,
                                  mismanaged_per_capita = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
                                  waste_per_capita = per_capita_plastic_waste_kilograms_per_person_per_day, 
                                  life_exp,
                                  gdp_per_capita = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
                                  population = total_population_gapminder_x)
  
map_waste2 <- map_waste %>% mutate(life_expectancy = case_when(
  life_exp<68.90 ~ "Low Life Expectancy",
  life_exp>68.90 & life_exp<80.50 ~ "Medium Life Expectancy",
  life_exp > 80.5 ~ "High Life Expectancy")) %>% 
  mutate(percentage_mismanaged=(mismanaged_per_capita/waste_per_capita)*100) 

map_waste2$life_expectancy <- as.factor(map_waste2$life_expectancy)
map_waste2$life_expectancy <- factor(map_waste2$life_expectancy, levels=c("Low Life Expectancy",
                                                                          "Medium Life Expectancy",
                                                                          "High Life Expectancy"))
#Map
anim<-ggplot() +
  geom_map (data = world_map[order(world_map$order),],
            map = world_map[order(world_map$order),],
            aes(x = long,
                y = lat,
                map_id = region)) +
  geom_polygon(data = map_waste2[order(map_waste$order), ],
               aes(
                 x = long,
                 y = lat,
                 map_id = entity,
                 group = group,
                 fill = percentage_mismanaged
               )) +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(title="% Per capita mismanaged plastic waste per person per day", 
       subtitle="Life Expectancy: {current_frame}", 
       caption="TidyTuesday, @karbartolome", 
       fill="% Mismanaged Waste")+ 
  theme(legend.position = "bottom", 
        plot.title = element_text(size=17), 
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=12),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())+
  transition_manual(life_expectancy)+
  ease_aes('cubic-in-out')

anim_save("Mismanaged_waste.gif", animation=anim, duration=7)

