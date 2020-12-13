#####################--EDA--#############
install.packages('dplyr')
library(dplyr)
library(maps)
library(ggthemes)
library(ggplot2)
library(mapdata)
library(purrr)

#---- Read data ----
setwd("/Users/weiweixie/Desktop/Fall 2020/575 Data Science/Final project/data")
state = read.delim("state.txt", sep = "|")
climate = read.csv("/Users/weiweixie/Desktop/Fall 2020/575 Data Science/Final project/data/climdiv_state_year.csv", 
                sep = ",", header = TRUE)
fire_data = read.csv("/Users/weiweixie/Desktop/Fall 2020/575 Data Science/Final project/data/fire_data4.csv", 
                     sep = ",", header = TRUE)

#---- Data preprocess ----
climate = climate %>% filter(year>1994 & year<2005) %>% left_join(state, by = c("fips" = "STATE"))
climate = climate[, -c(1,6,7)]
fire = fire_data%>% left_join(climate, by = c("STATE" = "STUSAB", "FIRE_YEAR" = "year"))
fire$DAYS_LAST = fire$CONT_DATE-fire$DISCOVERY_DATE
dat = fire[complete.cases(fire),]

head(dat)
#---- Stratified sampling ----
sample_dat = dat%>% group_by(FIRE_SIZE_CLASS, FIRE_YEAR, STAT_CAUSE_CODE, STATE, FIPS_NAME)%>%slice_sample(n=25)
write.csv(sample_dat, file = "sample_data2.csv", row.names = F)

fires_data = read.csv("/Users/weiweixie/Desktop/Fall 2020/575 Data Science/Final project/data/sample_data2.csv")
attach(fires_data)
head(fires_data)
###### fires by year
fires_data %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = FIRE_YEAR, y = n/100)) + 
  geom_bar(stat = 'identity', fill = 'pink') + 
  labs(x = ',', y = 'Number of Wildfires (hundreds)', title = ' US Wildfires by Year') 
  #geom_smooth(method = 'lm', se = F, linetype = 'dashed', size = 0.4, color = 'purple') 

###### US Wildfires by Cause
fires_data %>%
  group_by(STAT_CAUSE_DESCR) %>%
  summarize(n_fires = n()/1000) %>%
  ggplot(aes(x = reorder(STAT_CAUSE_DESCR, n_fires), y = n_fires)) +
  geom_bar(stat = 'identity', fill = 'pink') + 
  coord_flip() + 
  labs(x = '', y = 'Number of fires (thousands)', title = 'US Wildfires by Cause 1995 to 2004')


###### fires by size
fires_data %>%
  group_by(FIRE_SIZE_CLASS) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = FIRE_SIZE_CLASS, y = n/100)) +
  geom_bar(stat = 'identity', fill = 'pink') +
  labs(x = 'Fire Size(acres)', y = 'Number of Wildfires(hundres)', title = 'US Wildfires by Size Class')



#######--------------Mapping---------------##################
###### Wildfires by states
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))
fires_data$region <- map_chr(fires_data$STATE, function(x) { tolower(state.name[grep(x, state.abb)]) })
state_map <- map_data('state')

fires_data %>%
  select(region) %>%
  group_by(region) %>%
  summarize(n = n()) %>%
  right_join(state_map, by = 'region') %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon() +
  geom_path(color = 'white') +
  scale_fill_continuous(low = 'pink',
                        high = 'darkred',
                        name = 'Number of Wildfires') +
  theme_map() +
  coord_map('albers', lat0 = 30, lat1 = 40) +
  ggtitle("US Wildfires from 1995 to 2004") +
  theme(plot.title = element_text(hjust = 0.5))

####### fires in WA by county
str(fires_data)

wa = subset(fires_data, region == 'washington')
head(fires_data)
head(wa)

wa_county = map_data('county', 'washington') %>%
  as.data.frame() %>%
  filter(region == 'washington')

state_map1 = state_map %>% filter(region == 'washington')
  
wa %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(n = n()) %>%
  right_join(wa_county, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon() +
  geom_path(color = 'white') +
  scale_fill_continuous(low = 'pink',
                        high = 'darkred',
                        name = 'Number of Wildfires') +
  theme_map() +
  coord_map('albers', lat0 = 30, lat1 = 40) +
  ggtitle("Wildfires in WA from 1995 to 2004") +
  theme(plot.title = element_text(hjust = 0.4))


####### fires in OR by county
state_map2 = state_map %>% filter(region == 'oregon')

or_county = map_data('county', 'oregon') %>%
  as.data.frame() %>%
  filter(region == 'oregon')

fires_data %>%
  filter(region == 'oregon') %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(n = n()) %>%
  right_join(or_county, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon() +
  geom_path(color = 'white') +
  scale_fill_continuous(low = 'pink',
                        high = 'darkred',
                        name = 'Number of Wildfires') +
  theme_map() +
  coord_map('albers', lat0 = 30, lat1 = 40) +
  ggtitle("Wildfires in OR from 1995 to 2004") +
  theme(plot.title = element_text(hjust = 0.4))
  
####### fires in CA by county
state_map3 = state_map %>% filter(region == 'california')

ca_county = map_data('county', 'california') %>%
  as.data.frame() %>%
  filter(region == 'california')

fires_data %>%
  filter(region == 'california') %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(n = n()) %>%
  right_join(ca_county, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon() +
  geom_path(color = 'white') +
  scale_fill_continuous(low = 'pink',
                        high = 'darkred',
                        name = 'Number of Wildfires') +
  theme_map() +
  coord_map('albers', lat0 = 30, lat1 = 40) +
  ggtitle("Wildfires in CA from 1995 to 2004") +
  theme(plot.title = element_text(hjust = 0.4))

















