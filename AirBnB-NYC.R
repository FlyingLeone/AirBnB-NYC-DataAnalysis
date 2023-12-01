library(tidyverse)
library(knitr)
library(dplyr)
library(ggplot2)


airbnb = read_csv("C:/Users/chauh/Desktop/all projects/Data Science project/AB_NYC_2019.csv")
airbnb$last_review<-as.POSIXct(airbnb$last_review,format="%Y-%m-%d")
airbnb %>% glimpse()

#str(airbnb) can be used too if not glimpse()

NAValues <- airbnb %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))

NAValues

#There are 10052 values missing each in last review & reviews_per_month variables

sum(duplicated(airbnb))
#There is no duplicated row in this dataset.

summary(airbnb)


qtl1 = quantile(airbnb$price, 0.25)
qtl3 = quantile(airbnb$price, 0.75)
iqr = qtl3 - qtl1

lower = qtl1 - iqr * 1.5
upper = qtl3 + iqr * 1.5

lower
upper


airbnb %>%
  filter(price < lower | price > upper) %>%
  top_n(10, price) %>%
  select(neighbourhood_group, neighbourhood, price) %>%
  arrange(desc(price)) %>%
  kable(col.names = c("Neighbourhood Group", "Neighbourhood", "Price"))




#Exploratory Data Analysis & Visualizations

#Coordinates of Neighborhood Groups
ggplot(airbnb, aes(latitude, longitude, color = neighbourhood_group)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Coordinates of Airbnb Rooms According to the Neighbourhood Group",
       subtitle = "2019 NYC Airbnb Data",
       x = "Latitude",
       y = "Longitude",
       color = "Neighbourhood Group")


#Price Group Analyses of Neighborhood Groups

quant = quantile(airbnb$price, seq(0, 1, 0.2))
quant

airbnb_price_group = airbnb %>%
  mutate(price_group = case_when(
    price < quant[2] ~ "Very Low",
    price < quant[3] ~ "Low",
    price < quant[4] ~ "Medium",
    price < quant[5] ~ "High",
    TRUE ~ "Very High"
  )) %>%
  mutate(price_group = factor(price_group, levels = c("Very Low", "Low", "Medium", "High", "Very High")))

airbnb_price_group %>%
  group_by(neighbourhood_group, price_group) %>%
  summarize(counter = n())  %>%
  ggplot(., aes(x = '', y = counter, fill = price_group)) + 
  geom_bar(width = 1, stat = "identity", position = "fill") +
  coord_polar("y") +
  theme_void() +
  theme(plot.title = element_text(vjust = 0.5)) +
  facet_wrap(~neighbourhood_group) +
  labs(title = "Price Group Analyses of Neighborhood Groups",
       subtitle = "2019 NYC Airbnb Data",
       fill = "Price Group")



#In the previous analysis, we try to define the percentage of the price group in each neighborhood group. 
#To illustrate the price group change by location in each neighborhood group, the following plots are conducted.

airbnb_price_group %>%
  ggplot(., aes(latitude, longitude, color = price_group)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~neighbourhood_group, scales = "free") +
  labs(title = "Spread of the Price Group In Each Neighborhood Group",
       subtitle = "2019 NYC Airbnb Data",
       x = "Latitude",
       y = "Longtitude",
       color = "Price Group") 


#Minimum, Maximum and Average Price
airbnb %>% 
  group_by(neighbourhood_group, room_type) %>%
  summarise(min_price = min(price),
            mean_price = round(mean(price), digits = 2),
            max_price = max(price),
            average_availability = round(mean(availability_365), digits = 2),
            average_review = round(mean(number_of_reviews), digits = 2)) %>%
  
  select(neighbourhood_group,room_type, min_price, mean_price, max_price, average_availability, average_review ) %>%
  kable(col.names = c("Neighborhood Group", "Room Type", "Min Price", "Mean Price", "Max Price", "Average Availability", "Average Review"))




#The Most and Least Expensive Neighborhoods
airbnb %>%
  group_by(neighbourhood_group,neighbourhood)%>%
  summarise(mean_price = mean(price))%>%
  arrange(desc(mean_price))%>%
  head(15)%>%
  ggplot(., aes(x = reorder(neighbourhood, -mean_price) , y = mean_price, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(mean_price,digits=3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 15 Most Expensive Neighbourhoods", 
       subtitle ="2019 NYC Airbnb Data",
       x = "Neighbourhood", 
       y = "Mean price",
       fill = "Neighbourhood Group")



#Same analysis can be made for the least expensive neighborhoods.
airbnb %>%
  group_by(neighbourhood_group,neighbourhood)%>%
  summarise(mean_price = mean(price))%>%
  arrange(mean_price) %>%
  head(15)%>%
  ggplot(., aes(x = reorder(neighbourhood, mean_price) , y = mean_price, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(mean_price,digits=3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 15 Least Expensive Neighbourhoods", 
       subtitle ="2019 NYC Airbnb Data",
       x = "Neighbourhood", 
       y = "Mean price",
       fill = "Neighbourhood Group")




#The Most and Least Available Neighborhoods
airbnb %>%
  group_by(neighbourhood, neighbourhood_group)%>%
  summarise(mean_availability = mean(availability_365))%>%
  arrange(desc(mean_availability))%>%
  head(15)%>%
  ggplot(., aes(x = reorder(neighbourhood,-mean_availability) , y = mean_availability, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(mean_availability, digits = 3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 15 Most Available Neighbourhoods", 
       subtitle ="2019 NYC Airbnb Data",
       x = "Neighbourhood", 
       y = "Mean availability",
       fill = "Neighbourhood Group")


#Least available Neighborhoods
airbnb %>%
  group_by(neighbourhood, neighbourhood_group)%>%
  summarise(mean_availability = mean(availability_365))%>%
  arrange(mean_availability)%>%
  head(15)%>%
  ggplot(., aes(x = reorder(neighbourhood,mean_availability) , y = mean_availability, fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = format(mean_availability, digits = 3)), size=4, position = position_dodge(0.9),vjust = 5) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "right") +
  labs(title = "Top 15 Least Available Neighbourhoods", 
       subtitle ="2019 NYC Airbnb Data",
       x = "Neighbourhood", 
       y = "Mean availability",
       fill = "Neighbourhood Group")




#Room Type Analysis of Neighborhood Groups throuh Boxplots
ggplot(airbnb, aes(x = room_type, y = price, fill = room_type)) + scale_y_log10() + 
  geom_boxplot() +
  theme_minimal() +
  labs (x="", y= "Price") +
  facet_wrap(~neighbourhood_group) +
  facet_grid(.~ neighbourhood_group) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "right") +
  labs(title = "Room Type Analysis of Neighborhood Groups",
       subtitle = "2019 NYC Airbnb Data",
       fill = "Room Type")  

#Results indicate that:
  
#1.Each neighbourhood group's rooms are priced in the following order: full home, private room, and shared room.
#2.Manhattan has the highest average price across all accommodation types. Brooklyn, Manhattan, and Queens 
#all have comparable price structures, nevertheless.
#3.More outliers than average can be found in Brooklyn and Manhattan.




#Availability of Room Types According to the Neighborhood Groups
airbnb %>%
  ggplot(., aes(x = room_type, y = availability_365, color = room_type)) +
  geom_jitter() +
  theme_minimal() +
  theme(legend.position="bottom", plot.title = element_text(vjust = 0.5)) + 
  labs(title = "Availability of Room Types",
       subtitle = "2019 NYC Airbnb Data",
       x = "Room Type",
       y = "Availability", 
       color = " ") 



#Availability Count According to Type of Room
airbnb %>%
  ggplot(., aes(availability_365, fill = neighbourhood_group)) +
  geom_histogram(bins = 10) +
  facet_wrap(~room_type)+
  theme_minimal() +
  labs(title = "Availability Count According to Room Types",
       subtitle = "2019 NYC Airbnb Data",
       x = "Availability",
       y = "Count",
       fill = "Neighborhood Group")



#The Number of Rooms in Each Neighborhood Group
airbnb %>%
  group_by(neighbourhood_group) %>%
  summarise(count = n(), percentage = n()/nrow(airbnb)) %>%
  ggplot(., aes(x = '', y = count, fill = neighbourhood_group)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position="bottom", plot.title = element_text(vjust = 0.5)) + 
  labs(title = "The Comparison of the Number of Room",
       subtitle = "2019 NYC Airbnb Data",
       fill = "Neighborhood Group") 





#The Number of Rooms in Each Neighborhood Group By Using Room Type
airbnb %>%
  group_by(neighbourhood_group, room_type) %>%
  summarize(room_type_count = n())  %>%
  mutate(room_type_percentage = room_type_count / sum(room_type_count)) %>%
  ggplot(., aes(x = neighbourhood_group, y = room_type_percentage, fill = room_type)) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(round(room_type_percentage, 4))),
            position = position_stack(vjust = .5)) +
  theme_minimal() +
  labs(title = "The Number of Room Percentage for Different Room Type in Each Neighborhood Group",
       subtitle = "2019 NYC Airbnb Data",
       x = "Neighborhood Group",
       y = "Percentage of Room Types", 
       fill = "Room Type ")




#The Most Popular Hosts in NYC Airbnb
airbnb %>%
  group_by(neighbourhood, neighbourhood_group) %>%
  summarise(mean_review = mean(number_of_reviews)) %>%
  arrange(desc(mean_review)) %>%
  head(30) %>%
  
  ggplot(aes(x=mean_review, y = reorder(neighbourhood,mean_review), fill = neighbourhood_group)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Top 30 Neighborhood According to The Average Number of Reviews",
       subtitle = "2019 NYC Airbnb Data",
       x = "Average Number of Reviews", 
       y = "Neighborhood",
       fill = "Neighbourhood Group")


#***********************************************************************#




