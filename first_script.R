library(tidyverse)
library(dplyr)
library(data.table)
library(ggplot2)


data <- read.csv("Copy of EMMA.xlsb.csv")

select_data <- data |>
  select(X.NAME., Plot, Collar, Treatment, T1.mean..C., FCO2_DRY.LIN) |>
  rename(date = X.NAME.) |>
  mutate(date = mdy(date))


select_data$T1.mean..C. <- ifelse(select_data$T1.mean..C. > 37 | is.na(select_data$T1.mean..C.), 998, select_data$T1.mean..C.)
select_data <- select_data[-(217:220),]


select_data$Plot <- as.factor(select_data$Plot)
select_data$Collar <- as.factor(select_data$Collar)
select_data$Treatment<- as.factor(select_data$Treatment)


select_data$Plot <- ifelse(select_data$Plot == "plot 16",16,
                           ifelse(select_data$Plot == "plot 17",17,
                                  ifelse(select_data$Plot == "plot 19", 19,
                                         ifelse(select_data$Plot == "plot 20",20,
                                                ifelse(select_data$Plot == "plot 25", 25,
                                                       ifelse(select_data$Plot == "plot 11", 11,
                                                              ifelse(select_data$Plot == "plot 12",12,
                                                                     select_data$Plot)))))))


#--------------------------------------------------------


# select_data <- select_data |>
# group_by(Plot, Collar,Treatment,date) |>
# summarise(mean_FCO2 = mean(FCO2_DRY.LIN, na.rm = TRUE))
# 


# df_1 <- select_data |>
#   group_by(Plot,Treatment,date) |>
#   summarise(mean_FCO2 = mean(mean_FCO2))


df_2 <- select_data |>
  group_by(Treatment, date) |>
  summarise(mean_FCO2 = mean(FCO2_DRY.LIN, na.rm = TRUE)) 



ggplot(data = df_2, mapping = aes(x = date, y = mean_FCO2)) + 
  geom_point(aes(color = Treatment)) + 
  geom_line(aes(color = Treatment, group = Treatment)) +
  scale_x_date(date_labels = "%m-%d-%Y", date_breaks = "1 week") +  # Adjust breaks and labels as needed
  theme_minimal() +
  labs(x = "Date", y = "Mean FCO2", color = "Treatment", title = "Mean FCO2 over Time by Treatment")


