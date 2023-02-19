#File   : Module 6
#Project: Introduction to Analytics

#
#Clean canvas ----
#clears the variables
rm(list=ls())
#clears the plots
dev.off() 


library(tidyverse)
name <-c ("Final project: Gehlot")
name

df <- read.csv("C:/Users/kriti/Downloads/Weather_data.csv" )
df
summary(Weather_data)
dim(Weather_data)

#checking null values in dataset
colSums(is.na(Weather_data))

#	Created an object< counts>, that counts and lists all the weather records
counts <- length(Weather_data$Weather)
counts

weather_type <-unique(Weather_data$Weather)
weather_type

# 	Created a <df1> object that displays the different weather and the number of record of each weather type in the dataset. 

df1 <- table(Weather_data$Weather)
df1
# Created a subset, <tmp>, of just the weather variable
tmp <- subset(Weather_data, select = Weather)
tmp

# Creating a bar plot 
barplot(df1, main="Weather Type",
        ylab="COUNTS", col=c("lightgreen"),
        cex.axis=0.60, axisnames = 0.5, las=1)

#rough analysis
Rain <- filter(Weather_data, Weather == "Rain")
Rain
summary(Rain)


# Scatter plot between wind and pressure
plot(Weather_data$`Wind Speed_km/h`, Weather_data$Press_kPa, main = "Wind Speed~Pressure Scatter Plot", xlab = "Wind Speed(Km/hr)", ylab = "Pressure(kPa)")

#finding correlation between wind and pressure
cor(Weather_data$`Wind Speed_km/h`, Weather_data$Press_kPa)

# Scatter plot between wind and pressure
plot(Weather_data$Temp_C,Weather_data$Press_kPa, main = "Temp~Pressure Scatter plot", xlab = "Temp(C)", ylab= "Pressure(kPa)")
#Finding correlation between Temperature and Wind
cor(Weather_data$Temp_C,Weather_data$Press_kPa)

boxplot(Weather_data$Press_kPa)
ggplot(Weather_data) +
  aes(x = "", y = Press_kPa ) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


plot(Weather_data$`Rel Hum_%`, Weather_data$Press_kPa, main = "Humidity~Wind Scatter Plot", xlab="Humidity %", ylab = "Pressure(kPa)")
cor(Weather_data$`Rel Hum_%`, Weather_data$Press_kPa)


plot(Weather_data$`Dew Point Temp_C`, Weather_data$Visibility_km,main="Dew Point~Visibility Scatter Plot", xlab = "Dew Point", ylab= "Visibility(km)")
cor(Weather_data$`Dew Point Temp_C`, Weather_data$Visibility_km)

#created overdense plot
smoothScatter(Weather_data$`Dew Point Temp_C`, Weather_data$`Rel Hum_%`, main="Density Plot", xlab="Dew Point", ylab="Rel Humidity %",
              colramp= colorRampPalette(c("white","lightpink","lightblue"),space="Lab"),pch = 19)
cor(Weather_data$`Dew Point Temp_C`, Weather_data$`Rel Hum_%`)

# Histogram of Temperature
hist(Weather_data$Temp_C, main="Temperature", xlab="Temp(C)",ylab="Frequency",  col.main="cadetblue", col="cadetblue")

# Histogram of Temperature
hist(Weather_data$`Rel Hum_%`, main="Relative Humidity", xlab="Rel Hum(%)",ylab="Frequency",  col.main="red", col="yellow")



# creating new variable called pressure in Pa from kPa

Weather_data <- Weather_data %>% mutate(Pressure_Pa = Press_kPa * 1000)
Weather_data

#calculating absolute humidity
Absolute_hum <- 6.112*exp(17.67*Weather_data$`Dew Point Temp_C`/(Weather_data$`Dew Point Temp_C`+243.5))*Weather_data$`Rel Hum_%`*2.1674/(273.15+Weather_data$Temp_C)
head(Absolute_hum)

#adding new column to the data 
Weather_data <- Weather_data %>% mutate(Absolute_hum)
head(Weather_data)

# scatter plot between Temp and rel Humidity
smoothScatter(Weather_data$Temp_C, Weather_data$`Rel Hum_%`, main="Density Plot", xlab="Temp(C)", ylab="Rel Humidity %",
              colramp= colorRampPalette(c("white","purple","green"),space="Lab"),pch = 19)
cor(Weather_data$Temp_C, Weather_data$`Rel Hum_%`)

#scatter plot between Temp and Absolute humidity
smoothScatter(Weather_data$Temp_C, Weather_data$Absolute_hum, main="Density Plot_new", xlab="Temp(C)", ylab="Absolute Humidity(g/m3)",
              colramp= colorRampPalette(c("white","red","cadetblue"),space="Lab"),pch = 19)
cor(Weather_data$Temp_C, Weather_data$Absolute_hum)








