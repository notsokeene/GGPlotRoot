#source reference code
source(file="scripts/reference.R");

#import weatherData
weatherData = read.csv(file = "data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE);

#### Step 1: create a date column that include the year ####
theDate=weatherData$date;     # get all values from the dates column

# append (paste) "-2016" to all values in theDate
theDate = paste(theDate, "-2016", sep = "");

# Save the values in Date format
theDate = as.Date(theDate, format="%m-%d-%Y")

# Save theDate back to the data frame as a new column
weatherData$dateYr = theDate;

#### Step 2: Convert the three temperature columns in the dataframe from F to C ####

#maxTemp

themaxtemp = weatherData$maxTemp;
themaxtemp2 = 5/9*(themaxtemp - 32);
weatherData$maxTempC = themaxtemp2;

#minTemp

themintemp = weatherData$minTemp;
themintemp2 = 5/9*(themintemp - 32);
weatherData$minTempC = themintemp2;

#avgTemp

theavgtemp = weatherData$avgTemp;
theavgtemp2 = 5/9*(theavgtemp - 32);
weatherData$avgTempC = theavgtemp2;

#### Step 3: Plot ####

TempLineCelsius = ggplot( data = weatherData) +
                  geom_line( mapping = aes(x = dateYr, y = maxTempC) ) +
                  geom_line( mapping = aes(x= dateYr, y = minTempC) ) +
                  geom_smooth( mapping = aes(x = dateYr, y = avgTempC),
                               method="loess") +
                  labs(title = "Temperature vs. Date",
                       subtitle = "Lansing, Michigan: 2016",
                       x = "Date",
                       y = "Temperature (C)") +
  theme(plot.title = element_text(hjust = 1.0),
        plot.subtitle = element_text(hjust = 1.0),
        axis.text.x = element_text(color="blue"),
        axis.text.y = element_text(color = "red")) +
  scale_y_continuous(limits = c(-15,45),
                     breaks = seq(from=-15, to=45, by=20)) +
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-09-21")),
               date_breaks = "8 weeks", 
               date_labels = format("%b-%d-%Y"));

plot(TempLineCelsius);