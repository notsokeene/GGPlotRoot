# Create a script file in your GGPlot Class Project called app08.r.
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#Create months column in the dataset
dates = as.Date(weatherData$dateYr);  # save the date column to a vector
months = format(dates, format="%b");  # extract the month -- save to vector
weatherData$month = months;           # save months to data frame as new column


# Create a bar plot of Cooling Days (coolDays) for each month
#create a dummy variable for Cooling days 
for (i in 1:length(weatherData$coolDays)) 
{
  if(weatherData$coolDays[i] > 0)
  {
    weatherData$coolDaysCount[i] = 1;
  }
  else if(weatherData$coolDays[i] == 0)
  {
    weatherData$coolDaysCount[i] = 0;
  }
  else
  {
    weatherData$coolDaysCount[i] = "Error";
  }
}
#Create a dummy variable for HeatingDays
for (i in 1:length(weatherData$coolDays)) 
{
  if(weatherData$heatDays[i] > 0)
  {
    weatherData$heatDaysCount[i] = 1;
  }
  else if(weatherData$heatDays[i] == 0)
  {
    weatherData$heatDaysCount[i] = 0;
  }
  else
  {
    weatherData$heatDaysCount[i] = "Error";
  }
}

# Create a vector that consists of the first two values in the weatherType column (i.e., the new vector will have at most one weather condition for the day).
weatherType = weatherData$weatherType;
firstWeatherType = substr(weatherType, 0, 2);
weatherData$firstWeatherType = firstWeatherType;
for (i in 1:length(weatherData$firstWeatherType)) 
{
  if(weatherData$firstWeatherType[i] == "")
  {
    weatherData$firstWeatherType[i] = "No code";
  }
  else
  {
    weatherData$firstWeatherType[i] = weatherData$firstWeatherType[i];
  }
}
# Customize the colors of the 8 weather conditions (one of the conditions is empty).  Do this using component scale_fill_manual and the subcomponent values, which will be set a to a vector.  Note: this is the discrete version of the component scale_fill_gradient.
colorcol = c(
  "No code" = "lightblue",
  "BR" = "yellow",
  "FG" = "red",
  "FZ" = "green",
  "HZ" = "orange",
  "RA" = "blue",
  "SN" = "purple",
  "TS" = "grey"
);

weatherTypeFact = factor(weatherData$firstWeatherType,
                    c("BR",
                      "FG",
                      "FZ",
                      "HZ",
                      "RA",
                      "SN",
                      "TS",
                      "No code"
                    ))

coolPlot = ggplot( data=weatherData ) +
           geom_col(mapping=aes(x=month, y = coolDaysCount, fill = weatherTypeFact),
                    width = 0.6) +
           scale_x_discrete(limits = month.abb) +
           theme_bw()+
           scale_fill_manual(values = colorcol,
                             aesthetics = c("colour", "fill")) +
           labs(title = "Monthly Cooling Days",
                subtitle = "Lansing, Michigan; 2016",
                x = "Month",
                y = "Number of Cooling Days");
plot(coolPlot);

# Create a bar plot of Heating Days (heatDays) for each month
# Add a title, and put appropriate labels on the axes.
# Change the bar width to 0.6
# Create a vector that consists of the first two values in the weatherType column.
# Add a horizontal line to the plot that represent the sum of all coolDays for the year (it should be in the 800s)
# Add a label (or, annotate) that describes what the line represents.





# Combine Heating and Cooling Days into one barplot
# Add a title, and put appropriate labels on the axes.
# Change the bar widths of both to 0.4
# Change the bar fill to solid colors: red for heatDays , blue for coolDays