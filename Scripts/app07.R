# 1 Create a script file in your GGPlot Class Project called app07.r. 
source( file="scripts/reference.R");
weatherData = read.csv( file = "data/LansingNOAA2016-3.csv",
                        stringsAsFactors = FALSE );

# 2 Create a vector that holds the quantile values for the 
#30th and 70th percentile of relHumidity.

relHumQuant = quantile(weatherData$relHum, probs = c(.30, .70));

# 3 Using the quantile values, create a new column called humidityLevel 
#that creates three levels for relHumidity

for(i in 1:nrow(weatherData))
{
  #if the value is less than or equal to the first quant level
  if(weatherData$relHum[i] <= relHumQuant[1]) # <= .3
  {
    weatherData$humidityLevel[i] = "Low";
  }
  else if(weatherData$relHum[i] <= relHumQuant[2] ) # <= .7
  {
    weatherData$humidityLevel[i] = "Medium";
  }
  else if(weatherData$relHum[i] > relHumQuant[2] )
  {
    weatherData$humidityLevel[i] = "High";
  }
  else #errors
  {
    weatherData$humidityLevel[i] = "Error";
  }
}

# 4 Create a boxplot of stnPressure vs humidityLevel
  #  Put humidityLevel in order of increasing humidity

HumPPlot = ggplot(data = weatherData) +
           geom_boxplot(mapping = aes(x=humidityLevel, y=stnPressure)) +
           theme_bw() +
           scale_x_discrete(limits = c("Low",
                                       "Medium",
                                       "High")) +
           labs(title = "Pressure vs. Humidity Level",
                subtitle = "Lansing, Michigan: 2016",
                x = "Humidity",
                y = "stnPressure");
plot(HumPPlot);
  
# 5 Add windDir as a fill to the boxplot

HumPPlot2 = ggplot(data = weatherData) +
            geom_boxplot(mapping = aes(x=humidityLevel, 
                                       y=stnPressure,
                                       fill = windDir)) +
            theme_bw() +
            scale_x_discrete(limits = c("Low",
                                        "Medium",
                                        "High")) +
            labs(title = "Pressure vs. Humidity Level",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Humidity",
                 y = "stnPressure");
plot(HumPPlot2);

# 6 Change order of windDir to North - East - South - West

HumPPlot3 = ggplot(data = weatherData) +
            geom_boxplot(mapping = aes(x=humidityLevel, 
                             y=stnPressure,
                             fill = factor(windDir,
                                           levels = c("North",
                                                      "East",
                                                      "South",
                                                      "West")))) +
            theme_bw() +
            scale_x_discrete(limits = c("Low",
                              "Medium",
                              "High")) +
            labs(title = "Pressure vs. Humidity Level",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Humidity",
                 y = "stnPressure",
                 fill = "Wind Direction");
plot(HumPPlot3);

# 7 Add a horizontal facet (along the x-axis) usi+ng windSpeedLevel

HumPPlot4 = ggplot(data = weatherData) +
            geom_boxplot(mapping = aes(x=humidityLevel, 
                             y=stnPressure,
                             fill = factor(windDir,
                                           levels = c("North",
                                                      "East",
                                                      "South",
                                                      "West")))) +
            theme_bw() +
            scale_x_discrete(limits = c("Low",
                              "Medium",
                              "High")) +
            facet_grid(facets = . ~ windSpeedLevel) +
            labs(title = "Pressure vs. Humidity Level",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Humidity",
                 y = "stnPressure",
                 fill = "Wind Direction");
plot(HumPPlot4);
