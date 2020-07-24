source(file="scripts/reference.R");
weatherData = read.csv( file = "data/LansingNOAA2016-3.csv",
                        stringsAsFactors = FALSE);

#Create a vector that holds the quantile values for the 20, 40, 60 and 80th 
#percentile of stnPressure

stnPressureQuant = quantile(weatherData$stnPressure, 
                            probs = c(.20, .40, .60, .80));

#Create a new column called pressure level w/ five evenly spaced levels
#Level names: "Low", "Medium-Low", "Medium", "Medium-High", "High"

for(day in 1:nrow(weatherData)) #nrow(weatherData = 366)
{
  #if the value in pressue is less than or equal to the low quant value
  if(weatherData$stnPressure[day] <= stnPressureQuant[1]) # <= 28.9
  {
    weatherData$pressureLevel[day] = "Low";
  }
  # if the value in pressure is in second lowest
  #quant value
  else if(weatherData$stnPressure[day] > stnPressureQuant[1] &&
          weatherData$stnPressure[day] <= stnPressureQuant[2]) 
    # 28.9 < P <= 29.1
  {
    weatherData$pressureLevel[day] = "Medium-Low";
  }
  #the value in pressure is in medium level
  else if(weatherData$stnPressure[day] > stnPressureQuant[2] &&
          weatherData$stnPressure[day] <= stnPressureQuant[3]) 
  {
    weatherData$pressureLevel[day] = "Medium";
  }
  #medium high
  else if(weatherData$stnPressure[day] > stnPressureQuant[3] &&
          weatherData$stnPressure[day] <= stnPressureQuant[4]) 
  {
    weatherData$pressureLevel[day] = "Medium-High";
  }
  #High
  else 
  {
    weatherData$pressureLevel[day] = "High";
  }
}

#Make a boxplot of winSuspeed vs pressure level
pressureLevelFact = factor(weatherData$pressureLevel,
                     levels=c("Low", 
                              "Medium-Low", 
                              "Medium", 
                              "Medium-High", 
                              "High"));


WvP = ggplot( data = weatherData ) +
      geom_boxplot(mapping = aes(x=pressureLevelFact, y = windSusSpeed) ) +
      theme_bw() +
      labs(
        title = "Change in WindSusSpeed vs. pressure Level",
        subtitle = "Lansing, Michigan: 2016",
        x= "Pressure Level",
        y= "Wind Sus Speed"
      );

plot(WvP);

#Find the dates for the three outliers at the lowest pressure level

lowPressureVals= which(weatherData$pressureLevel == "Low" & 
                         weatherData$windSusSpeed >= 40);
lowOutliersDates = weatherData[lowPressureVals,"dateYr"];
lowOutliersSpeed = weatherData[lowPressureVals,"windSusSpeed"];

#Or can write (works better with tibbles)
# weatherData$dateYr[lowPressureVals]


# Label the outliers for the lowest pressure level with the dats on the 
# plot

WvP2 = ggplot( data = weatherData ) +
  stat_boxplot(mapping = aes(x=pressureLevelFact, y = windSusSpeed),
               na.rm = TRUE,
               geom = "errorbar",
               width = 0.2 ) +
  geom_boxplot(mapping = aes(x=pressureLevelFact, y = windSusSpeed) ) +
  annotate(geom = "text",
           x= 1.6,
           y = 47,
           label= "Feb 19, 2016" ) +
  annotate(geom = "text",
           x = 1.6,
           y = 41,
           label = "Mar 03, 2016") +
  annotate(geom = "text",
           x = 1.6,
           y = 48,
           label = "Jul 08, 2016") +
  theme_bw() +
  labs(
    title = "Change in WindSusSpeed vs. Pressure Level",
    subtitle = "Lansing, Michigan: 2016",
    x= "Pressure Level",
    y= "Wind Sus Speed"
  );

plot(WvP2);