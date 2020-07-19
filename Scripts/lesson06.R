source(file="scripts/reference.R");
weatherData = read.csv( file = "data/LansingNOAA2016-2.csv",
                        stringsAsFactors = FALSE);

#### Part 1: Create a wind direction column
windSpeedQuant = quantile(weatherData$windSpeed, probs = c(.30, .70));
for(day in 1:nrow(weatherData)) #nrow(weatherData = 366)
{
  #if the value in windSpeed is less than or equal to the low quant value
  if(weatherData$windSpeed[day] <= windSpeedQuant[1]) # <= 6.4
  {
    weatherData$windSpeedLevel[day] = "Low";
  }
  # if the value in windSpeed is greater than or equal to the high quant value
  else if(weatherData$windSpeed[day] >= windSpeedQuant[2]) # >= 10.2
  {
    weatherData$windSpeedLevel[day] = "High";
  }
  else #the value in windspped is betweeen 6.2 and 10.4
  {
    weatherData$windSpeedLevel[day] = "Medium";
  }
}

for(day in 1:nrow(weatherData))
{
  # if the direction is greater than 315 OR less than 45 degrees
  if(weatherData$windSusDir[day] >= 315 || 
     weatherData$windSusDir[day] < 45)
  {
    weatherData$windDir[day] = "North";
  }
  # if the direction is greater than 45 AND less than 135 degrees
  else if(weatherData$windSusDir[day] >= 45 && 
          weatherData$windSusDir[day] < 135)
  {
    weatherData$windDir[day] = "East";
  }
  # if the direction is greater than 135 AND less than 225 degrees
  else if(weatherData$windSusDir[day] >= 135 && 
          weatherData$windSusDir[day] < 225)
  {
    weatherData$windDir[day] = "South";
  }
  else # the direction is between 225 and 315 degrees
  {
    weatherData$windDir[day] = "West";
  }
}

for(day in 1:nrow(weatherData))
{
  if( day == 1)  # no day before the first day
  {
    weatherData$changeMaxTemp[day] = NA; 
  }
  else  # subtract previous day's maxTemp from current day's maxTemp
  {
    weatherData$changeMaxTemp[day] = weatherData$maxTemp[day] -
      weatherData$maxTemp[day-1];
  }
}

#Alternate with only one for statement
for(day in 1:nrow(weatherData))
{
  ## Adding a column that gives relative wind speed for the day
  # Winds less than 6.4 miles/hour -- label as "Low"
  if(weatherData[day,"windSpeed"] <= windSpeedQuant[1])
  {
    weatherData[day,"windSpeedLevel"] = "Low";
  }
  # Winds greater than 10.2 miles/hour -- label as "High"
  else if(weatherData[day,"windSpeed"] >= windSpeedQuant[2])
  {
    weatherData[day,"windSpeedLevel"] = "High";
  }
  else # wind speeds between 6.4 and 10.2 miles/hour -- label as "Medium"
  {
    weatherData[day,"windSpeedLevel"] = "Medium";
  }
  
  ## Adding a column that gives the cardinal wind direction
  # if the direction is greater than 315 OR less than 45 degrees
  if(weatherData[day,"windSusDir"] >= 315 ||
     weatherData[day,"windSusDir"] < 45)
  {
    weatherData[day,"windDir"] = "North";
  }
  # if the direction is greater than 45 AND less than 135 degrees
  else if(weatherData[day,"windSusDir"] >= 45 &&
          weatherData[day,"windSusDir"] < 135)
  {
    weatherData[day,"windDir"] = "East";
  }
  # if the direction is greater than 135 AND less than 225 degrees
  else if(weatherData[day,"windSusDir"] >= 135 &&
          weatherData[day,"windSusDir"] < 225)
  {
    weatherData[day,"windDir"] = "South";
  }
  else # the directions is between 225 and 315 degrees
  {
    weatherData[day,"windDir"] = "West";
  }
  ### Adding a changeMaxTemp column
  if(day == 1)
  {
    weatherData[day,"changeMaxTemp"] = NA;
  }
  else
  {
    weatherData[day,"changeMaxTemp"] = weatherData[day,"maxTemp"] -
      weatherData[day-1,"maxTemp"];
  }
}

#save data
write.csv(weatherData, file="data/LansingNOAA2016-3.csv");

#### Part 2: Plot Wind Speed vs wind direction
MaxTempvWindDir = ggplot(data=weatherData) +
                  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp)) +
                  theme_bw() +
                  labs(title = "Change in Temperature vs. Wind Direction",
                       subtitle = "Lansing, Michigan: 2016",
                       x = "Wind Direction",
                       y = "Degrees (Fahrenheit)");
plot(MaxTempvWindDir);


#### Part 3: Violin plot of Wind Speed vs. wind direction
MaxTempvWindDir2 = ggplot(data=weatherData) +
  geom_violin(mapping=aes(x=windDir, y=changeMaxTemp),
              na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(MaxTempvWindDir2);

#### Part 4: Add error bars
MaxTempvWindDir3 = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), 
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(MaxTempvWindDir3);

### Part 5: Re-order the directions on the x-axis using factor(s)
windDirFact = factor(weatherData$windDir,
                     levels=c("North", "East", "South", "West"));

MaxTempvWindDir4 = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(MaxTempvWindDir4);

### Part 6: Outlier changes
MaxTempvWindDir5 = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               outlier.shape = "@", 
               outlier.color = "red",
               outlier.alpha = 0.6, 
               outlier.size = 4 ) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(MaxTempvWindDir5);

#### Part 7: Add mean and median text
northVals=which(weatherData$windDir == "North");
southVals=which(weatherData$windDir == "South");

northMed = median(weatherData[northVals,"changeMaxTemp"], na.rm=TRUE);
southMed = median(weatherData[southVals,"changeMaxTemp"], na.rm=TRUE);

MaxTempvWindDir6 = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp), 
               na.rm=TRUE,
               outlier.shape = NA) +
  annotate(geom="text",    # North median
           x=1,
           y=20, 
           color="blue",   
           label=paste("median:", northMed) ) +
  annotate(geom="text",    # South median
           x=3, 
           y=-10, 
           color="red",    
           label=paste("median:", southMed) ) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(MaxTempvWindDir6);