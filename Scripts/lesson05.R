source(file = "scripts/reference.R");

weatherData = read.csv(file = "data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE);

#### PArt 1: Add year to date vector and save back to the data frame
theDate = weatherData$date; #save data column to vector
theDate = paste(theDate, "-2016", sep = ""); #append -2016 to vector
theDate = as.Date(theDate, format = "%m-%d-%Y"); # format vector as Date
weatherData$dateYr = theDate; # save vector to Data Frame

#### PArt 2: Set up the season and date variables
# create a season vector that ahs the same length as theDate vector
season = vector(mode = "character", length = length(theDate));

#create date variables for the beginning of each season
springStart = as.Date("03-21-2016", format = "%m-%d-%Y");
summerStart = as.Date("06-21-2016", format = "%m-%d-%Y");
fallStart = as.Date("09-21-2016", format = "%m-%d-%Y");
winterStart = as.Date("12-21-2016", format = "%m-%d-%Y");

#### Part 3: Create a season vector based on theDates vector
for (i in 1:length(theDate))  #go through each date
  {
  # if the date falls with the spring season
  if(theDate[i] >= springStart && theDate[i] < summerStart)
  {
    season[i] = "Spring";
  }
  #if the date falls with the summer season
  else if(theDate[i] >= summerStart && theDate[i] < fallStart)
  {
    season[i] = "Summer";
  }
  # if the date falls with the fall season
  else if(theDate[i] >= fallStart && theDate[i] < winterStart)
  {
    season[i] = "Fall";
  }
  # if the date falls with the winter season --
  # using || because dates are not continuous
  else if(theDate[i] >= winterStart || theDate[i] < springStart)
  {
    season[i] = "Winter";
  }
  else #something went wrong...
  {
    season[i] = "Error";
  }
}

#PArt 4: create a new column in weatherData called season and set it to
# the season vector

weatherData$season = season;

#### PArt 5: Create a histogram of temperatures for the year

TempHist = ggplot( data = weatherData ) +
            geom_histogram( mapping = aes(x=avgTemp, y=..count..) );
plot(TempHist);

#### Part 6: PArameter changes to histogram
TempHist2 = ggplot( data = weatherData ) +
            geom_histogram( mapping = aes(x=avgTemp, y=..count..),
                            bins = 40,
                            color = "grey20",
                            fill = "darkblue");
plot(TempHist2);

####Part 7: Change theme, add titles and labels

TempHist3 = ggplot( data = weatherData ) +
            geom_histogram( mapping = aes(x=avgTemp, y=..count..),
                  bins = 40,
                  color = "grey20",
                  fill = "darkblue") +
            theme_classic() +
            labs(title = "Temperature Histogram",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Average Temp (Fahrenheit)",
                 y = "Count");
plot(TempHist3);

####Part 8: Using binwidths and density
TempHist4 = ggplot( data = weatherData ) +
            geom_histogram( mapping = aes(x=avgTemp, y=..density..),
                  binwidth = 4,
                  color = "grey20",
                  fill = "darkblue") +
            theme_classic() +
            labs(title = "Temperature Histogram",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Average Temp (Fahrenheit)",
                  y = "Density");
plot(TempHist4);

#### PArt 9: Add vertical lines representing mean and median

TempHist5 = ggplot( data = weatherData ) +
            geom_histogram( mapping = aes(x=avgTemp, y=..density..),
                  binwidth = 4,
                  color = "grey20",
                  fill = "darkblue") +
            geom_vline(mapping = aes(xintercept=mean(avgTemp)),
                       color = "red",
                       size=1.2) +
            geom_vline(mapping=aes(xintercept=median(avgTemp)),
                       color="green",
                       size=1.2) +
            theme_classic() +
            labs(title = "Temperature Histogram",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Average Temp (Fahrenheit)",
                  y = "Density");
plot(TempHist5);

#PArt 10: Create a histogram for each season (faceting)
TempHist6 = ggplot(data=weatherData) +
  geom_histogram(mapping=aes(x=avgTemp, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darkblue") +
  theme_classic() +
  facet_grid( facet= season ~ .) +
  labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Count");
plot(TempHist6);

#### Part 11: Create a stacked histogram for each season
TempHist7 = ggplot(data=weatherData) + 
            geom_histogram(mapping=aes(x=avgTemp, y=..count.., fill=season),
                 bins=40,
                 color="grey20",
                 position="stack") +
            theme_classic() +
            labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Count");
plot(TempHist7);

#### End of Code: Save the modified data frame to a new CSV file
write.csv(weatherData, file = "data/LansingNOAA2016-2.csv")