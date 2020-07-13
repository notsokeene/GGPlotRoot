source(file="scripts/reference.R");

weatherData = read.csv(file = "data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE);

#### Part 1: Add year to date values ####
#a) save the date vector from the data fram to the variable theDate

theDate=weatherData$date;     # get all values from the dates column
#theDate = weatherData[["date"]]; # equivalent to previous line
#the Date = weatherData[ , "date"]; #euivalent to previous 2 lines in base R

# append (paster) "-2016" to all values in theDate
theDate = paste(theDate, "-2016", sep = "");
# theDate = paste(theDate, "2016", sep="-"); # functionally equivalent to previous line

# c) Save the values in Date format
theDate = as.Date(theDate, format="%m-%d-%Y")

# Save theDate back to the data frame as a new column

weatherData$dateYr = theDate;

# weatherData[["dateYr"]] = theDate; # equivalent
# weatherData[, "dateYr"] = theDate; # equivalent

#### PArt 2: Make two line plots of temperature vs date ##
TempLinePlot = ggplot(data = weatherData) +
                geom_line(mapping = aes(x=dateYr, y = maxTemp)) +
                geom_line(mapping = aes(x=dateYr, y=minTemp));

plot(TempLinePlot);

#### Part 3: Add labels and colors ###
TempLinePlotColors = ggplot(data = weatherData) +
                      geom_line(mapping = aes(x=dateYr, y=maxTemp),
                                color = "palevioletred1") +
                      geom_line(mapping = aes(x=dateYr, y=minTemp),
                                color = "aquamarine2") +
                      labs(title = "Temperature vs. Date",
                           subtitle = "Lansing, Michigan: 2016",
                           x = "Date",
                           y = "Temperature (F)");
plot(TempLinePlotColors);

#### PArt 4: Add average temperature and smooth it out ###

TempLinePlotSmooth = ggplot(data = weatherData) +
                     geom_line(mapping = aes(x=dateYr, y=maxTemp),
                      color = "palevioletred1") +
                     geom_line(mapping = aes(x=dateYr, y=minTemp),
                      color = "aquamarine2") +
                    geom_smooth(mapping = aes(x=dateYr, y=avgTemp),
                                color = "orange",
                                method="loess",
                                linetype=4,
                                fill="lightblue") +
                    labs(title = "Temperature vs. Date",
                         subtitle = "Lansing, Michigan: 2016",
                         x = "Date",
                         y = "Temperature (F)");
plot(TempLinePlotSmooth);

#### Part 5: Paneling changes ###
# grey0 is black, grey100 is white, and numbers in between are shades of grey

TempLinePanelc = ggplot(data = weatherData) +
                  geom_line(mapping = aes(x=dateYr, y=maxTemp),
                            color = "palevioletred1") +
                  geom_line(mapping = aes(x=dateYr, y=minTemp),
                            color = "aquamarine2") +
                  geom_smooth(mapping = aes(x=dateYr, y=avgTemp),
                              color = "orange",
                              method="loess",
                              linetype=4,
                              fill="lightblue") +
                  labs(title = "Temperature vs. Date",
                       subtitle = "Lansing, Michigan: 2016",
                       x = "Date",
                       y = "Temperature (F)") +
                  # size and color relate to the border, fill is the inside color
                  theme(panel.background = element_rect(fill="grey25",
                                                        size = 2, color = "grey0"),
                        panel.grid.minor = element_line(color = "grey50", linetype = 4),
                        panel.grid.major = element_line(color = "grey100"));
                            

plot(TempLinePanelc);

#### Part 6: Making changes outside the panel ###
TempLinePanelc2 = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  # size and color relate to the border, fill is the inside color 
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        axis.text = element_text(color="blue", family="mono", size=9));
plot(TempLinePanelc2);

#### Part 7: Scaling continuous values ###
TempLinePanelc3 = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        axis.text = element_text(color="blue", family="mono", size=9))+
  scale_y_continuous(limits = c(-15,90),
                     breaks = seq(from=-15, to=90, by=20)); 

plot(TempLinePanelc3);

#### Part 8: Scaling discrete values (dates) ###
TempLinePanelc4 = ggplot(data=weatherData) + 
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  # size and color relate to the border, fill is the inside color 
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        axis.text = element_text(color="blue", family="mono", size=9))+
  scale_y_continuous(limits = c(-15,90),
                     breaks = seq(from=-15, to=90, by=20)) +
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-12-21")),
               date_breaks = "6 weeks", 
               date_labels = format("%m/%d"));
plot(TempLinePanelc4);


