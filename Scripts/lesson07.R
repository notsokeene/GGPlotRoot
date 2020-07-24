source( file="scripts/reference.R");
weatherData = read.csv( file = "data/LansingNOAA2016-3.csv",
                        stringsAsFactors = FALSE );

#### Part 1: A different way to arrange x-axis values
windPlot1 = ggplot( data=weatherData ) +
          geom_boxplot(mapping = aes(x=windDir, y=changeMaxTemp),
                       na.rm = TRUE) +
          scale_x_discrete(limits=c("North", "East", "South", "West")) +
          theme_bw() +
          labs(title = "Change in Temperature vs. Wind Direction",
               subtitle = "Lansing, Michigan: 2016",
               x = "Wind Direction",
               y = "Degrees (Fahrenheit)");
plot(windPlot1);

#### Part 2: Group boxplots by wind speed levels
windPlot2 = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=windSpeedLevel),
               na.rm=TRUE) +
  scale_x_discrete(limits=c("North", "East", "South", "West")) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(windPlot2);

### Part 3: Re-order group as factors
windPlot3 = ggplot(data=weatherData) +
            geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                         fill=factor(windSpeedLevel,
                                     levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
            theme_bw() +
            scale_x_discrete(limits = c("North", "East", "South", "West")) +
            labs(title = "Change in Temperature vs. Wind Direction",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Wind Direction",
                  y = "Degrees (Fahrenheit)");
plot(windPlot3);


#### Part 4: Changing the legend title
windPlot4 = ggplot(data=weatherData) +
          geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
          theme_bw() +
          scale_x_discrete(limits = c("North", "East", "South", "West")) +
          labs(title = "Change in Temperature vs. Wind Direction",
                subtitle = "Lansing, Michigan: 2016",
                x = "Wind Direction",
                y = "Degrees (Fahrenheit)",
                fill = "Wind Speeds");
plot(windPlot4);

### Part 5: Adding color using rgb()
windPlot5 = ggplot(data=weatherData) +
            geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
            theme_bw() +
            scale_x_discrete(limits = c("North", "East", "South", "West")) +
            scale_fill_manual(values = c(rgb(red=1, green=1, blue=0), # low
                               rgb(red=1, green=0.2, blue=0), # medium
                               rgb(red=0.5, green=0, blue=0.8))) + # high
            labs(title = "Change in Temperature vs. Wind Direction",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Wind Direction",
                 y = "Degrees (Fahrenheit)",
                 fill = "Wind Speeds"); # changes the legend (fill) title
plot(windPlot5);

### Part 6: Using facets along the y-axis
windPlot6 = ggplot(data=weatherData) +
          geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
          theme_bw() +
          scale_x_discrete(limits = c("North", "East", "South", "West")) +
          facet_grid(facets=windSpeedLevel ~ .) + # facet in vertical direction
          labs(title = "Change in Temperature vs. Wind Direction",
               subtitle = "Lansing, Michigan: 2016",
               x = "Wind Direction",
               y = "Degrees (Fahrenheit)");
plot(windPlot6);


### Part 7: Using facets along the x-axis
windPlot7 = ggplot(data=weatherData) +
            geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
            theme_bw() +
            scale_x_discrete(limits = c("North", "East", "South", "West")) +
            facet_grid(facets=. ~ windSpeedLevel) + # facet in horizontal direction
            labs(title = "Change in Temperature vs. Wind Direction",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Wind Direction",
                 y = "Degrees (Fahrenheit)");
plot(windPlot7);

### Part 8: Ordering facets
windPlot8 = ggplot(data=weatherData) +
            geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), na.rm=TRUE) +
            theme_bw() +
            scale_x_discrete(limits = c("North", "East", "South", "West")) +
            facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High"))) +
            labs(title = "Change in Temperature vs. Wind Direction",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Wind Direction",
                 y = "Degrees (Fahrenheit)");
plot(windPlot8);

### Part 9: Filling and coloring facets
windPlot9 = ggplot(data=weatherData) +
            geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
                          na.rm=TRUE,
                          color="blue", # outline color
                          fill="red") + # fill color
            theme_bw() +
            scale_x_discrete(limits = c("North", "East", "South", "West")) +
            facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High"))) +
            labs(title = "Change in Temperature vs. Wind Direction",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Wind Direction",
                 y = "Degrees (Fahrenheit)");
plot(windPlot9);

### Part 10: Filling and coloring facets
windPlot10 = ggplot(data=weatherData) +
             geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
                          na.rm=TRUE,
               color=c("blue", rep("black", 3), 
                       "green", rep("black", 3), 
                       "orange", rep("black", 3)),
               fill=c(rep(NA, 8), rep("red", 3), NA)) +
             theme_bw() +
             scale_x_discrete(limits = c("North", "East", "South", "West")) +
             facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High"))) +
             labs(title = "Change in Temperature vs. Wind Direction",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Wind Direction",
                  y = "Degrees (Fahrenheit)");
plot(windPlot10);

### Part 11: Changing facet labels
windLabels = c(Low = "Light Winds",
               Medium = "Medium Winds",
               High = "Strong Winds");

windPlot11 = ggplot(data=weatherData) +
             geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               color=c("blue", rep("black", 3),
                       "green", rep("black", 3),
                       "orange", rep("black", 3)),
               fill=c(rep(NA, 8), rep("red", 3), NA)) +
             theme_bw() +
             scale_x_discrete(limits = c("North", "East", "South", "West")) +
             facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High")),
                        labeller=as_labeller(windLabels)) +
             labs(title = "Change in Temperature vs. Wind Direction",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Wind Direction",
                  y = "Degrees (Fahrenheit)");
plot(windPlot11);