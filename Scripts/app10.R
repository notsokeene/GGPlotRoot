# Create a script file in your GGPlot Class Project called app10.r.
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

# Create a histogram of tempDept restricted to days that meet a condition in the weatherType column:
#   To restrict the data, you can subset with the indices that meet the condition (e.g., data=weatherData[daysWithRain,])
freezeRainyDays = grep(weatherData$weatherType, pattern="FZRA");

freezeRainyDaysData = weatherData[freezeRainyDays,];
fzrnMean = mean(freezeRainyDaysData$tempDept);

Plot1 = ggplot(data = weatherData[freezeRainyDays,] ) +
        geom_histogram(mapping = aes(x=tempDept , y=..count..),
                       binwidth = 5,
                       color = "white",
                       fill = "lightblue") +
  # Place a vertical line at the average tempDept for the condition and label the line with the average value
        geom_vline(mapping = aes(xintercept=fzrnMean))+
        annotate(geom = "text",
                 x = -2, 
                 y = 1.5,
                 label = "-2.94 F")+
        theme_classic() +
        labs(title = "Temperature Departure Histogram, Freezing Rain Days",
             subtitle = "Lansing, Michigan, 2016",
             x = "Temperature Departure (F)",
             y = "Count") ;
plot(Plot1);

# Repeat step 2 for two more conditions in the weatherType column (so, three histograms in all)
snowDays = grep(weatherData$weatherType, pattern="SN");  
breezyDays = grep(weatherData$weatherType, pattern="BR");  

snowDaysData = weatherData[snowDays,];
snowMean = mean(snowDaysData$tempDept);
        
breezyDaysData = weatherData[breezyDays,];
breezyMean = mean(breezyDaysData$tempDept);
        
Plot2 = ggplot(data = weatherData[snowDays,] ) +
        geom_histogram(mapping = aes(x=tempDept , y=..count..),
                         binwidth = 5,
                         color = "white",
                         fill = "lightblue") +
        # Place a vertical line at the average tempDept for the condition and label the line with the average value
        geom_vline(mapping = aes(xintercept=snowMean))+
        annotate(geom = "text",
                   x = -2.5, 
                   y = 7.5,
                   label = "-4.11 F")+
        theme_classic() +
        labs(title = "Temperature Departure Histogram, Snow Days",
             subtitle = "Lansing, Michigan, 2016",
             x = "Temperature Departure (F)",
             y = "Count") ;
plot(Plot2);
        
Plot3 = ggplot(data = weatherData[breezyDays,] ) +
        geom_histogram(mapping = aes(x=tempDept , y=..count..),
                         binwidth = 5,
                         color = "white",
                         fill = "lightblue") +
        # Place a vertical line at the average tempDept for the condition and label the line with the average value
        geom_vline(mapping = aes(xintercept=breezyMean))+
        annotate(geom = "text",
                 x = 4.5, 
                 y = 25,
                 label = "2.82 F")+
        theme_classic() +
        labs(title = "Temperature Departure Histogram, Breezy Days",
             subtitle = "Lansing, Michigan, 2016",
             x = "Temperature Departure (F)",
             y = "Count") ;
plot(Plot3);

# Create a histogram of tempDept restricted to days where two conditions occur in the weatherType column (e.g., rainy AND breezy).
SnowyAndBreezy = intersect(snowDays, breezyDays);
snowyAndBreezyData = weatherData[SnowyAndBreezy,];
snowyAndBreezyMean = mean(snowyAndBreezyData$tempDept);

Plot4 = ggplot(data = weatherData[SnowyAndBreezy,] ) +
        geom_histogram(mapping = aes(x=tempDept , y=..count..),
                       binwidth = 5,
                       color = "white",
                       fill = "lightblue") +
  # Place a vertical line at the average tempDept for the condition and label the line with the average value
        geom_vline(mapping = aes(xintercept=snowyAndBreezyMean))+
        annotate(geom = "text",
                 x = -2, 
                 y = 7,
                 label = "-3.70 F")+
        theme_classic() +
        labs(title = "Temperature Departure Histogram, Snow and Breezy Days",
             subtitle = "Lansing, Michigan, 2016",
             x = "Temperature Departure (F)",
             y = "Count") ;
plot(Plot4);

# Create a histogram of tempDept restricted to days where one of two conditions occur in the weatherType column (e.g., rainy OR breezy).
# Place a vertical line at the average tempDept and label the line with the average value
snowyOrBreezy = union(snowDays, breezyDays); 
snowyOrBreezyData = weatherData[snowyOrBreezy,];
snowyOrBreezyMean = mean(snowyOrBreezyData$tempDept);

Plot5 = ggplot(data = weatherData[snowyOrBreezy,] ) +
  geom_histogram(mapping = aes(x=tempDept , y=..count..),
                 binwidth = 5,
                 color = "white",
                 fill = "lightblue") +
  geom_vline(mapping = aes(xintercept=snowyOrBreezyMean))+
  annotate(geom = "text",
           x = 4.5, 
           y = 25,
           label = "2.09 F")+
  theme_classic() +
  labs(title = "Temperature Departure Histogram, Snow or Breezy Days",
       subtitle = "Lansing, Michigan, 2016",
       x = "Temperature Departure (F)",
       y = "Count") ;
plot(Plot5);

# Using grid.arrange(), place the 5 histograms you created in the previous steps on one canvas
grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5,
             ncol=2);

# Using grid.arrange(), pick three histograms from steps 2-4 and place them on a canvas
# Resize at least 2 of the histogram so they take up more than 1 cell
grid.arrange(Plot2, Plot4, Plot5, 
             layout_matrix = rbind(c(2,NA,NA),
                                   c(4,4,NA),
                                   c(5,5,NA)));
