source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#Create precipitation column without T's
precip = weatherData$precip;
precip1 = gsub(pattern = "T",
               replacement = 0.005,
               x = precip);
precip2 = as.numeric(precip1);
weatherData$precip2 = precip2;

#reorder the data so it's by temperature
reorderedData = weatherData[order(weatherData$avgTemp),];

#figure out quantile values for temperature
temp_quants = quantile(weatherData$avgTemp, 
                       probs = c(0, .5, 1));


# Create a text plot of Precipitation vs Humidity

thePlot1 = ggplot(data=reorderedData) +
            geom_text(mapping=aes(x=precip2, y=relHum, color=1:nrow(weatherData),
                        label=avgTemp),
                        size=2.5) +
            scale_color_gradientn(colors=c("blue", "darkgreen", "red"),
                                  values=c(0, 0.5, 1),
                                  breaks=c(1L, 87L, 366L),
                                  labels=c("8", "52", "82")) +
            scale_y_continuous(trans="log10") +
            theme_bw() +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.key.height = unit(15, units="pt"),  # height
                  legend.key.width = unit(20, units="pt"),   # width
                  legend.direction = "horizontal",           # alignment
                  legend.position = c(0.75, 0.15)) +         # position
  
            labs(title = "Humidity vs. Precipitation",
                 subtitle = "Lansing, Michigan: 2016",
                 x = "Precipitation",
                 y = "Relative Humidity",
                 color = ""); 
plot(thePlot1);
