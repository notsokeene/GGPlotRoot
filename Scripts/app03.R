source(file="scripts/reference.R"); #include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE);
#Plot data####
plotData = ggplot(data = weatherData ) +
  geom_point( mapping=aes(x=windSpeed, y=abs(tempDept)),
              color= rgb(red=0, green=0.6, blue = 0.6),
              size=2.0,
              shape=18,
              alpha = 0.7) +
  theme_bw() +
  labs(title = "Wind Speed vs. Temperature Departure",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Wind Speed (MPH)",
       y = "Daily Temperature Departure (F)" ) +
  theme(axis.title.x = element_text(size=14, color = "darkblue", face = "italic"),
        axis.title.y = element_text(size=14, color = "Darkblue"),
        axis.text.y=element_text(angle=90),
        plot.title = element_text(size = 18, face = "bold", 
                                  color = "lightblue"),
        plot.subtitle = element_text(size = 12, face = "italic",
                                     color = "turquoise", family = "serif")) +
  geom_smooth( mapping = aes(x=windSpeed, y=abs(tempDept)),
               method = "lm",
               color = "darkgreen",
               size=0.8,
               linetype=2,
               fill="lightgreen");
plot(plotData);

#This plot shows that Average Wind Speed is positively related to 
# absolute Daily Temperature Departure in Lansing, Michigan. 
# That is, If Wind Speed increases, a person can say that it is
# likely that The temperature will also change, and vice versa. 


ggsave(filename = "images/WndTemp.jpeg",
       plot = plotData,
       scale = 1,
       width = 12,
       height = 7,
       units = "cm",
       dpi = 300)

ggsave(filename = "images/pngFiles/WndTemp.png",
       plot = plotData,
       scale = 1,
       width = 6,
       height = 8,
       units = "in",
       dpi = 300)