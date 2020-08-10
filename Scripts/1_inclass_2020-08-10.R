{
  source(file="scripts/reference.R");  # this line will be in all your scripts
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE);
  
  library(package=gganimate);  # package used to create the animation mappings
  library(package=gifski);
  library(package=av);
  

  workplot1 = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=avgTemp, y=stnPressure)) +
    labs(title = 'Pressure (y) vs. Temperature (x)',
         subtitle = 'Lansing, MI - 2016',
         x = 'Average Temp', 
         y = 'stnPressure',
         caption = '{closest_state} Wind (animation)') +
    theme_bw() +
    transition_states(states = windSpeedLevel, 
                      transition_length = 1, # relative animation time (default: 1)  
                      state_length = 3,      # relative pause time (default: 1)
                      wrap = TRUE);          # gif always wraps so this is useless
  print(workplot1);
  
  workplot2 = ggplot(data=weatherData) +
              geom_histogram(mapping = aes(x=avgTemp, y=..count..)) +
              labs(title = "Average Temperature Histogram by {closest_state} (animation)",
                   subtitle = "Lansing, MI - 2016",
                   x = "Average Temperature",
                   y = "count") +
              theme_bw() +
    #Shadow mark leaves the past years values as a new color
    #in the graph.
              shadow_mark(fill = "blue") +
              transition_states(states = season,
                                transition_length = 1,
                                state_length = 2,
                                wrap = TRUE);
  print(workplot2);
  
  # create a numeric precipitation column
  precipNum = weatherData$precip;
  precipNum[which(precipNum == "T")] = 0.005;
  weatherData$precipNum = as.numeric(precipNum);
  
  workplot3 = ggplot(data=weatherData) +
    geom_col(mapping = aes(x=precipNum, y=avgTemp, fill=windDir)) +
    labs(title = "Precip(x) vs avgTemp(y) by {closest_state} (animation)",
         subtitle = "Lansing, MI - 2016",
         x = "Precipitation",
         y = "Average Temperature") +
    theme_bw() +
    transition_states(states = season,
                      transition_length = 1,
                      state_length = 2,
                      wrap = TRUE);
  print(workplot3);
  
  
  #Assignment 2
  anim_save(filename = "media/workplot2.gif",
            animation = workplot2);
  
  anim_save(filename = "media/workplot2v.mp4",
            animation = workplot2,
            renderer = av_renderer(),
            nframes = 60,       # number of frames in animation
            fps = 3);           # frames per second
  
}