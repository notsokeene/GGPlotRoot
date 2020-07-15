source(file = "scripts/reference.R");

#Open the dataframe created in lesson 5
weatherData = read.csv(file = "data/LansingNOAA2016-2.csv",
                       stringsAsFactors = FALSE);

#Do a histogram of avg humidity -- add titles and labels
RelHumHist = ggplot(data = weatherData) +
             geom_histogram(mapping = aes(x=relHum, y=..count..),
                            bins = 30,
                            color = "grey",
                            fill = "blue") +
             theme_classic()+
             labs(title = "Average Relative Humidity Histogram",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Humidity (%)",
                  y = "Count");
plot(RelHumHist);

#create a column in the dataframe called biMonth
theDate = weatherData$dateYr
biMonth = vector(mode = "character", length = length(theDate));

#starts of the bimonthly periods
janStart = as.Date("01-01-2016", format = "%m-%d-%Y");
marStart = as.Date("03-01-2016", format = "%m-%d-%Y");
mayStart = as.Date("05-01-2016", format = "%m-%d-%Y");
julStart = as.Date("07-01-2016", format = "%m-%d-%Y");
sepStart = as.Date("09-01-2016", format = "%m-%d-%Y");
novStart = as.Date("11-01-2016", format = "%m-%d-%Y");

#Populate it
for (i in 1:length(theDate))  #go through each date
{
  # if the date falls within JanFeb
  if(theDate[i] >= janStart && theDate[i] < marStart)
  {
    biMonth[i] = "JanFeb";
  }
  #if the date falls with the marapr season
  else if(theDate[i] >= marStart && theDate[i] < mayStart)
  {
    biMonth[i] = "MarApr";
  }
  # if the date falls with the mayjun season
  else if(theDate[i] >= mayStart && theDate[i] < julStart)
  {
    biMonth[i] = "MayJun";
  }
  # if the date falls with the julaug season
  else if(theDate[i] >= julStart && theDate[i] < sepStart)
  {
    biMonth[i] = "JulAug";
  }
  # if the date falls with the sepout season
  else if(theDate[i] >= sepStart && theDate[i] < novStart)
  {
    biMonth[i] = "SepOct";
  }
  # if the date falls with the novdec season
  else if(theDate[i] >= novStart)
  {
    biMonth[i] = "NovDec";
  }
  else #something went wrong...
  {
    biMonth[i] = "Error";
  }
}
#connect bimonth to dataset

weatherData$biMonth = biMonth;

#use faceting to create seperate histogram for each biMonth
RelHumHistFac = ggplot(data = weatherData) +
  geom_histogram(mapping = aes(x=relHum, y=..count..),
                 bins = 30,
                 color = "grey",
                 fill = "blue") +
  theme_classic()+
  facet_grid( facet= biMonth ~ .) +
  labs(title = "Average Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity (%)",
       y = "Count");
plot(RelHumHistFac);


#create stacked histogram
#RelHumVector
Humidity = weatherData$relHum
#add mean line for JanFeb
JanFebHum = vector(mode = "numeric",length = length(biMonth))
for (i in 1:length(theDate))  #go through each date
{
if(biMonth[i] == "JanFeb")
{
  JanFebHum[i] = Humidity[i];
}
  else 
    JanFebHum[i] = "";
}

JanFebHum2 = as.numeric(JanFebHum);
#Add mean line for JulAug
JulAugHum = vector(mode = "numeric",length = length(biMonth))
for (i in 1:length(theDate))  #go through each date
{
  if(biMonth[i] == "JulAug")
  {
    JulAugHum[i] = Humidity[i];
  }
  else 
    JulAugHum[i] = "";
}

JulAugHum2 = as.numeric(JulAugHum);
#chall 2
RelHumFact = factor(weatherData$biMonth,
                    c("JanFeb",
                      "MarApr",
                      "MayJun",
                      "JulAug",
                      "SepOct",
                      "NovDec"
                    ))
#make histogram
RelHumHistFac2 = ggplot(data=weatherData) + 
  geom_histogram(mapping=aes(x=relHum, y=..count.., fill=RelHumFact),
                 bins=40,
                 color="grey20",
                 position="stack") +
  geom_vline(mapping = aes(xintercept=mean(JanFebHum2, na.rm = TRUE)),
             color = "grey0",
             size=1.2) +
  geom_vline(mapping=aes(xintercept=mean(JulAugHum2, na.rm = TRUE)),
             color="grey50",
             size=1.2) +
  theme_classic() +
  labs(title = "Average Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity (%)",
       y = "Count") +
      theme(
        #Chall 1
        legend.position = c(x=0.15, y=0.75)
      );
plot(RelHumHistFac2);
