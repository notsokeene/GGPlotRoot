{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE);
 
  ### Replace "T" in precip column with 0.005 using gsub()
  weatherData$precip2 = gsub(x=weatherData$precip, 
                             pattern="T", 
                             replacement="0.005");
  
  ### Probably want the column to be numeric...
  weatherData$precip3 = as.numeric(gsub(x=weatherData$precip, 
                             pattern="T", 
                             replacement="0.005"));
  
  ### Plot humidity vs. precip2 (precip2 is string value -- this will cause problems!) 
  thePlot = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=relHum, y=precip2)) +
    theme_bw() +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity",
         y = "Precipitation (string/character/factor/categorical values)");
  plot(thePlot);


  ### Plot humidity vs. precip3 (much better)  
  thePlot = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=relHum, y=precip3)) +
    theme_bw() +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity",
         y = "Precipitation (numeric values)");
  plot(thePlot);
  
  # First 10 days of precipitaion (using precip3)
  precipAmount = 0;   # starts at zero (this is sometimes called a "state" variable)

  ## show with debugger values going up  
  for(i in 1:10)
  {
    precipAmount = precipAmount + weatherData$precip3[i]; 
  }
  
  # February precipitation take 1
  FebPrecip = 0;
  for(i in 32:60)
  {
    FebPrecip = FebPrecip + weatherData$precip3[i]; 
  }
  
  # February precipitation take 2
  FebPrecip2 = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(i >= 32 & i <= 60)      # & vs && (| vs ||)
    {
      FebPrecip2 = FebPrecip2 + weatherData$precip3[i]; 
    }
  }
  
  # February precipitation take 3
  FebPrecip3 = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="02-"))
    {
      FebPrecip3 = FebPrecip3 + weatherData$precip3[i]; 
    }
  }
  
  # 1) Using one for loop:
  #    Find the total rainfall in April, July, and November
  #    - you will need three state variables
  # 2) Using one for loop:
  #    Find the total rainfall for the first three months and the last three months
  #    - you can extend the grep pattern (i.e., pattern="RA|SN|FG"  
  #                                             looks for rain, snow, or fog)
  # 3) Using one for loop:
  #    Find the month with the most amount of rain
 
  # TK Answers 
#1)
  #vector  of three state variables
  AprJulNovPrecip = c(0,
                      0,
                      0);
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="04-")) #April changes 1 row of vector
    {
      AprJulNovPrecip[1] = AprJulNovPrecip[1] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "07-")) #July changes 2 row of vector
    {
      AprJulNovPrecip[2] = AprJulNovPrecip[2] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "11-")) #November changes 3 row of vector
    {
      AprJulNovPrecip[3] = AprJulNovPrecip[3] + weatherData$precip3[i]; 
    }
  }
  
  #2)
  firstLastThreeMonths = c(0,
                           0);
  
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="01-|02-|03-"))
    {
      firstLastThreeMonths[1] = firstLastThreeMonths[1] + weatherData$precip3[i]; 
    }
    if(grepl(x=weatherData$date[i], pattern = "10-|11-|12-"))
    {
      firstLastThreeMonths[2] = firstLastThreeMonths[2] + weatherData$precip3[i]; 
    }
  }
  
  #3
  monthlyPrecipTotals = c(0, #Jan
                           0, #Feb
                           0, #Mar
                           0, #Apr
                           0, #May
                           0, #Jun
                           0, #Jul
                           0, #Aug
                           0, #Sep
                           0, #Oct
                           0, #Nov
                           0) #Dec
  
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="01-")) 
    {
       monthlyPrecipTotals[1] =  monthlyPrecipTotals[1] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "02-")) 
    {
       monthlyPrecipTotals[2] =  monthlyPrecipTotals[2] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "03-")) 
    {
       monthlyPrecipTotals[3] =  monthlyPrecipTotals[3] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="04-")) 
    {
      monthlyPrecipTotals[4] =  monthlyPrecipTotals[4] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "05-")) 
    {
      monthlyPrecipTotals[5] =  monthlyPrecipTotals[5] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "06-"))
    {
      monthlyPrecipTotals[6] =  monthlyPrecipTotals[6] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="07-"))
    {
      monthlyPrecipTotals[7] =  monthlyPrecipTotals[7] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "08-")) 
    {
      monthlyPrecipTotals[8] =  monthlyPrecipTotals[8] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "09-")) 
    {
      monthlyPrecipTotals[9] =  monthlyPrecipTotals[9] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="10-")) 
    {
      monthlyPrecipTotals[10] =  monthlyPrecipTotals[10] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "11-")) 
    {
      monthlyPrecipTotals[11] =  monthlyPrecipTotals[11] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern = "12-")) 
    {
      monthlyPrecipTotals[12] =  monthlyPrecipTotals[12] + weatherData$precip3[i]; 
    }
    
  }
 
  maxMonthPrecip = max(monthlyPrecipTotals);
  compareMonthPrecip = as.data.frame(monthlyPrecipTotals, month.abb);
  #August has highest precipitation.
  
  
#easier  - Haven't completed.

  Month = c("01-","02-","03-","04-","05-","06-","07-","08-","09-","10-", "11-", "12-");
  monthlyPrecipTotals2 = c(rep(0,12)); 
  
  for (i in 1:length(weatherData$precip3)) 
  {
    for (j in 1:length(month)) 
    {
      if (grepl(x=weatherData$date[i], pattern = Month[j]))
      {
      monthlyPrecipTotals2[11] =  monthlyPrecipTotals2[11] + weatherData$precip3[i]; 
      }
    }
  }
   

  
}  