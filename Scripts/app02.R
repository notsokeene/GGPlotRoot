# execute the lines of code from reference.r
source(file="scripts/reference.r");

# Import deaths data
accdeaths = read.csv(file="data/accdeaths.csv")

#Create Plot
plotDeath = ggplot( data=accdeaths ) +
  geom_point( mapping=aes(x=time, y=value) ) +
  ggtitle( label="Accidental Deaths in the US (1973 - 1978)" ) +
  scale_y_continuous( breaks = seq(from=7000, to=11000, by=2000) ) +
  scale_x_continuous( breaks = seq(from=1973, to=1979, by=0.5) ) +
  theme( axis.text.x=element_text(angle=45, hjust=1 ) );

plot(plotDeath);