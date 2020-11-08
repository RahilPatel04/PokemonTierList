library(MASS)
library(devtools)
library(ggplot2)
pokemons <- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/PokemonStats.csv")

#Pre process the data

# Create Tiers
maxStat <- max(pokemons$base_total)
minStat <- min(pokemons$base_total)
df = pokemons

FindTiers <- function(numTiers)
{
  section =  (maxStat-minStat)/numTiers;
  df$tier <- 0
  for (row in 1:nrow(df)) 
  {
    for(tiered in 1:numTiers)
    {
      if(df[row,'base_total'] >= (minStat+(section *(tiered-1))) && df[row,'base_total'] < (minStat+(section*tiered)))
      {
        df[row,'tier'] <- numTiers - (tiered - 1);
      }
      if(tiered == numTiers){
        if(df[row, 'base_total'] > (minStat+(section *(tiered-1)))){
          df[row,'tier'] <- numTiers - (tiered - 1);
        }
      }
    }
  }
  return(df)
  
}
df = FindTiers(3)

# Take out the name column
pokeData <- df[,2:ncol(df)]


# run lda on the tier column
ldaData <- lda(tier~., pokeData)
ldaData
ldaData$scaling
ldaData$prior

# predict the ldaData on the pokeData that we already have. I think we can put the test data in pokeData place
predictionData <- predict(ldaData, pokeData)
# I think this data is that each point under lda 1 or 2 is calculated by the coefficents of lda 1 and the columns
predictionData$class
 
# See how accurate our prediction was and maybe tune it?
# Table gives you: column is what the predictor said it was and row is what its suppose to be
predictTable <- table(predictionData$class, pokeData[,ncol(pokeData)])
predictTable

# Run a plot to show the distribution of them
str(pokeData$tier)
ggord(ldaData, factor(pokeData$tier))

