library(MASS)
library(devtools)
library(ggplot2)
library(ggord)
pokemons <- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/TrainingPokeData.csv")
testPokeData <- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/TestPokeData.csv")

CompareAccuracy <- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/PokemonStats.csv")

#Pre process the data

# Create Tiers

# FindTiers takes in number of tiers and the data it wants to put the tiers in
FindTiers <- function(numTiers, pokemonDataInput)
{
  maxStat <- max(pokemonDataInput$base_total)
  minStat <- min(pokemonDataInput$base_total)
  df = pokemonDataInput
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
# Update the pokemon data with the tiers
tiers = 3
pokemons = FindTiers(tiers, pokemons)
CompareAccuracy = FindTiers(tiers, CompareAccuracy)


# Take out the name column
pokeData <- pokemons[,2:ncol(pokemons)]
CompareAccuracyData <- CompareAccuracy[,2:ncol(CompareAccuracy)]

# run lda on the tier column
ldaData <- lda(tier~., pokeData)
ldaData
ldaData$scaling
ldaData$prior

# predict the ldaData on the pokeData that we already have. I think we can put the test data in pokeData place
predictionData <- predict(ldaData, testPokeData)
# I think this data is that each point under lda 1 or 2 is calculated by the coefficents of lda 1 and the columns
predictionData$class[1]
 
# See how accurate our prediction was and maybe tune it?
# Table gives you: column is what the predictor said it was and row is what its suppose to be
predictTable <- table(predictionData$class, CompareAccuracyData[1:49,ncol(CompareAccuracyData)])
predictTable

# Run a plot to show the distribution of them
str(pokeData$tier)
ggord(ldaData, factor(pokeData$tier))

# Create a new plot with the new certain pokemon classified
print(predictionData$class[1])

