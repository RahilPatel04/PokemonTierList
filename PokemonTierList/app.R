library(shiny)
library(shinythemes)
library(MASS)
library(devtools)
library(ggplot2)
library(ggord)
library(pracma)

# Define UI for application
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage("Navigation Bar",
   tabPanel("Welcome",
    h1("Pokemon Tier List"),
    img(src = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/Squirtle_Squad.png", height = 478, width = 640)      
   ),             
   tabPanel("Classify Pokemon",
    # Application title
    titlePanel("Pokemon Tier List"),
    
    # Sidebar Layout 
    sidebarLayout(
        sidebarPanel(
           h3("Pre Process Pokemon Data"),
           sliderInput("tiersID", "How Many Tiers?", min = 3, max = 10, value = 3),
           h3("Classify Your Pokemon Below:"),
           numericInput("hpID","Pokemon HP Stat", value = 10),
           numericInput("attackID","Pokemon Attack Stat", value = 50),
           numericInput("SPAttackID", "Pokemon SP-Attack Stat", value = 15),
           numericInput("defenseID","Pokemon Defense Stat", value = 20),
           numericInput("SPDefenseID", "Pokemon SP-Defense Stat", value = 7),
           numericInput("speedID", "Pokemon Speed Stat", value = 100),
           actionButton("Submit", "Classify Pokemon")
        ),

        # Output 
        mainPanel(
            plotOutput("ldaPlotOutput"),
            h3("Pokemon Classified as"),
            uiOutput("ClassifierOutput"),
        )
    )
   ),
   tabPanel("Closest Pokemon",
    sidebarLayout(
      sidebarPanel(
        h3("What Pokemons are Similar to Yours?"),
        numericInput("KClosestID","Please Input the K Closests Pokemons you Want?", value = 5)
      ),
      mainPanel(
        plotOutput("ClosestPokeID"),
        tableOutput("ClosestPokeIDData")
      )
    )        
   )
  )  
)

# Define server logic
server <- function(input, output) {

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
    
    # Knn function - takes in the pokemon stats and returns a list of K closest pokemons names
    
    knnknn <- function(hps, atak, spatak, defse, spdefse, spe, bT, theK, pokeData)
    {
      datab <- pokeData
      tAxis <- datab$hp
      uAxis <- datab$attack
      vAxis <- datab$sp_attack
      wAxis <- datab$defense
      xAxis <- datab$sp_defense
      yAxis <- datab$speed
      zAxis <- datab$base_total
      
      
      
      #input values you want to test
      t2 <- hps
      u2 <- atak
      v2 <- spatak
      w2 <- defse
      x2 <- spdefse
      y2 <- spe
      z2 <- bT
      
      best_array <- xAxis
      i <- 1
      while(i <= NROW(xAxis))
      {
        #distance array
        best_array[i] <- sqrt(((x2-xAxis[i])*(x2-xAxis[i]))+((y2-yAxis[i])*(y2-yAxis[i])) + ((t2-tAxis[i])*(t2-tAxis[i]))+((u2-uAxis[i])*(u2-uAxis[i])) + ((v2-vAxis[i])*(v2-vAxis[i]))+((w2-wAxis[i])*(w2-wAxis[i])) + ((z2-zAxis[i])*(z2-zAxis[i])))
        #((x2-xAxis[i])*(x2-xAxis[i]))+((y2-yAxis[i])*(y2-yAxis[i]))
        #((t2-tAxis[i])*(t2-tAxis[i]))+((u2-uAxis[i])*(u2-uAxis[i]))
        #((v2-vAxis[i])*(v2-vAxis[i]))+((w2-wAxis[i])*(w2-wAxis[i]))
        #((z2-zAxis[i])*(z2-zAxis[i]))
        
        i <- i + 1
      }
      
      
      namer <- datab$name
      name <- namer[0]
      
      #the k value (number of pokemons to display)
      k <- theK
      names = character(k)
      answers = integer(k)
      ix <- 1
      while(ix <= k)
      {
        answers[ix] <- 1000
        ix <- ix + 1
      }
      
      ans <- 10000
      u <- 1
      
      while(u <= NROW(best_array))
      {
        if(best_array[u] < ans)
        {
          ans <- best_array[u]
          name <- namer[u]
        }
        if(best_array[u] <= answers[k])
        {
          name_count <- 1
          while(name_count <= k)
          {
            if(best_array[u] < answers[name_count])
            {
              push <- answers[name_count]
              push_name <- names[name_count]
              ii <- name_count + 1
              while(ii <= k)
              {
                push2 <- answers[ii]
                push_name2 <- names[ii]
                answers[ii] <- push
                names[ii] <- push_name
                push <- push2
                push_name <- push_name2
                ii <- ii + 1
              }
              
              answers[name_count] <- best_array[u]
              names[name_count] = as.character(namer[u])
              break
            }
            name_count <- name_count + 1
          }
        }
        
        u <- u + 1
      }
      
      #display all stats in list
      dataC <- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/TrainingPokeData.csv")
      answer_set <- 0
      as1 <- 0
      co <- 1
      while(co <= NROW(names))
      {
        c02 <- 1
        while(c02 < NROW(dataC$name))
        {
          dataC$name[c02]
          names[1]
          if(strcmp(names[co],dataC$name[c02]))
          {
            if(as1 == 1)
            {
              answer_set <- rbind(answer_set,dataC[c02,])
            }
            if(as1 == 0)
            {
              answer_set <- dataC[c02,]
              as1 <- 1
            }
          }
          c02 <- c02 + 1
        }
        co <- co + 1
      }
      answer_set
      distance <- answers
      answer_set <- cbind(answer_set,distance)
      return(answer_set)
    }
    
    output$ldaPlotOutput <- renderPlot({
        # Get our Pokemon Training Data
        pokemons <<- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/TrainingPokeData.csv")
       
         #Pre Process our Data
        
        # Update the pokemon data with the tiers
        tiers = input$tiersID
        pokemons = FindTiers(tiers, pokemons)
        # Take out the name column
        pokeData <- pokemons[,2:ncol(pokemons)]
        # run lda on the tier column. Make ldaData global
        ldaData <<- lda(tier~., pokeData)
            
        ggord(ldaData, factor(pokeData$tier))
        
    })
    
    observeEvent(input$Submit, {
        # Calculate Base Total
        hp = input$hpID
        atk = input$attackID
        spAtk = input$SPAttackID
        def = input$defenseID
        spDef = input$SPDefenseID
        speed = input$speedID
        
        baseTotal = input$hpID + input$attackID + input$SPAttackID + input$defenseID + input$SPDefenseID + input$speedID
        
        classifyPokemon <- data.frame(hp = input$hpID, attack = input$attackID, sp_attack = input$SPAttackID, defense = input$defenseID, sp_defense = input$SPDefenseID, speed = input$speedID, base_total = baseTotal)
        
        output$ClassifierOutput <- renderUI({
           # predict the ldaData on the pokemon that the user gives us
           predictionData <- predict(ldaData, classifyPokemon)
           # Output the class that the pokemon belongs to
           classVal = predictionData$class[1]
           
           result = switch(classVal,
                           "1" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier1.png",
                           "2" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier2.png",
                           "3" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier3.png",
                           "4" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier4.png",
                           "5" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier5.png",
                           "6" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier6.png",
                           "7" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier7.png",
                           "8" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier8.png",
                           "9" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier9.png",
                           "10" = "https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/FinishedTier10.png"
           )
           
           img(src = result, height = 300, width = 600)
           
        })
        
        output$ClosestPokeID <- renderPlot({
          closestPokemonsKNN <- knnknn(hp, atk, spAtk, def, spDef, speed, baseTotal, input$KClosestID, pokemons)
          output$ClosestPokeIDData <- renderTable({
            closestPokemonsKNN
          })
          ggplot(closestPokemonsKNN, aes(x = reorder(name, distance), y = distance, fill=name)) + geom_bar(stat = "identity")
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
