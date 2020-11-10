library(shiny)
library(shinythemes)
library(MASS)
library(devtools)
library(ggplot2)
library(ggord)

# Define UI for application
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage("Navigation Bar",
   tabPanel("Welcome",
            
   ),             
   tabPanel("Pokemon Tier List",
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
            h3("Your Pokemon is in Tier:"),
            textOutput("ClassifierOutput"),
        )
    )
   ),
   tabPanel("Closest Pokemon",
   
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
    
    output$ldaPlotOutput <- renderPlot({
        # Get our Pokemon Training Data
        pokemons <- read.csv("https://raw.githubusercontent.com/RahilPatel04/PokemonTierList/main/TrainingPokeData.csv")
       
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
        baseTotal = input$hpID + input$attackID + input$SPAttackID + input$defenseID + input$SPDefenseID + input$speedID
        
        classifyPokemon <- data.frame(hp = input$hpID, attack = input$attackID, sp_attack = input$SPAttackID, defense = input$defenseID, sp_defense = input$SPDefenseID, speed = input$speedID, base_total = baseTotal)
        print(classifyPokemon)
        
        output$ClassifierOutput <- renderText({
           # predict the ldaData on the pokemon that the user gives us
           predictionData <- predict(ldaData, classifyPokemon)
           # Output the class that the pokemon belongs to
           predictionData$class[1]
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
