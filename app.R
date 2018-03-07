library(shiny)
library("dplyr")
library("ggplot2")
library("png")
library("grid")
library(lubridate)

Stats <- read.csv("Stats.csv", stringsAsFactors = FALSE) 
erangelData <- read.csv("data_subset.csv")

getPlayerStats <- function(playername, mode) {
  if (mode == "Squad") {
    size <- 4
  } else if (mode == "Duo") {
    size <- 2
  } else {
    size <- 1
  }
  Stats_Filtered <- subset(Stats, player_name == playername & party_size == size)
  Stats_Player <- data.frame(matrix(ncol = 10, nrow = 1))
  Column_names <- c("Games_Played", "Wins", "Top_5", "Top_10", "Most_Kills",
                    "Average_Kills", "Average_Damage", "Average_Assists", "Average_Distance_Travelled", "Average_Time_Survived")
  colnames(Stats_Player) <- Column_names
  Stats_Player$Games_Played <- nrow(Stats_Filtered)
  if(Stats_Player$Games_Played[1] == 0) {
    Stats_Player[is.na(Stats_Player)] <- 0
  } else {
    Stats_Player$Average_Kills <- round(mean(Stats_Filtered$player_kills), digits = 2)
    Stats_Player$Most_Kills <- max(Stats_Filtered$player_kills)
    Stats_Player$Average_Assists <- round(mean(Stats_Filtered$player_assists), digits = 2)
    Stats_Player$Average_Damage <- round(mean(Stats_Filtered$player_dmg), digits = 2)
    
    Stats_Filtered$Distance_Travelled <- mean(Stats_Filtered$player_dist_walk) + mean(Stats_Filtered$player_dist_ride)
    Distance <- round(mean(Stats_Filtered$Distance_Travelled), digits = 2)
    
    Stats_Player$Average_Distance_Travelled <- paste(Distance,'m')
    Stats_Player$Top_10 <- sum(Stats_Filtered$team_placement <= 10)
    Stats_Player$Top_5 <- sum(Stats_Filtered$team_placement <= 5)
    Stats_Player$Wins <- sum(Stats_Filtered$team_placement == 1)
    Stats_Player$Average_Time_Survived <- paste(seconds_to_period(round(mean(Stats_Filtered$player_survive_time), digits = 0)))
    colnames(Stats_Player) <- c("Games Played", "Games Won", "Placed Top 5", "Placed Top 10", "Most Kills",
                                "Average Kills", "Average Damage", "Average Assists", "Average Distance Tavelled", "Average Time Survived")
    return(Stats_Player)
  }
}

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("PUBG Statistics"),
   
   includeCSS("styles.css"),
   
    sidebarLayout(
      sidebarPanel(
         selectInput("player_name",
                    "Find Player Stats. (Select or search players name)", 
                    choices = unique(Stats$player_name)),

        tags$style(".well {background-color:#fff; 
                         border-top: 3px solid #eda338;}"),
        sliderInput("time", "Time In Game (in seconds):",
                    60, 2201, value = c(100, 1300))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
            tabPanel("Stats", tags$style(".tab-content { border-left: 1px solid #ddd; border-right: 1px solid #ddd; background-color: #fff; padding: 15px; }
                       .nav-tabs { margin-bottom: 0;"),
            tabsetPanel(id = "tabs",
                        tabPanel("Solo", value = "Solo", tableOutput("solo")),
                        tabPanel("Duo",value = "Duo", tableOutput("duo")),
                        tabPanel("Squad", value = "Squad", tableOutput("squad"))
            )),
            tabPanel("Weapon Statistics", plotOutput("sidePlot")),
            tabPanel("Death Plot", tags$style(".tab-content { height: auto; }"), plotOutput("distPlot")
        )
      )
   )
 )
)

# Define server 
server <- function(input, output) {
   
  output$distPlot <- renderPlot({
    img <- readPNG("erangel.PNG")
    bg <- rasterGrob(img, interpolate = FALSE, width=unit(1,"npc"), height=unit(1,"npc"))
    
    mintime <- input$time[1]
    maxtime <- input$time[2]
    
    timedata <- erangelData %>% filter(
      time >= mintime,
      time <= maxtime
    )
    
    timedata <- as.data.frame(timedata)
    
    ggplot(data = timedata) +
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
            axis.text.x=element_blank(), axis.text.y=element_blank(),
            axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) +
      annotation_custom(bg, xmin = 0,  xmax = 800000, ymin = -800000, ymax = 0) +
      geom_point(mapping = aes(x = victim_position_x * (800000/812800), y = victim_position_y * (800000/812800)), color = "red", alpha = 0.04) + #0.008
      xlim(0, 800000) + scale_y_reverse(lim=c(800000, 0))
  }, height = 800, width = 800) # size of map
  
   output$solo <- renderTable({
    getPlayerStats(input$player_name, input$tabs)
   })
  
   output$duo <- renderTable({
    getPlayerStats(input$player_name, input$tabs) 
   })
  
   output$squad <- renderTable({
     getPlayerStats(input$player_name, input$tabs)  
   })  
   
   output$sidePlot <- renderPlot({
     ggplot(erangelData) + 
       geom_bar(aes(x = killed_by), fill = "red") +
       scale_y_continuous(expand = c(0,0)) +
       theme(axis.text = element_text(size = 4),
             axis.title = element_text(size = 6, face = "bold"),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_flip()
   }, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)

