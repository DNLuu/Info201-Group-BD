library(shiny)
library("dplyr")
library("ggplot2")
library("png")
library("grid")

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("PUBG Deaths"),
   
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #   sidebarPanel(
   #      sliderInput("bins",
   #                  "Number of bins:",
   #                  min = 1,
   #                  max = 50,
   #                  value = 30)
   #   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   
)

# Define server 
server <- function(input, output) {
   erangelData <- read.csv("data_subset.csv")

   
   output$distPlot <- renderPlot({
      img <- readPNG("erangel.PNG")
      bg <- rasterGrob(img, interpolate = FALSE, width=unit(1,"npc"), height=unit(1,"npc"))
     
      ggplot(data = erangelData) +
         ggtitle("PUBG Deaths") +
         annotation_custom(bg, xmin = 0,  xmax = 800000, ymin = -800000, ymax = 0) +
         geom_point(mapping = aes(x = victim_position_x * (800000/812800), y = victim_position_y * (800000/812800), color = "red"), alpha = 0.04) + #0.008
         xlim(0, 800000) + scale_y_reverse(lim=c(800000, 0))
   }, height = 800, width = 1200) # size of map
}

# Run the application 
shinyApp(ui = ui, server = server)

