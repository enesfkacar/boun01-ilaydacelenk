#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
    movies %>% 
    filter(year >= 2000) %>%
    select(title,year,length,rating,votes,Action:Short) %>% 
    gather(genre,value,Action:Short) %>% 
    filter(value == 1) %>% 
    select(-value)

# Get genre list
genres <- 
    shiny_movie_set %>% 
    distinct(genre) %>% 
    unlist(.)

names(genres) <- NULL



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Movie Length and IMDB Scores"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Years",
                        min = 2000,
                        max = 2005,
                        value = 2002),
            selectInput(
                inputId="dropdown",
                label="Genre",
                choices=c("All", "genre1", "genre2", "genre3", "genre4"),
                selected="All"),
            sliderInput("bins",
                        "At Least X Votes",
                        min = 0,
                        max = 158000,
                        value = 0),
        ), 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        ggplot(shiny_movie_set, aes(x=length, y=rating, color=genre)) + geom_point() + xlim(0,400) + ylim(0, 10) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
