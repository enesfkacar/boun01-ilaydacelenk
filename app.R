
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
            sliderInput("years",
                        "Years",
                        min = 2000,
                        max = 2005,
                        value = c(2002,2003),
                        sep = ""
                        ),
            selectInput(
                inputId="genre",
                label="Genre",
                choices=c("All", genres),
                selected="All"),
            sliderInput("minvotes",
                        "At Least X Votes",
                        min = min(shiny_movie_set$votes),
                        max = max(shiny_movie_set$votes),
                        value = median(shiny_movie_set$votes)),
        ), 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$moviePlot <- renderPlot({
        
        print("===Trigger===")
        print(input$years)
        print(input$genre)
        print(input$votes)
        
    
        plot_df <- shiny_movie_set %>% filter(year >= input$years[1] & year <= input$years[2] & votes >= input$minvotes)
        
        if(input$genre != "All"){
            plot_df <- plot_df %>% filter(genre == input$genre)
        }
        print(paste0("Number of rows: ", nrow(plot_df)))
        
        ggplot(shiny_movie_set, aes(x=length, y=rating, color=genre)) + geom_point() + xlim(0,400) + ylim(0, 10) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
