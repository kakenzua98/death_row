#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(ggrepel)
library(datasets)

top_words <- read_rds("top_words.rds")
sentiment_by_time <- read_rds("sentiment_by_time.rds")
race_sentiment_tbl <- read_rds("race_sentiment_tbl.rds")

race_choices <- c("All",
                  "Black",
                  "Hispanic",
                  "White")

# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Sentiment Analysis of Death Row Executions"),
  tabsetPanel(
    tabPanel(
      title = "Most Common Words",
      numericInput("number", "Number of Words", 15, 1),
      plotOutput("wordPlot")),
    # Create "Table" tab
    
    tabPanel(
      title = "Time Plot",
      selectInput(inputId = "race",
                  label = "Race:",
                  choices = race_choices,
                  selected = "All"),
      sliderInput(inputId = "age", label = "Age",
                  min = 27, max = 67,
                  value = c(30, 50)),
      checkboxInput(inputId = "line", 
                    label = "Show Best Fit Line", 
                    value = FALSE),
      plotOutput("timePlot"),
      DT::dataTableOutput("table")),
    
    tabPanel(
      title = "Age Plot",
      plotOutput("agePlot"))
    
    
    
  ))
  


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$wordPlot <- renderPlot({
     
     top_words_plot <- top_words %>% 
       top_n(input$number) %>% 
       ggplot(aes(word, n, fill = sentiment)) +
       # Make a bar chart with geom_col()
       
       geom_col(show.legend = FALSE) +
       facet_wrap(~sentiment, scales = "free") +  
       coord_flip()
     
     top_words_plot
      
     
   })
   
   output$agePlot <- renderPlot({
     
     by_age <- sentiment_by_time %>% 
       count(age, sentiment, total_words) %>%
       ungroup() %>%  
       mutate(percent = n / total_words) %>% 
       ggplot(aes(age, percent, color = sentiment)) +
       #geom_boxplot()
       geom_line(size = 1.5)
     
     by_age
     
     
   })
   
   
   
   output$timePlot <- renderPlot({
     
     # ALL IS NOT WORKING
     
     if(input$race == "All") {
       sen_by_time_plot <- sentiment_by_time %>%
         count(date, age, sentiment, total_words) %>%
         ungroup() %>%  
         mutate(percent = n / total_words) %>% 
         ggplot(aes(date, percent, color = sentiment)) +
         geom_line(size = 1.5) 
       
       if (input$line == TRUE) {
         sen_by_time_plot <- sen_by_time_plot + geom_smooth(method = "lm", se = FALSE, lty = 2)
       }
       
       sen_by_time_plot
       
     }
     
     if(input$race != "All") {
       sen_by_time_plot <- sentiment_by_time %>% 
       filter(offender_race == input$race) %>% 
       #filter(age >= input$age[1] & age <= input$age[2])
       count(date, sentiment, total_words) %>%
       ungroup() %>%  
       mutate(percent = n / total_words) %>% 
       ggplot(aes(date, percent, color = sentiment)) +
       geom_line(size = 1.5) 
       
       if (input$line == TRUE) {
         sen_by_time_plot <- sen_by_time_plot + geom_smooth(method = "lm", se = FALSE, lty = 2)
       }

     
     sen_by_time_plot
     }
     
     
     
   })
   
   output$table <- DT::renderDataTable({
     race_sentiment_tbl 
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

