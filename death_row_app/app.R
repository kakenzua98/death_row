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
library(wordcloud2)

#figPath = system.file("examples/peace.png", package = "wordcloud2")
top_words <- read_rds("top_words.rds")
sentiment_by_time <- read_rds("sentiment_by_time.rds")
race_sentiment_tbl <- read_rds("race_sentiment_tbl.rds")
word_cloud <- read_rds("word_cloud.rds")
word_freq <- read_rds("word_freq.rds")

race_choices <- c("All",
                  "Black",
                  "Hispanic",
                  "White")

# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Sentiment Analysis of Death Row Executions"),
  tabsetPanel(
    tabPanel(
      title = "Introduction",
      h1("Map")),
    tabPanel(
      title = "Why Texas?",
      h1("Map")),
    tabPanel(
      title = "Most Common Words",
      h1("Column"),
      numericInput("number", "Number of Words", 15, 1),
      plotOutput("wordPlot"),
      h1("Word Cloud"),
      numericInput(inputId = "num_cloud", label = "Maximum number of words",
                 value = 100, min = 5),
      #CAUSES AN ERROR WHENEVER I: 
  #     # sliderInput("slider1", label = h3("Slider"), min = 0, 
  #     max = 100, value = 50)
  # )
      #colourInput("col", "Background colour", value = "white"),
      wordcloud2Output("cloud")
      
      ),
    # Create "Table" tab
    
    tabPanel(
      title = "Time Plot",
      selectInput(inputId = "race",
                  label = "Race:",
                  choices = race_choices,
                  selected = "Black"),
      sliderInput(inputId = "age", label = "Age",
                  min = 27, max = 67,
                  value = c(30, 50)),
      checkboxInput(inputId = "line", 
                    label = "Show Best Fit Line", 
                    value = FALSE),
      checkboxInput(inputId = "race_sent_tbl", 
                    label = "Show Summary Table", 
                    value = FALSE),
      #checkboxGroupButtons(inputId = "Id038"),
      plotOutput("timePlot"),
      DT::dataTableOutput("table")),
    
    tabPanel(
      title = "Age Plot",
      plotOutput("agePlot")),
  
    tabPanel(
      title = "Analysis",
      h1("Map"))
    
    
    
    
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
   
   output$cloud <- renderWordcloud2({
     # Create a word cloud object

     #wordcloud2(word_freq, figPath = "texas.png", size = 1.5,color = "skyblue")
     wordcloud2(word_freq, size = 2, shape = 'circle')
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
     if (input$race_sent_tbl == TRUE) {
       race_sentiment_tbl 
     }
   })

   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

