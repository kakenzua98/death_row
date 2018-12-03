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
library(ggplot2)
theme_set(theme_classic())

#figPath = system.file("examples/peace.png", package = "wordcloud2")
top_words <- read_rds("top_words.rds")
sentiment_by_time <- read_rds("sentiment_by_time.rds")
race_sentiment_tbl <- read_rds("race_sentiment_tbl.rds")
word_cloud <- read_rds("word_cloud.rds")
word_freq <- read_rds("word_freq.rds")
state_executions <- read_rds("state_executions.rds")

race_choices <- c("All",
                  "Black",
                  "Hispanic",
                  "White")

#year_choices <- c(levels(state_executions$date))

year_choices <- c("2018",
                         "2017",
                         "2016",
                         "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008")


# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Sentiment Analysis of Death Row Executions"),
  tabsetPanel(
    tabPanel(
      title = "Introduction",
      h2("Summary:"),
      h2("Findings:"), 
      h2("Source of Data"),
      h3("Relevant Reading Materials:")),
    tabPanel(
      title = "Why Texas?",
      h3("Texas Conducts More Executions than Other States"),
      h5("Texas has seen a rise in death penalty executions, as shown below. Sentiment Analysis of the men and women executed in Texas can provide more detail than that of any of state because of the open record and sheer number of data points."),
      checkboxInput(inputId = "state_executions_tbl", 
                    label = "Show Summary Table", 
                    value = FALSE),
      br(),
      br(),
      h4("Pie Chart of State Executions:"),
      p("The pie chart below show state executions over the past 10 years. As shown below, Texas has taken up a considerable majority over the past few years, with an apparent rise in the number of executions"),
      selectInput(inputId = "year",
                  label = "Year:",
                  choices = year_choices,
                  selected = "2018"),
      plotOutput("piePlot"),
      br(),
      br(),
      h4("Timeline of Executions in the 5 Top States:"),
      p("The line graph below is limited to states that have conducted excecutions over the last year since many states have a 'death row' but very rarely conduct executions, such as CA. For these states, the death penalty is often symbolic of the severity of the offender's actins rather than the result of their time in prison. The offenders often just spend the rest of their lives on 'death row'"),
      plotOutput("statePlot"),
      DT::dataTableOutput("state_table")),
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
   
  
  output$piePlot <- renderPlot({
    
    pie_chart <- state_executions %>% 
      filter(str_detect(date, input$year)) %>% 
      ggplot(aes(x= "", y= n, fill= state)) + geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="state", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of Executions", 
           caption="Source: Death Penalty Information Center")
    
    pie_chart + coord_polar(theta = "y", start=0)
    
    
  })
  
  output$statePlot <- renderPlot({
    
    state_plot <- state_executions %>% 
      filter(state == c("TX","GA","TN","AL","GA","NE","OH","SD","FL")) %>% 
      ggplot(aes(x = date, y = n, color = state)) + geom_line(size = 1.5) +
      theme( plot.title = element_text(hjust=0.5)) + 
      labs(fill="state", 
           x="Year", 
           y="Number of Executions", 
           title="Pie Chart of Executions", 
           caption="Source: Death Penalty Information Center")
    
    state_plot
    
  })
   
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

   output$state_table <- DT::renderDataTable({
     if (input$state_executions_tbl == TRUE) {
       state_executions 
     }
   })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

