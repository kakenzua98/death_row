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
library(stringr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
theme_set(theme_classic())



#figPath = system.file("examples/peace.png", package = "wordcloud2")
top_words <- read_rds("top_words.rds")
sentiment_by_time <- read_rds("sentiment_by_time.rds")
race_sentiment_tbl <- read_rds("race_sentiment_tbl.rds")
word_cloud <- read_rds("word_cloud.rds")
word_freq <- read_rds("word_freq.rds")
state_executions <- read_rds("state_executions.rds")
row_exec <- read_rds("row_exec.rds")
last_words <- read_rds("last_words.rds")

race_choices <- c("All",
                  "Black",
                  "Hispanic",
                  "White")

year_choices <- state_executions %>%
  mutate(year = str_sub(date, start = 1, end = 4)) %>%
  select(year) %>%
  unique() %>%
  arrange(year)

# HOW COULD I DO LEVELS FOR THIS
# year_choices <- c("2018",
#                          "2017",
#                          "2016",
#                          "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008")

sentiment_choices <- c(
  "Positive" = "positive",
  "Anticipation" = "anticipation",
  "Joy" = "joy",
  "Trust" = "trust",
  "Sadness" = "sadness",
  "Surprise" = "surprise",
  "Anger" = "anger",
  "Fear" = "fear",
  "Disgust" = "disgust",
  "Negative" = "negative"
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Sentiment Analysis of Death Row Executions",
    tabPanel(
      title = "Introduction",
      h2("Summary:"),
      h2("Findings:"),
      h2("Source of Data"),
      h3("Relevant Reading Materials:")
    ),
    tabPanel(
      title = "Why Texas?",
      h3("Texas Conducts More Executions than Other States"),
      h5(
        "Texas has seen a rise in death penalty executions, as shown below. Sentiment Analysis of the men and women executed in Texas can provide more detail than that of any of state because of the open record and sheer number of data points."
      ),
      br(),
      h4("Share of Executions Each Year:"),
      p(
        "The pie chart below show state executions over the past 10 years. As shown below, Texas has taken up a considerable majority over the past few years, with an apparent rise in the number of executions"
      ),
      br(),
      sidebarPanel(
        p("Select a year to see the breakdown of executions between states."),
        selectInput(
          inputId = "year",
          label = "Year:",
          choices = year_choices,
          selected = "2018"
        ),
        br(),
        p(
          "Summary Table contains additional information about specifc offenders, counties, and victims."
        ),
        checkboxInput(
          inputId = "state_executions_tbl",
          label = "Show Summary Table",
          value = FALSE
        )
      ),
      br(),
      mainPanel(plotOutput("piePlot")),
      br(),
      br(),
      br(),
      br(),
      h4("The Past 30 Years of Dealth Penalty Executions:"),
      p(
        "Though the amount of death row executions have descreased nationwide, Texas has maintained high levels of executions. In 2018, Texas conducted nearly half of the executions nationwide even though the death penalty in legal in 31 states including California and Florida"
      ),
      dataTableOutput("state_table"),
      plotlyOutput("overallPlot")
    ),
    tabPanel(
      title = "Most Common Words",
      h2("The Most Common Words"),
      sidebarLayout(# does not currently work but i want it toooo
        sidebarPanel(
          p(
            "Select options below to view the breakdown between sentiments in the last words of death row inmates."
          ),
          pickerInput(
            inputId = "multi_sent",
            label = "Select Multiple Sentiments",
            choices = sentiment_choices,
            selected = c("positive", "negative"),
            multiple = TRUE
          )
        ),
        mainPanel(plotOutput("wordPlot"),
                  br())),
      h2("Word Cloud"),
      numericInput(
        inputId = "num_cloud",
        label = "Maximum number of words",
        value = 100,
        min = 5
      ),
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
      sidebarPanel(
        selectInput(
          inputId = "race",
          label = "Race:",
          choices = race_choices,
          selected = "All"
        ),
        sliderInput(
          inputId = "age",
          label = "Age",
          min = 27,
          max = 67,
          value = c(30, 50)
        ),
        checkboxInput(
          inputId = "line",
          label = "Show Best Fit Line",
          value = FALSE
        ),
        checkboxInput(
          inputId = "offender_info",
          label = "Show offender_info",
          value = FALSE
        ),
        checkboxInput(
          inputId = "race_sent_tbl",
          label = "Show Summary Table",
          value = FALSE
        )
      ),
      #checkboxGroupButtons(inputId = "Id038"),
      mainPanel(plotlyOutput("timePlot"),
                br(),
                dataTableOutput("table"))
    ),
    
    tabPanel(
      title = "Specific Inmates",
      sidebarPanel(actionButton("explore", "Generate New Set")),
      dataTableOutput("offender_sample")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$piePlot <- renderPlot({
    pie_chart <- state_executions %>%
      count(date, state) %>%
      filter(str_detect(date, input$year)) %>%
      ggplot(aes(x = "", y = n, fill = state)) + geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(
        fill = "state",
        x = NULL,
        y = NULL,
        caption = "Source: Death Penalty Information Center"
      )
    
    pie_chart + coord_polar(theta = "y", start = 0)
    
    
  })
  
  
  output$overallPlot <- renderPlotly({
    overall_plot <- state_executions %>%
      count(date, state) %>%
      mutate(state = case_when(state == "TX" ~ "Texas",
                               TRUE ~ "Rest of the US")) %>%
      ggplot(aes(x = date, y = n, fill = state)) + geom_col() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(
        fill = "state",
        x = "Year",
        y = "Number of Executions",
        title = "Texas vs. The Rest of the United States",
        caption = "Source: Death Penalty Information Center"
      )
    
    overall_plot
    
  })
  
  output$wordPlot <- renderPlot({
    top_words_plot <- top_words %>%
      #top_n(input$number) %>%
      filter(sentiment %in% input$multi_sent) %>%
      ggplot(aes(x = "", y = n, fill = sentiment)) + geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(
        fill = "sentiment",
        x = NULL,
        y = NULL,
        title = paste("looking at", nrow(top_words), "words"),
        caption = "Source: Texas Department of Criminal Justice"
      )
    
    top_words_plot + coord_polar(theta = "y", start = 0)
    # Make a bar chart with geom_col
  })
  
  output$cloud <- renderWordcloud2({
    # Create a word cloud object
    
    #wordcloud2(word_freq, figPath = "texas.png", size = 1.5,color = "skyblue")
    wordcloud2(word_freq, size = 2)
  })
  
  output$agePlot <- renderPlotly({
    by_age <- sentiment_by_time %>%
      count(age, sentiment, total_words) %>%
      ungroup() %>%
      mutate(percent = n / total_words) %>%
      ggplot(aes(age, percent, color = sentiment)) +
      #geom_boxplot()
      geom_line(size = 1.5)
    
    by_age
    
    
  })
  
  
  
  output$timePlot <- renderPlotly({
    if (input$race != "All") {
      sentiment_by_time <- sentiment_by_time %>%
        filter(offender_race == input$race)
    }
    
    sen_by_time_plot <- sentiment_by_time %>%
      filter(age >= input$age[1] & age <= input$age[2]) %>%
      count(full_name, date, sentiment, total_words) %>%
      #ungroup() %>%
      mutate(percent = (n / total_words) * 100) %>%
      ggplot(aes(date, percent, color = sentiment)) +
      geom_point(size = 1.5)
    
    # sen_by_time_plot <- ggplotly(sen_by_time_plot)
    # chart_link = plotly_POST(p, filename="hover/tooltip")
    # chart_link
    
    if (input$line == TRUE) {
      sen_by_time_plot <-
        sen_by_time_plot + geom_smooth(method = "lm",
                                       se = FALSE,
                                       lty = 2)
    }
    
    if (input$offender_info == TRUE) {
      sen_by_time_plot <-
        sen_by_time_plot + geom_label_repel(aes(label = toupper(full_name)), size = 3, force = 3)
    }
    
    
    sen_by_time_plot
  })
  
  
  output$table <- renderDataTable({
    if (input$race_sent_tbl == TRUE) {
      if(input$race != "All") {
        sentiment_by_time <- sentiment_by_time %>%
          filter(offender_race == input$race) 
      }
      
      sentiment_by_time <- sentiment_by_time %>% 
        filter(age >= input$age[1] & age <= input$age[2]) 
    }
  })
  
  output$state_table <- renderDataTable({
    if (input$state_executions_tbl == TRUE) {
      state_executions %>%
        filter(str_detect(date, input$year))
      #filter(age >= input$age[1] & age <= input$age[2])
    }
  })
  
  
  
  output$offender_sample <-
    renderTable(
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      spacing = "l",
      {
        input$explore
        
        last_words %>%
          sample_n(3)
      }
    )
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
