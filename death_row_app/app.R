#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

######################
## Loading packages ##
######################

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
library(tidytext)
theme_set(theme_classic())

top_words <- read_rds("top_words.rds")
sentiment_by_time <- read_rds("sentiment_by_time.rds")
word_cloud <- read_rds("word_cloud.rds")
word_freq <- read_rds("word_freq.rds")
state_executions <- read_rds("state_executions.rds")
last_words <- read_rds("last_words.rds")

###########################################
## Setting Choices for Inputs in the UI ##
##########################################

race_choices <- c("All",
                  "Black",
                  "Hispanic",
                  "White")

year_choices <- state_executions %>%
  mutate(year = str_sub(date, start = 1, end = 4)) %>%
  select(year) %>%
  unique() %>%
  arrange(year)


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

########
## UI ##
########

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # NAVIGATION BARS: navbarPage function enables a better layout for the navigation bar tabs
  
  
  navbarPage(
    "Analysis of Death Row Inmates in Texas",
    tabPanel(
      title = "Introduction",
      h5("By Kemi Akenzua for the GOV1005 Final Project"),
      h3("Summary:"),
      p(
        "Texas published the last statements of every death row inmate they've executed. This presents a great opportunity to analysis what many think is an outdated practice from a entirely inaccessible perspective: that of the executed inmates."
      ),
      br(),
      h3("Organization of the App:"),
      p(
        "Why Texas:? On this page I show why Texas is one of, if not the, most important states when discussing the death penalty."
      ),
      p(
        "Most Common Words: On this page, you can see specific visualizations of the breakdown of sentiments in all Last Statements by selecting which sentiment to include in a pie chart. There is also a word cloud that shows the most commonly used words based on the sentiments selected."
      ),
      p(
        "Time Plot: This graph plots the percentage of positive or negative words. Users can look at all subjects or subset based on race and/or age."
      ),
      p(
        "Specific Inmates: This tab allows users see a random set of subjects and their last statements."
      ),
      br(),
      h3("Some Findings:"),
      p(
        "As shown on the 'Why Texas?' tab, I found that Texas conducted nearly half of the death row executions nationwide in 2018. Though executions have decreased since 1999, Texas has maintained a large margin of the total executions and now, as stated, conducted the majority of them. Additionally, though 31 states have not outlawed the death penalty, less than 10 actually conducted any executions thsi year. This reaffirms some of the statements made in the Medium articles that discuss death penalty being more of a de factor life sentence in most states.   Sentiment Analysis begins on the Most Common Words tab which shows that a majority of the words in Last Statements are connote positivity and/or trust. The word cloud below shows that one of the most common words in Last Statements by the subjects was the word good with 55 entries.  The time plot is inconclusive. However, it does show that the percentage of positive words used has increased at a higher rate than the percentage of negative words used. "
      ),
      p(
        "Poke around with the data and see if you can find more interesting insights."
      ),
      br(),
      h2("Relevant Reading Materials and Sources:"),
      p(
        "https://medium.com/bigger-picture/kill-the-death-penalty-ea38c8929e30, https://medium.com/s/story/love-is-the-most-common-word-in-death-row-last-statements-f15ab0e8ad16, https://deathpenaltyinfo.org/views-executions, http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
      )
    ),
    
    #
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
      
      # I have multiple selectInputs.
      # The choices, which is the list of values to select from, are set equal lists created earlier.
      # Here the user selects with a drop down menu.
      # The choices are coded earlier in the app.R and enable the user to choose any year with available data
      
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
        
        # I have a checkboxInput to show the summary data table.
        # When the user clicks on either checkbox, it is set equal to true and the graph is shown.
        
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
      p(
        "The data visualizations below are based on sentiment analysis of Last Statements by the subjects."
      ),
      sidebarLayout(
        # does not currently work but i want it toooo
        sidebarPanel(
          p(
            "Select options below to view the breakdown between sentiments in the last words of death row inmates."
          ),
        
          # The pickerInput lets the user pick which sentiments they want to be included in the pie chart
          # This input is cleaner than check boxer because it collapses like a selectInput but allows you to check multiple similar to checkboxes
          
          pickerInput(
            inputId = "multi_sent",
            label = "Select Multiple Sentiments",
            choices = sentiment_choices,
            selected = c("positive", "negative"),
            multiple = TRUE
          )
        ),
        
        # I have the charts in the mainPanel so that they will be formatted next to (rather than below) the sidebar options
        # I've added multiple breaks because I think it spaces it better
        
        mainPanel(
          h2("Pie Charts of Sentiments"),
          p(
            "The chart below shows the breakdown between sentiments users selects."
          ),
          p(
            "Note: There are some words that may count as different sentiments based on context. This overlap is shown by colors that visibly overlap below."
          ),
          plotOutput("wordPlot"),
          br(),
          h2("Word Cloud"),
          p(
            "Hover to see the word and how many times it was used in all Last Statements."
          ),
          wordcloud2Output("cloud")
        )
      )
      
    ),
    # Create "Table" tab
    
    tabPanel(
      title = "Sentiments Over Time",
      h2(
        "View Changes in Sentiment over Time Amongst Texas Death Row Subjects"
      ),
      p(
        "Use the options in the side panel to view the graph comparing the year and percentage of positive or negative sentiments."
      ),
      p("Note: The calculations below are not weighted."),
      
      # I put the input functions in the sidebarPanel to format each tab better
      # I used selectInput for race since there are 4 choices and only one can be chosen at time
      # I use checkboxInput for best fit line since there's only two options which is easily handled with the value argument
      # I also use checkboxInput for the summary table
      # I use plotlyOutput so the graph will have plotly functionality for hovering, subsetting the data, etc. 
      
      sidebarPanel(
        p("Hover over any point to view additional info about that subject"),
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
          inputId = "race_sent_tbl",
          label = "Show Summary Table",
          value = FALSE
        )
      ),
      mainPanel(plotlyOutput("timePlot"),
                br(),
                dataTableOutput("table"))
    ),
    
    tabPanel(
      title = "Specific Inmates",
      h2("Read the Statements of Inmates Seconds Before Execution"),
      
      # I put the input functions in the sidebarPanel to format each tab better
      
      sidebarPanel(
        p("Show a randomized set of three inmates and their statements."),
        actionButton("explore", "Generate New Set")
      ),
      tableOutput("offender_sample")
    )
  )
)


############
## Server ##
############


# Define server logic required to draw all the graphs

server <- function(input, output) {
  output$piePlot <- renderPlot({
    pie_chart <- state_executions %>%
      
      # I count by date and state. I initially only counted by date but I needed to maintain the state info for the graph
      # I filter by the year users have inputted
      # The code to create the pie chart was based on this link: https://ggplot2.tidyverse.org/reference/coord_polar.html
      # As the link explains, pie charts are a combo of stacked bar charts and polar coordinates
      
      count(date, state) %>%
      filter(str_detect(date, input$year)) %>%
      ggplot(aes(x = "", y = n, fill = state)) + geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      
      # I fill by state and remove labels becaue I don't think they're useful to the user's analysis of the data
      
      labs(
        fill = "state",
        x = NULL,
        y = NULL,
        caption = "Source: Death Penalty Information Center"
      )
    
    pie_chart + coord_polar(theta = "y", start = 0)
    
    
  })
  
  # I use renderPlotly since I want my plot to be ggplotly with all the cinluded functionality 
  
  output$overallPlot <- renderPlotly({
    overall_plot <- state_executions %>%
      
      # I count by date and state. I initially only counted by date but I needed to maintain the state info for the graph
      # I filtered to just compared Texas and the Rest of the US since that is most useful to the purpose of this tab, which is to show Texas' share of executions
      
      count(date, state) %>%
      mutate(state = case_when(state == "TX" ~ "Texas",
                               TRUE ~ "Rest of the US")) %>%
      
      # I renamed some of the columns after I've finished counting, mutating, etc. so that the plotly labels will be more intuitive
      
      rename(Region = state,
             Executions = n,
             Date = date) %>%
      ggplot(aes(x = Date, y = Executions, fill = Region)) + geom_col() +
      
      # In the theme function I set the title of the plot and adjust it's position
      # I changed the labels to be more intuitive/repesentative of the data
      
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(
        fill = "Region",
        x = "Year",
        y = "Number of Executions",
        title = "Texas vs. The Rest of the United States",
        caption = "Source: Death Penalty Information Center"
      )
    
    overall_plot
    
  })
  
  output$wordPlot <- renderPlot({
    top_words_plot <- top_words %>%
      
      # I filter sentiment to be just the options users have selected
      # I filter by the year users have inputted
      # The code to create the pie chart was based on this link: https://ggplot2.tidyverse.org/reference/coord_polar.html
      # As the link explains, pie charts are a combo of stacked bar charts and polar coordinates
      
      
      filter(sentiment %in% input$multi_sent) %>%
      ggplot(aes(x = "", y = n, fill = sentiment)) + geom_bar(width = 1, stat = "identity") +
      
      # In the theme function I set the title of the plot and adjust it's position
      # I changed the labels to be more intuitive/repesentative of the data
      
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
  })
  
  output$cloud <- renderWordcloud2({
    
    word_freq %>%
      
      # I renamed the variables representing words to word so that I can join it with nrc sentiments; those sentiments were removed when I calculated frequency in the Rmd file
      # I filter sentiment to be just the options users have selected
      
      rename(word = Var1) %>%
      inner_join(get_sentiments("nrc")) %>%
      filter(sentiment %in% input$multi_sent)
    
    # The wordcloud2 function generates the cloud from a data fram with words along with frequency
    
    wordcloud2(word_freq, size = 2)
  })
  
  # I use renderPlotly since I want my plot to be ggplotly with all the cinluded functionality 
  
  output$timePlot <- renderPlotly({
    
    # If All is not selected, then the dataframe will be filtered by the users' choice
    
    if (input$race != "All") {
      sentiment_by_time <- sentiment_by_time %>%
        filter(offender_race == input$race)
    }
    
    # I filter by age based on the input from the sliderInput for age. I couldn't think of another way to do that; perhaps with a single slider or faceted options
    
    sen_by_time_plot <- sentiment_by_time %>%
      filter(age >= input$age[1] & age <= input$age[2]) %>%
      count(full_name, date, sentiment, total_words) %>%
      mutate(percent = (n / total_words) * 100) %>%
      
      # I renamed some of the columns after I've finished counting, mutating, etc. so that the plotly labels will be more intuitive
      
      rename(
        Name = full_name,
        Date = date,
        Percent = percent,
        Sentiment = sentiment
      ) %>%
      ggplot(aes(Date, Percent, color = Sentiment)) +
      geom_point(aes(label1 = Name), size = 1.5)

    # My best fit line code is based off of the code of Ms. Gayton and Mr. Arellano/Ms. Fridkin but different to fit 
    
    if (input$line == TRUE) {
      sen_by_time_plot <-
        sen_by_time_plot + geom_smooth(method = "lm",
                                       se = FALSE,
                                       lty = 2)
    }
    sen_by_time_plot
  })
  
  # I want the dataset to filter with the graph so I filter that here.
  # I use the same logic I used to filter the graph by filtering race only if All is not chosen.
  # Then I filter by age. 
  
  output$table <- renderDataTable({
    if (input$race_sent_tbl == TRUE) {
      if (input$race != "All") {
        sentiment_by_time <- sentiment_by_time %>%
          filter(offender_race == input$race)
      }
      
      sentiment_by_time <- sentiment_by_time %>%
        filter(age >= input$age[1] & age <= input$age[2])
    }
  })
  
  # I filter the datatable based on user input so that it facets by year for all death row executions
  # There are likey many ways to do this but I used str_detect to detect the year the user chose in the dataset
  
  output$state_table <- renderDataTable({
    if (input$state_executions_tbl == TRUE) {
      state_executions %>%
        filter(str_detect(date, input$year))
    }
  })
  
  # The table below is to give users a glimpse into specific statements through randomization
  
  output$offender_sample <-
    renderTable(
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      spacing = "m",
      digits = 0,
      {
        
        # I use input$explore so that the code runs each time the button is clicked
        # The usage of this code is based off of code from Mr. Weiss' final project and stackoverflow
        
        input$explore
        
        last_words %>%
          filter(!is.na(last_words)) %>%
          filter(last_words != "Last Statement") %>%
          
          # I rename columns so that when the dataset is rendered, it will be easier to interpret
          # I select just the columns I need so users aren't overwhelmed by data
          # I use sample to randomnly generate 5  data entries
          
          rename(
            Name = full_name,
            Statement = last_words,
            Age = age,
            Race = offender_race
          ) %>%
          select(Name, Statement, Age, Race) %>%
          sample_n(1)
        
      }
    )
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
