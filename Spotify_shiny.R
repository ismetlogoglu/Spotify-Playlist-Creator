#??smet Selman Lo??o??lu 2507515
#Emre Ta??k??n 2561538



#necessary libraries
library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(shinydashboard)

#Import data set
dataset <- read.csv("C:/Users/ismet/Desktop/dataset.csv")

# User Interface part
ui <- dashboardPage(
  dashboardHeader(title = "Music Tracks Data Explorer"),
  dashboardSidebar( #Our aims here to show that see each menu separately. We used shinyDashboard packages to separate sidebar 
    sidebarMenu(
      menuItem("Data Exploration", tabName = "exploration", icon = icon("table")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Modeling", tabName = "modeling", icon = icon("cogs"))
    )
  ),
  dashboardBody( #we prepared every side bar context each. We built up every tab by constructing pattern.
    tabItems(
      tabItem(tabName = "exploration",
              fluidPage(
                titlePanel("Best Playlist Finder"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("popularity", "Popularity:", min = 0, max = 100, value = c(50, 100)),
                    sliderInput("danceability", "Danceability:", min = 0, max = 1, value = c(0.5, 1)),
                    sliderInput("energy", "Energy:", min = 0, max = 1, value = c(0.5, 1)),
                    selectInput("genre", "Genre:", choices = unique(dataset$track_genre), selected = unique(dataset$track_genre)[1]),
                    textInput("artist", "Artist:", value = "")
                    
                  ),
                  mainPanel(
                    DTOutput("table"),
                    style = "overflow-x: auto;"  # We add horizontal scrolling since data vanished when we scrolling down.
                  )
                )
              )),
      tabItem(tabName = "visualization",
              fluidPage(
                titlePanel("Visualization"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("vis_genre", "Choose a Genre:", 
                                choices = unique(dataset$track_genre))
                  ),
                  mainPanel( #we have added bar plot to the main panel. We've generated bar plot to see some numeric var. comparison by selecting genre 
                    plotOutput("barPlot")
                  )
                )
              )),
      tabItem(tabName = "modeling",
              fluidPage(
                titlePanel("Modeling"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("model_genre", "Choose a Genre:", 
                                choices = unique(dataset$track_genre)),
                    selectInput("model_response", "Choose Response Variable:", 
                                choices = c("popularity", "danceability", "energy", "valence", "liveness", "acousticness", "speechiness", "instrumentalness")),
                    selectInput("model_predictor", "Choose Predictor Variable:", 
                                choices = c("duration_ms", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "tempo"))
                  ),
                  mainPanel(
                    verbatimTextOutput("modelSummary")
                  )
                )
              ))
    )
  )
)

# Server
server <- function(input, output) { #we built up server block
  
  # Data Exploration
  filteredData <- reactive({
    dataset %>%
      filter(
        popularity >= input$popularity[1],
        popularity <= input$popularity[2],
        danceability >= input$danceability[1],
        danceability <= input$danceability[2],
        energy >= input$energy[1],
        energy <= input$energy[2],
        grepl(input$artist, artists, ignore.case = TRUE),
        track_genre == input$genre
      )
  })
  
  output$table <- renderDT({
    datatable(filteredData())
  })
  
  # Visualization - Bar Plot
  output$barPlot <- renderPlot({
    data <- dataset %>% filter(track_genre == input$vis_genre)
    summary_data <- data %>%
      summarize(
        danceability = mean(danceability),
        energy = mean(energy),
        valence = mean(valence),
        liveness = mean(liveness),
        acousticness = mean(acousticness),
        speechiness = mean(speechiness),
        instrumentalness = mean(instrumentalness)
      ) %>%
      pivot_longer(cols = everything(), names_to = "metric", values_to = "value")
    
    ggplot(summary_data, aes(x = metric, y = value, fill = metric)) +
      geom_bar(stat = "identity") +
      labs(x = "Metric", y = "Average Value", fill = "Metric", title = "Average Metrics by Genre") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Modeling
  output$modelSummary <- renderPrint({
    data <- dataset %>% filter(track_genre == input$model_genre)
    model <- lm(as.formula(paste(input$model_response, "~", input$model_predictor)), data = data)
    summary(model)
  })
}
#Let see it!!
shinyApp(ui, server)
