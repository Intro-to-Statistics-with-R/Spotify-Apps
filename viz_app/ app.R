library(shiny)
library(tidyverse)
library(janitor)
library(glue)

set.seed(7777)
dat <- read_csv("https://www.dropbox.com/s/bc2u2vjm2dqyx0l/top_artists_spotify.csv?dl=1")

dat <- dat %>%
  mutate(
    Artist = factor(Artist),
    Explicit = factor(Explicit),
    Acoustic = factor(Acousticness > 0.5),
    Live = factor(Liveness > 0.5),
    TimeSignature = factor(TimeSignature),
    Mode = factor(Mode)
  ) %>%
  select(
    Name, Album, Artist, Acoustic, Danceability, Duration, Energy, Explicit, Live, Loudness, Mode, Speechiness, Tempo, TimeSignature, Valence
  )

my_needs <- function(v1, v2, stat) {
  if (stat %in% c("barplot - counts", "barplot - percents") & !is.factor(dat[[v1]])) {
    "Cannot make a barplot from a non-categorical variable."
  } else if (!(stat %in% c("barplot - counts", "barplot - percents")) & !is.numeric(dat[[v1]])) {
    glue("Cannot make {stat} from a non-numeric variable.")
  } else if (v2 != "none" && !is.factor(dat[[v2]])) {
    "Must use a categorical variable to color plot."
  }
}

ui <- fluidPage(
  titlePanel("Spotify Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("v1", "What variable would you like to visualize?", choices = names(dat), selected = "Danceability"),
      selectInput("stat", "What type of plot would you like to make?", 
                  choices = c("barplot - counts", "barplot - percents", "histogram", "density curve", "boxplot", "scatterplot"), selected = "histogram"),
      selectInput("v2", "What variable would you like to color by?", choices = c("none", names(dat)), selected = "none"),
      selectInput("v3", "What variable would you like put on the y-axis of a scatterplot?", choices = c("none", names(dat)), selected = "none"),
      actionButton("go", "Calculate")
    ),
    mainPanel(plotOutput("viz"))
  )
)

server <- function(input, output) {
  res <- eventReactive(input$go, {
    validate(
      need(is.null(my_needs(input$v1, input$v2, input$stat)), my_needs(input$v1, input$v2, input$stat))
    )
    
    p <- ggplot(dat, aes_string(x = input$v1))
    
    if (input$stat == "barplot - counts") {
      p <- p + geom_bar(aes_string(fill = ifelse(input$v2 == "none", NA, input$v2)))
    } else if (input$stat == "barplot - percents") {
      p <- p + geom_bar(aes_string(y = ..count../sum(..count..), fill = ifelse(input$v2 == "none", NA, input$v2)), position = "fill") +
        scale_y_continuous(labels = scales::percent)
    } else if (input$stat == "histogram") {
      p <- p + geom_histogram(aes_string(fill = ifelse(input$v2 == "none", NA, input$v2)), alpha = 0.5)
    } else if (input$stat == "density curve") {
      p <- p + geom_density(aes_string(fill = ifelse(input$v2 == "none", NA, input$v2)), alpha = 0.5)
    } else if (input$stat == "boxplot") {
      p <- p + geom_boxplot(aes_string(y = ifelse(input$v2 == "none", NA, input$v2), fill = input$v2)) + theme(legend.position = "none")
    } else if (input$stat == "scatterplot") {
      p <- ggplot(dat, aes_string(x = input$v1, y = input$v3, color = input$v2)) + geom_point()
    }
    
    p + theme_minimal() + guides(fill = guide_legend(title = NULL))
  })
  
  output$viz <- renderPlot({ res() })
}

shinyApp(ui, server)
