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

my_needs <- function(v1, v2, stat, v3) {
  if (stat == "counts" & !is.factor(dat[[v1]])) {
    "Cannot compute category counts of non-categorical variable."
  } else if (stat != "counts" & !is.numeric(dat[[v1]])) {
    glue("Cannot compute {stat} of a non-numeric variable.")
  } else if (v2 != "none" && !is.factor(dat[[v2]])) {
    "Must use a categorical variable to separate observations into categories."
  } else if (stat == "correlation" & (!is.numeric(dat[[v1]]) | !is.numeric(dat[[v3]]))) {
    glue("Cannot compute correlation with a non-numeric variable.")
  }
}

ui <- fluidPage(
  titlePanel("Spotify Data Query"),
  sidebarLayout(
    sidebarPanel(
      selectInput("v1", "What variable would you like to summarize?", choices = names(dat), selected = "Danceability"),
      selectInput("stat", "What statistic would you like to calculate?", 
                  choices = c("mean", "median", "std deviation", "IQR", "counts", "correlation"), selected = "mean"),
      selectInput("v2", "What variable would you like to group by?", choices = c("none", names(dat)), selected = "none"),
      selectInput("v3", "What second variable would you like to use for correlation?", choices = c("none", names(dat)), selected = "none"),
      actionButton("go", "Calculate")
    ),
    mainPanel(tableOutput("summary"))
  )
)

server <- function(input, output) {
  res <- eventReactive(input$go, {
    validate(
      need(is.null(my_needs(input$v1, input$v2, input$stat, input$v3)), my_needs(input$v1, input$v2, input$stat, input$v3))
    )
    
    if (input$stat == "counts" & input$v2 == "none") {
      dat %>% tabyl(!!sym(input$v1))
    } else if (input$stat == "counts") {
      as.data.frame(table(dat[[input$v1]], dat[[input$v2]]))
    } else {
      my_func <- switch(input$stat,
                        mean = mean,
                        median = median,
                        IQR = IQR,
                        "std deviation" = sd,
                        "correlation" = cor)
      
      if (input$v2 == "none") {
        dat %>%
          summarize_at(input$v1, my_func) %>%
          mutate_if(is.numeric, ~round(.,2))
      } else if (input$stat != "correlation") {
        temp <- dat %>%
          group_by(!!sym(input$v2)) %>%
          summarize_at(input$v1, my_func) %>%
          mutate_if(is.numeric, ~round(.,2))
        
        colnames(temp)[1] <- input$v2
        temp
      } else {
        temp <- dat %>%
          group_by(!!sym(input$v2)) %>%
          select(input$v1, input$v3) %>%
          nest()
        
        cors <- map_dbl(temp$data, ~cor(.x[[1]], .x[[2]], use = "complete.obs"))
        
        tibble(Category = temp[[1]], Correlation = cors)
      }
    }
  })
  
  output$summary <- renderTable({ res() })
}

shinyApp(ui, server)
