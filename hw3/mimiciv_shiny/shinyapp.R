# "Biostat 203B Homework 3" - Shiny app
# Due Mar 1 @ 11:59PM
# author: Sylvia Wang 105118268

sessionInfo()

# app.R
library(shiny)
library(tidyverse)

icu_cohort <- readRDS("./mimiciv_shiny/icu_cohort.rds")
table(icu_cohort$thirty_day_mort)

#UI
ui <- fluidPage(
  titlePanel("Summaries of the ICU cohort data"),
  tabsetPanel(
    tabPanel("Histogram", 
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               mainPanel(
                 plotOutput(outputId = "distPlot")
               )
             )
    ),
    tabPanel("Summary Statistics",
             fluidRow(
               column(width = 6,
                      h3("Summary Statistics"),
                      verbatimTextOutput("summary")
               ),
               column(width = 6,
                      h3("Boxplot"),
                      plotOutput("boxplot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- icu_cohort$admittime
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Admission time",
         main = "Histogram of admission time")
  })
}

#Launch the app
shinyApp(ui, server)



