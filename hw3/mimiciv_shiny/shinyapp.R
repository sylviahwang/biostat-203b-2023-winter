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
    tabPanel("Demographics", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("demo",
                             label = "Demographics:",
                             choices = c("gender", "anchor_age",
                                         "age_hadm", "ethnicity",
                                         "language", "insurance",
                                         "marital_status", "admission_type",
                                         "thirty_day_mort")),
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               mainPanel(
                 helpText("Summary statistics:"),
                 tableOutput("demotable"),
                 helpText("Histograms:"),
                 plotOutput(outputId = "demoPlot")
               )
             )
    ),
    
    tabPanel("Lab measurements",
             sidebarLayout(
               sidebarPanel(
                 selectInput("labvar",
                             label = "Lab measurements:",
                             choices = c("bicarbonate", "chloride",
                                         "creatinine", "glucose",
                                         "hematocrit", "potassium",
                                         "sodium", "wbc_count")),
                 numericInput(inputId = "labobs",
                              label = "Number of observations to view:",
                              value = 10)
               ),
               mainPanel(
                 verbatimTextOutput("labsummary"),
                 tableOutput("labview")
               )
             )
    ),

    tabPanel("Vitals",
             sidebarLayout(
               sidebarPanel(
                 selectInput("labvar",
                             label = "Lab measurements:",
                             choices = c("bicarbonate", "chloride",
                                         "creatinine", "glucose",
                                         "hematocrit", "potassium",
                                         "sodium", "wbc_count")),
                 numericInput(inputId = "labobs",
                              label = "Number of observations to view:",
                              value = 10)
               ),
               mainPanel(
                 verbatimTextOutput("labsummary"),
                 tableOutput("labview")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$demotable <- renderTable({
    x <- switch(input$demo,
                gender          = icu_cohort %>% count(gender),
                anchor_age      = summarise(icu_cohort, 
                                            mean = mean(anchor_age),
                                            median = median(anchor_age),
                                            sd = sd(anchor_age),
                                            min = min(anchor_age),
                                            max = max(anchor_age)),
                age_hadm        = summarise(icu_cohort, 
                                            mean = mean(age_hadm),
                                            median = median(age_hadm),
                                            sd = sd(age_hadm),
                                            min = min(age_hadm),
                                            max = max(age_hadm)),
                ethnicity       = icu_cohort %>% count(ethnicity),
                language        = icu_cohort %>% count(language),
                insurance       = icu_cohort %>% count(insurance),
                marital_status  = icu_cohort %>% count(marital_status),
                admission_type  = icu_cohort %>% count(admission_type),
                thirty_day_mort = icu_cohort %>% count(thirty_day_mort))
    x
  })
  
  output$demoPlot <- renderPlot({
    x <- switch(input$demo,
                gender          = icu_cohort$gender,
                anchor_age      = icu_cohort$anchor_age,
                age_hadm        = icu_cohort$age_hadm,
                ethnicity       = icu_cohort$ethnicity,
                language        = icu_cohort$language,
                insurance       = icu_cohort$insurance,
                marital_status  = icu_cohort$marital_status,
                admission_type  = icu_cohort$admission_type,
                thirty_day_mort = icu_cohort$thirty_day_mort)
    
    if (input$demo %in% c("anchor_age", "age_hadm")) {
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "#106e82", border = "white",
           xlab = input$demo,
           main = paste("Histogram of", input$demo))
    }
    else if (input$demo %in% c("ethnicity", "admission_type")) {
      par(mar = c(15, 4, 4, 4))
      plot_data <- table(factor(x, levels = unique(x)))
      barplot(plot_data, col = "#106e82",
              xlab = input$demo,
              main = paste("Bar plot of", input$demo),
              las = 2)
    }
    else {
      plot_data <- table(factor(x, levels = unique(x)))
      barplot(plot_data, col = "#106e82",
              xlab = input$demo,
              main = paste("Bar plot of", input$demo))
    }
  })
  
  datasetInput <- reactive({
    switch(input$labvar,
           bicarbonate = icu_cohort$bicarbonate,
           chloride = icu_cohort$chloride,
           creatinine = icu_cohort$creatinine,
           glucose = icu_cohort$glucose,
           hematocrit = icu_cohort$hematocrit,
           potassium = icu_cohort$potassium,
           sodium = icu_cohort$sodium,
           wbc_count = icu_cohort$wbc_count)
  })
  
  output$labsummary <- renderPrint({
    labvar <- datasetInput()
    summary(labvar)
  })
  
  output$labview <- renderTable({
    head(datasetInput(), n = input$labobs)
  })
  
}

#Launch the app
shinyApp(ui, server)





