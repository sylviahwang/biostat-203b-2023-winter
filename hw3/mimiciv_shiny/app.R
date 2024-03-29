# "Biostat 203B Homework 3" - Shiny app
# Due Mar 1 @ 11:59PM
# author: Sylvia Wang 105118268

sessionInfo()

# app.R
library(shiny)
library(tidyverse)

icu_cohort <- readRDS("icu_cohort.rds")
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
                             choices = c("Gender", "Anchor age",
                                         "Age at admission", "Ethnicity",
                                         "Language", "Insurance",
                                         "Marital status", "Admission type",
                                         "Thirty-day mortality")),
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               mainPanel(
                 helpText("Summary statistics:"),
                 tableOutput("demotable"),
                 helpText("Histogram:"),
                 plotOutput(outputId = "demoPlot")
               )
             )
    ),
    
    tabPanel("Lab measurements",
             sidebarLayout(
               sidebarPanel(
                 selectInput("labvar",
                             label = "Lab measurements:",
                             choices = c("Bicarbonate", "Chloride",
                                         "Creatinine", "Glucose",
                                         "Hematocrit", "Potassium",
                                         "Sodium", "White blood cell")),
                 sliderInput(inputId = "labbins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 numericInput(inputId = "labobs",
                              label = "Number of observations to view:",
                              value = 10)
               ),
               mainPanel(
                 helpText("Summary statistics:"),
                 verbatimTextOutput("labsummary"),
                 helpText("Histogram:"),
                 plotOutput(outputId = "labplot"),
                 helpText("Data preview:"),
                 tableOutput("labview")
               )
             )
    ),

    tabPanel("Vitals",
             sidebarLayout(
               sidebarPanel(
                 selectInput("vitalvar",
                             label = "Vitals:",
                             choices = c("Heart Rate", 
                                         "Non Invasive Blood Pressure systolic",
                                         "Non Invasive Blood Pressure mean", 
                                         "Respiratory Rate",
                                         "Temperature Fahrenheit")),
                 sliderInput(inputId = "vitalbins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 numericInput(inputId = "vitalobs",
                              label = "Number of observations to view:",
                              value = 10)
               ),
               mainPanel(
                 helpText("Summary statistics:"),
                 verbatimTextOutput("vitalsummary"),
                 helpText("Histogram:"),
                 plotOutput(outputId = "vitalplot"),
                 helpText("Data preview:"),
                 tableOutput("vitalview")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$demotable <- renderTable({
    x <- switch(input$demo,
                Gender                 = icu_cohort %>% count(gender),
                "Anchor age"           = summarise(icu_cohort, 
                                            mean = mean(anchor_age),
                                            median = median(anchor_age),
                                            sd = sd(anchor_age),
                                            min = min(anchor_age),
                                            max = max(anchor_age)),
                "Age at admission"     = summarise(icu_cohort, 
                                            mean = mean(age_hadm),
                                            median = median(age_hadm),
                                            sd = sd(age_hadm),
                                            min = min(age_hadm),
                                            max = max(age_hadm)),
                Ethnicity              = icu_cohort %>% count(ethnicity),
                Language               = icu_cohort %>% count(language),
                Insurance              = icu_cohort %>% count(insurance),
                "Marital status"       = icu_cohort %>% count(marital_status),
                "Admission type"       = icu_cohort %>% count(admission_type),
                "Thirty-day mortality" = icu_cohort %>% count(thirty_day_mort))
    x
  })
  
  output$demoPlot <- renderPlot({
    x <- switch(input$demo,
                Gender                 = icu_cohort$gender,
                "Anchor age"           = icu_cohort$anchor_age,
                "Age at admission"     = icu_cohort$age_hadm,
                Ethnicity              = icu_cohort$ethnicity,
                Language               = icu_cohort$language,
                Insurance              = icu_cohort$insurance,
                "Marital status"       = icu_cohort$marital_status,
                "Admission type"       = icu_cohort$admission_type,
                "Thirty-day mortality" = icu_cohort$thirty_day_mort)
    
    if (input$demo %in% c("Anchor age", "Age at admission")) {
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "#106e82", border = "white",
           xlab = input$demo,
           main = paste("Histogram of", input$demo))
    }
    else if (input$demo %in% c("Ethnicity", "Admission type")) {
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
  
  labInput <- reactive({
    switch(input$labvar,
           Bicarbonate        = icu_cohort$bicarbonate,
           Chloride           = icu_cohort$chloride,
           Creatinine         = icu_cohort$creatinine,
           Glucose            = icu_cohort$glucose,
           Hematocrit         = icu_cohort$hematocrit,
           Potassium          = icu_cohort$potassium,
           Sodium             = icu_cohort$sodium,
           "White blood cell" = icu_cohort$wbc_count)
  })
  
  output$labsummary <- renderPrint({
    labvar <- labInput()
    summary(labvar[!is.na(labvar)])
  })
  
  output$labview <- renderTable({
    head(labInput(), n = input$labobs)
  })
  
  output$labplot <- renderPlot({
    x <- switch(input$labvar,
                Bicarbonate        = icu_cohort$bicarbonate,
                Chloride           = icu_cohort$chloride,
                Creatinine         = icu_cohort$creatinine,
                Glucose            = icu_cohort$glucose,
                Hematocrit         = icu_cohort$hematocrit,
                Potassium          = icu_cohort$potassium,
                Sodium             = icu_cohort$sodium,
                "White blood cell" = icu_cohort$wbc_count)
    x <- na.omit(x)
    
    labbins <- seq(min(x), max(x), length.out = input$labbins + 1)
    hist(x, breaks = labbins, col = "#106e82", border = "white",
         xlab = input$labvar,
         main = paste("Histogram of", input$labvar))
  })
  
  vitalInput <- reactive({
    switch(input$vitalvar,
           "Non Invasive Blood Pressure systolic" = icu_cohort$bp_systolic,
           "Non Invasive Blood Pressure mean"     = icu_cohort$bp_mean,
           "Respiratory Rate"                     = icu_cohort$resp_rate,
           "Heart Rate"                           = icu_cohort$heart_rate,
           "Temperature Fahrenheit"               = icu_cohort$temp) 
  })
  
  output$vitalsummary <- renderPrint({
    vitalvar <- vitalInput()
    vitalvar <- vitalvar[vitalvar >= 0]
    summary(vitalvar[!is.na(vitalvar)])
  })
  
  output$vitalview <- renderTable({
    head(vitalInput(), n = input$vitalobs)
  })
  
  output$vitalplot <- renderPlot({
    x <- switch(input$vitalvar,
                "Non Invasive Blood Pressure systolic" = icu_cohort$bp_systolic,
                "Non Invasive Blood Pressure mean"     = icu_cohort$bp_mean,
                "Respiratory Rate"                     = icu_cohort$resp_rate,
                "Heart Rate"                           = icu_cohort$heart_rate,
                "Temperature Fahrenheit"               = icu_cohort$temp) 
    x <- na.omit(x)
    x <- subset(x, x >= 0)
    
    vitalbins <- seq(min(x), max(x), length.out = input$vitalbins + 1)
    hist(x, breaks = vitalbins, col = "#106e82", border = "white",
         xlab = input$vitalvar,
         main = paste("Histogram of", input$vitalvar))
  })
  
}

#Launch the app
shinyApp(ui, server)
