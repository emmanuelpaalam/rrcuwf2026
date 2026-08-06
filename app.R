library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel(
    div(class = "title-panel",
        "MetaSense AI")
  ),
  mainPanel(
    width = 12,
    
    tabsetPanel(
      type = "pills",
      
      tabPanel("Descriptive Data", 
               br(),
               h3("Metabolic Syndrome Indicators"),
               p("Descriptive statistics and distributions of key sociodemographic and clinical variables for the targeted project cohort: participants within the NIH All of Us Research Program who present with both Metabolic Syndrome (MetS) and Head and Neck Cancer (HNC)."),
               br(),
               
               h3("Demographics"),
               fluidRow(
                 column(6, plotOutput("dem1")),
                 column(6, plotOutput("dem2")),
                 column(12, plotOutput("dem3")),
               ),
               
               h3("MetS Symptoms Prevalence by Subject Group"),
               fluidRow(column(12, plotOutput("mets_prev"))),
               
               h3("Metabolic Count Distribution"),
               fluidRow(column(12, plotOutput("metcount"))),
               
               h3("Physical/Mental Impact of MetS"),
               fluidRow(column(12, plotOutput("impacts"))),
               
               h3("Behavioral Data"),
               fluidRow(column(6, plotOutput("smoking")),
                        column(6, plotOutput("activity"))),
      ),
      
      tabPanel("LCA Subgroups", 
               br(),
               h3("Latent Class Analysis of Mental-Metabolic Profiles"),
               p("This section visualizes distinct latent mental-metabolic subgroups based on depression, anxiety, and individual metabolic syndrome components."),
               
               # --- Static Visuals: The What and How ---
               fluidRow(
                 column(6, 
                        h4("Conditional Item-Response Probabilities", align = "center"),
                        imageOutput("bubble_chart", height = "auto")
                 ),
                 column(6, 
                        h4("Multiple Correspondence Analysis", align = "center"),
                        imageOutput("mca_plot", height = "auto")
                 )
               ),
               
               p("Based on the Latent Class Analysis, the algorithm identified five distinct mental-metabolic profiles within the cohort:"),
               tags$ul(
                 tags$li(strong("Class 1:"), " Diabetes, hypertension, and triglyceride burden (24.8%)"),
                 tags$li(strong("Class 2:"), " Triglyceride-dominant (11.2%)"),
                 tags$li(strong("Class 3:"), " Hypertension and triglyceride burden (27.6%)"),
                 tags$li(strong("Class 4:"), " Obesity-dominant with mental health burden (6.5%)"),
                 tags$li(strong("Class 5:"), " High combined mental-metabolic burden (29.9%)")
               ),
               
               hr(),
               
               # --- Interactive Geographic Toggle ---
               h3("Sociodemographic & Geographic Breakdown"),
               p("Choose between descriptive analysis of the LCA groups for the general MetS population or those specifically located in the Florida Panhandle (ZIP codes 324**, 325**)."),
               wellPanel(
                 radioButtons("region_toggle", 
                              "Select Geographic Region for Class Distribution:",
                              choices = c("Full National Cohort", "Florida Panhandle"),
                              selected = "Full National Cohort")
               ),
               
               # Geographic Distribution Plot
               fluidRow(
                 column(12, plotOutput("geographic_distribution"))
               ),
               
               hr(),
               
               # --- Demographic RDS Plots ---
               fluidRow(
                 column(6, plotOutput("age_plot")),
                 column(6, plotOutput("sex_plot"))
               ),
               br(),
               fluidRow(
                 column(6, plotOutput("race_plot")),
                 column(6, plotOutput("education_plot"))
               ),
               br(),
               fluidRow(
                 column(12, plotOutput("marital_plot"))
               )
      )
    )
  )
)


server <- function(input, output, session) {
  output$dem1 <- renderPlot({readRDS("data/dem_birthsex.rds")})
  output$dem2 <- renderPlot({readRDS("data/dem_raceeth.rds")})
  output$dem3 <- renderPlot({readRDS("data/dem_age.rds")})
  output$mets_prev <- renderPlot({readRDS("data/metsprev.rds")})
  output$metcount <- renderPlot({readRDS("data/metcount.rds")})
  output$impacts <- renderPlot({readRDS("data/fatiguemet.rds")})
  output$smoking <- renderPlot({readRDS("data/smokrisk.rds")})
  output$activity <- renderPlot({readRDS("data/actlevels.rds")})
  
  # --- Static Image Rendering ---
  # Note: renderImage is used for non-RDS image files like PNG/TIFF
  output$bubble_chart <- renderImage({
    list(
      src = "data/lca/Classes_5_class_full_cohort.png",
      width = "130%",
      alt = "5-Class Mental-Metabolic LCA Solution"
    )
  }, deleteFile = FALSE)
  
  output$mca_plot <- renderImage({
    list(
      src = "data/lca/MCA_5_class_solution_full_cohort.png",
      width = "100%",
      alt = "Multiple Correspondence Analysis"
    )
  }, deleteFile = FALSE)
  
  # --- Interactive Geographic Toggle ---
  output$geographic_distribution <- renderPlot({
    if (input$region_toggle == "Full National Cohort") {
      readRDS("data/lca/class_dist_full.rds")
    } else {
      readRDS("data/lca/class_dist_panhandle.rds")
    }
  })
  
  # --- Demographic RDS Plots (Interactive) ---
  output$age_plot <- renderPlot({
    if (input$region_toggle == "Full National Cohort") readRDS("data/lca/class_age.rds")
    else readRDS("data/lca/class_age_panhandle.rds")
  })
  
  output$sex_plot <- renderPlot({
    if (input$region_toggle == "Full National Cohort") readRDS("data/lca/class_sex.rds")
    else readRDS("data/lca/class_sex_panhandle.rds")
  })
  
  output$race_plot <- renderPlot({
    if (input$region_toggle == "Full National Cohort") readRDS("data/lca/class_race.rds")
    else readRDS("data/lca/class_race_panhandle.rds")
  })
  
  output$education_plot <- renderPlot({
    # Note: earlier we saved this as "class_education.rds"
    if (input$region_toggle == "Full National Cohort") readRDS("data/lca/class_education.rds")
    else readRDS("data/lca/class_edu_panhandle.rds")
  })
  
  output$marital_plot <- renderPlot({
    if (input$region_toggle == "Full National Cohort") readRDS("data/lca/class_marital.rds")
    else readRDS("data/lca/class_mar_panhandle.rds")
  })
}
shinyApp(ui, server)