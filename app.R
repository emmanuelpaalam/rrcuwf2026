library(shiny)
library(ggplot2)
library(bslib)

# Helper: friendly placeholder plot for metrics not yet computed
# (Kept in case you need it for future empty tabs!)
placeholder_plot <- function(msg = "Data not yet available for this cohort.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, color = "grey40") +
    theme_void()
}

# UWF brand colors (Argonaut Green & Blue)
uwf_green <- "#00543C"
uwf_blue  <- "#041E42"

# Placeholder team roster -- replace with real names/roles/affiliations.
team_members <- list(
  list(name = "Team Member Name", role = "Principal Investigator", affiliation = "University of West Florida"),
  list(name = "Team Member Name", role = "Co-Investigator", affiliation = "University of West Florida"),
  list(name = "Team Member Name", role = "Data Scientist", affiliation = "University of West Florida"),
  list(name = "Team Member Name", role = "Clinical Collaborator", affiliation = "University of West Florida"),
  list(name = "Team Member Name", role = "Graduate Research Assistant", affiliation = "University of West Florida"),
  list(name = "Team Member Name", role = "Graduate Research Assistant", affiliation = "University of West Florida")
)

team_card <- function(member) {
  card(
    class = "text-center",
    card_body(
      div(style = paste0("width:72px; height:72px; border-radius:50%; margin:0 auto 10px auto;",
                         "background:", uwf_green, "; color:white; display:flex; align-items:center;",
                         "justify-content:center; font-size:1.5rem; font-weight:600;"),
          toupper(substr(member$name, 1, 1))
      ),
      h5(member$name),
      p(strong(member$role)),
      p(class = "text-muted", member$affiliation)
    )
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5, primary = uwf_blue, success = uwf_green) |>
    bs_add_rules(sprintf("
      .nav-pills .nav-link.active { background-color: %s !important; }
      .nav-pills .nav-link { color: %s; }
      .title-panel { color: %s; font-weight: 700; }
      hr { border-top: 2px solid %s; }
    ", uwf_green, uwf_blue, uwf_blue, uwf_green)),
  titlePanel(
    div(class = "title-panel", "MetaSense AI")
  ),
  mainPanel(
    width = 12,
    
    tabsetPanel(
      type = "pills",
      
      # ---------------------------------------------------------------
      # TAB 1: ABOUT THE PROJECT
      # ---------------------------------------------------------------
      tabPanel("About the Project",
               br(),
               h3("MetaSense: AI-Enabled Remote Health Monitoring"),
               p("MetaSense is an AI-enabled remote health monitoring platform that detects early metabolic and behavioral risk signals using wearable devices and clinical data."),
               p("Using the NIH All of Us Research Program, our project will analyze multimodal data from individuals with Metabolic Syndrome (MetS) and evaluate the association between metabolic dysfunction and chronic disease burden, including outcomes related to osteoarthritis (OA) and multiple cancers."),
               p("Data sources will include Fitbit wearable metrics, electronic health records (EHRs), treatment and medication data, and patient-reported outcomes."),
               hr(),
               h4("Data Sources"),
               tags$ul(
                 tags$li(strong("Wearable Metrics:"), " Fitbit-derived activity, heart rate, and sleep data"),
                 tags$li(strong("Electronic Health Records:"), " Diagnoses, labs, and vital signs"),
                 tags$li(strong("Treatment & Medication Data:"), " Prescriptions and treatment history"),
                 tags$li(strong("Patient-Reported Outcomes:"), " Fatigue, pain, and quality-of-life measures")
               ),
               h4("Cohorts"),
               p("Throughout this dashboard, results are shown for two cohorts: the ", strong("Full National Cohort"),
                 " of All of Us participants meeting study criteria, and a geographically restricted ",
                 strong("Northwest Florida Cohort"), " (ZIP codes 323**, 324**, 325**), to examine regional patterns relevant to our research region."),
               h4("How to Use This Dashboard"),
               tags$ul(
                 tags$li(strong("Descriptive Data:"), " Explore sociodemographic, clinical, and behavioral distributions for the cohort."),
                 tags$li(strong("LCA Subgroups:"), " View latent class subgroups defined by mental-metabolic burden."),
                 tags$li(strong("ML Model Input:"), " Enter patient-level data to preview inputs for the risk-prediction model (currently a mockup; not yet connected to a trained model).")
               )
      ),
      
      # ---------------------------------------------------------------
      # TAB 2: TEAM MEMBERS
      # ---------------------------------------------------------------
      tabPanel("Team",
               br(),
               h3("Project Team"),
               p("Meet the researchers and collaborators behind MetaSense.", em(" (Placeholder roster \u2014 update with real names, roles, and affiliations.)")),
               br(),
               fluidRow(
                 lapply(team_members, function(m) column(4, team_card(m), br()))
               )
      ),
      
      # ---------------------------------------------------------------
      # TAB 3: DESCRIPTIVE DATA
      # ---------------------------------------------------------------
      tabPanel("Descriptive Data",
               br(),
               h3("Metabolic Syndrome Indicators"),
               p("Descriptive statistics and distributions of key sociodemographic and clinical variables for the targeted project cohort: participants within the NIH All of Us Research Program who present with both Metabolic Syndrome (MetS) and Head and Neck Cancer (HNC)."),
               
               wellPanel(
                 radioButtons("desc_region_toggle",
                              "Select Cohort:",
                              choices = c("Full National Cohort", "Northwest Florida Cohort"),
                              selected = "Full National Cohort",
                              inline = TRUE)
               ),
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
      
      # ---------------------------------------------------------------
      # TAB 4: LCA SUBGROUPS
      # ---------------------------------------------------------------
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
               p("Choose between descriptive analysis of the LCA groups for the general MetS population or those specifically located in Northwest Florida (ZIP codes 323**, 324**, 325**)."),
               wellPanel(
                 radioButtons("region_toggle",
                              "Select Cohort:",
                              choices = c("Full National Cohort", "Northwest Florida Cohort"),
                              selected = "Full National Cohort",
                              inline = TRUE)
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
      ),
      
      # ---------------------------------------------------------------
      # TAB 5: ML MODEL INPUT (mockup - not yet connected to a trained model)
      # ---------------------------------------------------------------
      tabPanel("ML Model Input",
               br(),
               h3("Metabolic-Cancer Risk Model: Input Data"),
               p("Enter patient-level data below to preview the inputs used by the MetaSense risk-prediction model. ",
                 strong("This form is a mockup:"),
                 " it is not yet connected to a trained model. Once the model is finalized, submitting this form will return a predicted chronic disease risk score."),
               hr(),
               
               fluidRow(
                 column(6,
                        h4("Demographics & Cohort"),
                        selectInput("ml_cohort", "Cohort", choices = c("Full National Cohort", "Northwest Florida Cohort")),
                        numericInput("ml_age", "Age (years)", value = 55, min = 18, max = 100),
                        selectInput("ml_sex", "Sex", choices = c("Female", "Male", "Other/Unknown")),
                        selectInput("ml_race", "Race/Ethnicity", choices = c("White", "Black/African American", "Hispanic/Latino", "Asian", "Other/Multiple")),
                        numericInput("ml_bmi", "BMI (kg/m\u00b2)", value = 30, min = 10, max = 80, step = 0.1)
                 ),
                 column(6,
                        h4("Fitbit Wearable Metrics"),
                        numericInput("ml_steps", "Avg. Daily Steps", value = 6000, min = 0, max = 40000, step = 100),
                        numericInput("ml_rhr", "Resting Heart Rate (bpm)", value = 72, min = 30, max = 150),
                        numericInput("ml_sleep", "Avg. Sleep Duration (hours/night)", value = 6.5, min = 0, max = 14, step = 0.1),
                        numericInput("ml_active_min", "Avg. Active Minutes/Day", value = 20, min = 0, max = 300)
                 )
               ),
               
               fluidRow(
                 column(6,
                        h4("EHR / Clinical Labs"),
                        numericInput("ml_sbp", "Systolic Blood Pressure (mmHg)", value = 130, min = 70, max = 240),
                        numericInput("ml_dbp", "Diastolic Blood Pressure (mmHg)", value = 85, min = 40, max = 150),
                        numericInput("ml_glucose", "Fasting Glucose (mg/dL)", value = 100, min = 40, max = 400),
                        numericInput("ml_trig", "Triglycerides (mg/dL)", value = 150, min = 20, max = 1000),
                        numericInput("ml_hdl", "HDL Cholesterol (mg/dL)", value = 45, min = 10, max = 150)
                 ),
                 column(6,
                        h4("Treatment, Medications & Patient-Reported Outcomes"),
                        checkboxGroupInput("ml_meds", "Current Medications",
                                           choices = c("Antihypertensive", "Statin", "Metformin/Diabetes Medication", "Insulin", "NSAID/Pain Medication")),
                        selectInput("ml_oa", "Osteoarthritis Diagnosis", choices = c("No", "Yes")),
                        selectInput("ml_cancer_hx", "Cancer History", choices = c("None", "Head & Neck", "Other", "Multiple")),
                        sliderInput("ml_fatigue", "Patient-Reported Fatigue (0 = none, 10 = severe)", min = 0, max = 10, value = 4),
                        sliderInput("ml_pain", "Patient-Reported Pain (0 = none, 10 = severe)", min = 0, max = 10, value = 3)
                 )
               ),
               
               br(),
               actionButton("ml_predict", "Preview Model Input", class = "btn-primary"),
               br(), br(),
               uiOutput("ml_output")
      )
    )
  )
)


server <- function(input, output, session) {
  
  # ---------------------------------------------------------------
  # Descriptive Data (region-aware; dynamically pulls _fl.rds)
  # ---------------------------------------------------------------
  is_national_desc <- reactive(input$desc_region_toggle == "Full National Cohort")
  
  render_desc_plot <- function(path) {
    renderPlot({
      if (is_national_desc()) {
        readRDS(path)
      } else {
        # Automatically change the path to load the _fl version of the RDS file
        fl_path <- sub("\\.rds$", "_fl.rds", path)
        readRDS(fl_path)
      }
    })
  }
  
  output$dem1 <- render_desc_plot("data/dem_birthsex.rds")
  output$dem2 <- render_desc_plot("data/dem_raceeth.rds")
  output$dem3 <- render_desc_plot("data/dem_age.rds")
  output$mets_prev <- render_desc_plot("data/metsprev.rds")
  output$metcount <- render_desc_plot("data/metcount.rds")
  output$impacts <- render_desc_plot("data/fatiguemet.rds")
  output$smoking <- render_desc_plot("data/smokrisk.rds")
  output$activity <- render_desc_plot("data/actlevels.rds")
  
  # ---------------------------------------------------------------
  # LCA Subgroups
  # ---------------------------------------------------------------
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
  
  is_national_lca <- reactive(input$region_toggle == "Full National Cohort")
  
  output$geographic_distribution <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_dist_full.rds")
    else readRDS("data/lca/class_dist_fl.rds")
  })
  
  output$age_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_age.rds")
    else readRDS("data/lca/class_age_fl.rds")
  })
  
  output$sex_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_sex.rds")
    else readRDS("data/lca/class_sex_fl.rds")
  })
  
  output$race_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_race.rds")
    else readRDS("data/lca/class_race_fl.rds")
  })
  
  output$education_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_education.rds")
    else readRDS("data/lca/class_edu_fl.rds")
  })
  
  output$marital_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_marital.rds")
    else readRDS("data/lca/class_mar_fl.rds")
  })
  
  # ---------------------------------------------------------------
  # ML Model Input (mockup)
  # ---------------------------------------------------------------
  observeEvent(input$ml_predict, {
    output$ml_output <- renderUI({
      tagList(
        div(class = "alert alert-info",
            strong("Model not yet connected."),
            " Below is the input vector that would be passed to the MetaSense risk-prediction model."
        ),
        tags$table(class = "table table-striped table-sm",
                   tags$tbody(
                     tags$tr(tags$td("Cohort"), tags$td(input$ml_cohort)),
                     tags$tr(tags$td("Age"), tags$td(input$ml_age)),
                     tags$tr(tags$td("Sex"), tags$td(input$ml_sex)),
                     tags$tr(tags$td("Race/Ethnicity"), tags$td(input$ml_race)),
                     tags$tr(tags$td("BMI"), tags$td(input$ml_bmi)),
                     tags$tr(tags$td("Avg. Daily Steps"), tags$td(input$ml_steps)),
                     tags$tr(tags$td("Resting Heart Rate"), tags$td(input$ml_rhr)),
                     tags$tr(tags$td("Avg. Sleep Duration (hrs)"), tags$td(input$ml_sleep)),
                     tags$tr(tags$td("Avg. Active Minutes/Day"), tags$td(input$ml_active_min)),
                     tags$tr(tags$td("Systolic BP"), tags$td(input$ml_sbp)),
                     tags$tr(tags$td("Diastolic BP"), tags$td(input$ml_dbp)),
                     tags$tr(tags$td("Fasting Glucose"), tags$td(input$ml_glucose)),
                     tags$tr(tags$td("Triglycerides"), tags$td(input$ml_trig)),
                     tags$tr(tags$td("HDL Cholesterol"), tags$td(input$ml_hdl)),
                     tags$tr(tags$td("Current Medications"), tags$td(paste(input$ml_meds, collapse = ", "))),
                     tags$tr(tags$td("Osteoarthritis Diagnosis"), tags$td(input$ml_oa)),
                     tags$tr(tags$td("Cancer History"), tags$td(input$ml_cancer_hx)),
                     tags$tr(tags$td("Patient-Reported Fatigue"), tags$td(input$ml_fatigue)),
                     tags$tr(tags$td("Patient-Reported Pain"), tags$td(input$ml_pain))
                   )
        )
      )
    })
  })
}
shinyApp(ui, server)