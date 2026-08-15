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
      # ABOUT
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
                 tags$li(strong("Descriptive Data:"), " Explore sociodemographic, clinical, and behavioral distributions for both cohorts."),
                 tags$li(strong("LCA Subgroups:"), " View latent class subgroups defined by mental-metabolic burden, with descriptive data per class."),
                 tags$li(strong("ML Model Input:"), " Enter patient-level data to identify individual likelihood for falling into one of the identified risk groups (currently a mockup; not yet connected to a trained model).")
               )
      ),
      
      # ---------------------------------------------------------------
      # DESCRIPTIVE DATA
      # ---------------------------------------------------------------
      tabPanel("Descriptive Data",
               br(),
               h3("Metabolic Syndrome & Cohort Indicators"),
               p("Explore the clinical, behavioral, and demographic breakdown of our study population. Both the National and Northwest Florida cohorts are displayed side-by-side for comparison."),
               
               # Dropdown selector for the plot
               wellPanel(
                 selectInput("desc_plot_choice", 
                             "Select a Clinical or Demographic Metric to Visualize:",
                             choices = c(
                               "Demographics: Sex at Birth" = "dem_sex",
                               "Demographics: Race/Ethnicity" = "dem_race",
                               "Demographics: Age Distribution" = "dem_age",
                               "Clinical: MetS Symptom Prevalence" = "mets_prev",
                               "Clinical: Metabolic Component Count" = "metcount",
                               "Patient-Reported: Fatigue Severity" = "impacts",
                               "Behavioral: Lifetime Smoking Risk" = "smoking",
                               "Behavioral: Everyday Activity Levels" = "activity"
                             ), 
                             width = "100%")
               ),
               
               # Dynamic caption and plot
               fluidRow(
                 column(12, 
                        uiOutput("desc_plot_caption"),
                        br(),
                        plotOutput("desc_main_plot", height = "500px")
                 )
               )
      ),
      
      # ---------------------------------------------------------------
      # LCA SUBGROUPS
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
               
               h3("Sociodemographic & Geographic Breakdown"),
               p("Explore the distribution and demographic makeup of the Latent Classes, comparing the National cohort alongside the Northwest Florida region."),
               
               wellPanel(
                 selectInput("lca_plot_choice", 
                             "Select an LCA Breakdown to Visualize:",
                             choices = c(
                               "Class Distribution Overview" = "dist",
                               "Demographics: Age Distribution" = "age",
                               "Demographics: Sex at Birth" = "sex",
                               "Demographics: Race/Ethnicity" = "race",
                               "Demographics: Educational Attainment" = "edu",
                               "Demographics: Marital Status" = "marital"
                             ), 
                             width = "100%")
               ),
               
               fluidRow(
                 column(12, 
                        uiOutput("lca_plot_caption"),
                        br(),
                        plotOutput("lca_main_plot", height = "500px")
                 )
               )
      ),
      
      # ---------------------------------------------------------------
      # ML MODEL
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
      ),
      
      # ---------------------------------------------------------------
      # TEAM MEMBERS
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
    )
  )
)


server <- function(input, output, session) {
  
  # ---------------------------------------------------------------
  # Descriptive Data Logic
  # ---------------------------------------------------------------
  
  # Render the single dynamic plot based on user selection
  output$desc_main_plot <- renderPlot({
    plot_file <- switch(input$desc_plot_choice,
                        "dem_sex" = "data/dem_birthsex.rds",
                        "dem_race" = "data/dem_raceeth.rds",
                        "dem_age" = "data/dem_age.rds",
                        "mets_prev" = "data/metsprev.rds",
                        "metcount" = "data/metcount.rds",
                        "impacts" = "data/fatiguemet.rds",
                        "smoking" = "data/smokrisk.rds",
                        "activity" = "data/actlevels.rds")
    readRDS(plot_file)
  })
  
  # Render the dynamic clinical caption
  output$desc_plot_caption <- renderUI({
    caption_text <- switch(input$desc_plot_choice,
                           "dem_sex" = "This chart shows the proportion of male versus female participants. Comparing the national and regional cohorts helps us understand if our local Northwest Florida population mirrors broader national trends in this disease space.",
                           "dem_race" = "Here we see the racial and ethnic breakdown of the cohorts. Understanding these proportions is vital for ensuring our predictive models perform equitably and accurately across different patient populations.",
                           "dem_age" = "This curve illustrates the age distribution. A wider curve means ages are more spread out, while a taller peak indicates a high concentration of patients around a specific age group.",
                           "mets_prev" = "This plot breaks down the individual components of Metabolic Syndrome (like high blood pressure or diabetes). It separates patients by whether or not they have Head and Neck Cancer (HNC) to reveal potential overlapping clinical risks.",
                           "metcount" = "Patients can have between 0 and 5 Metabolic Syndrome components. This visualization shows the density of the metabolic burden across the population, divided by head and neck cancer (HNC) status.",
                           "impacts" = "Fatigue is a major patient-reported outcome. This stacked chart explores whether an increasing number of concurrent metabolic conditions correlates with more severe self-reported fatigue.",
                           "smoking" = "Smoking is a primary behavioral risk factor for Head and Neck Cancer. This chart shows the lifetime smoking history of patients, grouped by their head and neck cancer (HNC) status.",
                           "activity" = "Physical activity strongly impacts metabolic health. Here we compare how much everyday activity patients report, divided by their head and neck cancer (HNC) status.")
    
    # Wrap it in a nice alert box matching the UWF theme
    div(class = "alert alert-secondary",
        style = "border-left: 4px solid #00543C; background-color: #f8f9fa; color: #041E42;",
        strong("Clinical Context: "), caption_text)
  })
  
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
  
  # Render the single dynamic plot for LCA based on user selection
  output$lca_main_plot <- renderPlot({
    lca_file <- switch(input$lca_plot_choice,
                       "dist" = "data/lca/class_dist.rds",
                       "age" = "data/lca/class_age.rds",
                       "sex" = "data/lca/class_sex.rds",
                       "race" = "data/lca/class_race.rds",
                       "edu" = "data/lca/class_education.rds",
                       "marital" = "data/lca/class_marital.rds")
    readRDS(lca_file)
  })
  
  # Render the dynamic clinical caption for LCA
  output$lca_plot_caption <- renderUI({
    caption_text <- switch(input$lca_plot_choice,
                           "dist" = "This plot compares the proportion of patients assigned to each latent mental-metabolic class between the National and Northwest Florida cohorts, highlighting potential geographic differences in disease profiles.",
                           "age" = "Age distributions within each latent class. Identifying whether severe classes skew younger or older helps tailor age-appropriate early interventions.",
                           "sex" = "Sex breakdown across the classes. This highlights whether specific mental-metabolic risk profiles are more prevalent in male or female patient populations.",
                           "race" = "Racial and ethnic distribution per class. Observing disparities here is critical for recognizing populations disproportionately affected by combined mental and metabolic burdens.",
                           "edu" = "Educational attainment across the classes. This acts as a proxy for socioeconomic status, revealing if specific risk profiles are associated with social determinants of health.",
                           "marital" = "Marital status breakdown. Social support systems, or lack thereof, can significantly impact a patient's mental-metabolic profile and adherence to lifestyle interventions.")
    
    div(class = "alert alert-secondary",
        style = "border-left: 4px solid #00543C; background-color: #f8f9fa; color: #041E42;",
        strong("Clinical Context: "), caption_text)
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