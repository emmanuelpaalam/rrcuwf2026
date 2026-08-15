library(shiny)
library(ggplot2)
library(bslib)

# ---------------------------------------------------------------------
# THEME
# A calm, clinical palette suited for a healthcare-leadership audience:
# navy for authority/trust, a muted UWF green as the accent, generous
# white space, and card-based sections instead of dense blocks of text.
# ---------------------------------------------------------------------
navy      <- "#0B3D62"   # primary - deep clinical blue
uwf_green <- "#146C43"   # accent - muted UWF green
bg_grey   <- "#F5F7F8"   # page background
ink       <- "#22303F"   # body text

app_theme <- bs_theme(
  version = 5,
  primary = navy,
  success = uwf_green,
  bg = "#FFFFFF",
  fg = ink,
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  "navbar-bg" = navy,
  "border-radius" = "0.6rem",
  "card-border-color" = "#E3E8EC"
) |>
  bs_add_rules(sprintf("
    body { background-color: %s; }
    .navbar-brand { font-weight: 700; letter-spacing: 0.02em; }
    .card { box-shadow: 0 1px 3px rgba(0,0,0,0.06); border: 1px solid #E3E8EC; margin-bottom: 1.25rem; }
    .card-header { background-color: #FBFCFC; font-weight: 600; color: %s; border-bottom: 1px solid #E3E8EC; }
    .section-lede { color: #5A6B7A; max-width: 900px; }
    .accent-rule { border: none; border-top: 3px solid %s; width: 64px; margin: 0.25rem 0 1.25rem 0; }
    .cohort-toggle .form-check-label { font-weight: 500; }
    .btn-primary { background-color: %s; border-color: %s; }
    .btn-primary:hover { background-color: #082C48; border-color: #082C48; }
    footer.app-footer { color: #8A97A3; font-size: 0.85rem; padding: 2rem 0 1rem 0; text-align: center; }
  ", bg_grey, navy, uwf_green, navy, navy))

# ---------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------

# Friendly placeholder for metrics not yet computed for a given cohort
# (e.g. Northwest Florida descriptive stats not yet generated).
placeholder_plot <- function(msg = "Data not yet available for this cohort.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, color = "grey45") +
    theme_void()
}

section_header <- function(title, lede = NULL) {
  tagList(
    h3(title),
    if (!is.null(lede)) p(class = "section-lede", lede),
    tags$hr(class = "accent-rule")
  )
}

cohort_toggle <- function(input_id) {
  div(class = "cohort-toggle",
      radioButtons(input_id, "Cohort",
                   choices = c("Full National Cohort", "Northwest Florida Cohort"),
                   selected = "Full National Cohort",
                   inline = TRUE)
  )
}

lca_classes <- list(
  list(pct = "24.8%", title = "Diabetes, Hypertension & Triglyceride Burden", color = navy),
  list(pct = "11.2%", title = "Triglyceride-Dominant", color = uwf_green),
  list(pct = "27.6%", title = "Hypertension & Triglyceride Burden", color = navy),
  list(pct = "6.5%",  title = "Obesity-Dominant with Mental Health Burden", color = uwf_green),
  list(pct = "29.9%", title = "High Combined Mental-Metabolic Burden", color = navy)
)

lca_value_box <- function(cls) {
  value_box(
    title = cls$title,
    value = cls$pct,
    theme = value_box_theme(bg = cls$color, fg = "white"),
    showcase = NULL,
    full_screen = FALSE
  )
}

team_members <- list(
  list(name = "Achraf Cohen, PhD", role = "Statistics and AI", affiliation = "Machine learning, AI, statistical modeling", email = "acohen@uwf.edu", pic = "team/Cohen.png"),
  list(name = "Karishma Chhabria Unrue, PhD", role = "Public Health", affiliation = "Cancer, metabolic burden, mental health", email = "kchhabria@uwf.edu", pic = "team/Chabbria.png"),
  list(name = "Armaghan Mahmoudian, PhD", role = "Movement Sciences and Health", affiliation = "Osteoarthritis, physical therapy", email = "amahmoudian@uwf.edu", pic = "team/Mahmoudian.png"),
  list(name = "Shrishti Sharma", role = "Statistics and Data Science (Student)", affiliation = "Statistical modeling, machine learning", email = "fs56@students.uwf.edu", pic = "team/shrishti.png"),
  list(name = "Emmanuel Paalam", role = "Computer Science and Data Science (Student)", affiliation = "Dashboard development, data engineering", email = "ejp25@students.uwf.edu", pic = "team/emmanuel.png")
)

community_advisors <- list(
  list(name = "Licheng \u201cTony\u201d Lee, M.D., FACC", role = "Community Advisor", affiliation = "Baptist Health", email = "licheng.lee@bhcpns.org")
)

team_card <- function(member) {
 avatar <- if (!is.null(member$pic)) {
    tags$img(src = member$pic, style = "width:120px; height:120px; border-radius:50%; object-fit:cover; margin:0 auto 12px auto; display:block;")
  } else {
    div(style = paste0("width:120px; height:120px; border-radius:50%; margin:0 auto 12px auto;",
                        "background:", navy, "; color:white; display:flex; align-items:center;",
                        "justify-content:center; font-size:2rem; font-weight:600;"),
        toupper(substr(member$name, 1, 1)))
  }
  card(
    class = "text-center",
    card_body(
      avatar,
      h5(member$name),
      p(class = "mb-0", strong(member$role)),
      p(class = "text-muted", member$affiliation),
      if (!is.null(member$email)) p(class = "small", tags$a(href = paste0("mailto:", member$email), member$email))
    )
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_navbar(
  title = "MetaSense AI",
  theme = app_theme,
  fillable = FALSE,
  bg = navy,

  # -----------------------------------------------------------------
  # ABOUT THE PROJECT
  # -----------------------------------------------------------------
  nav_panel("About",
    div(class = "container-fluid py-4",
      section_header(
        "MetaSense: Wearable Intelligence for Chronic Disease Risk Monitoring",
       paste("Discovering mental-metabolic chronic disease risk groups and predicting individual",
          "group membership using wearable-derived physical activity, demographics, and uncertainty-aware AI."
    )
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Project Summary"),
          card_body(
            p(
              "MetaSense is a precision-health research platform designed to identify clinically meaningful ",
              "mental-metabolic chronic disease risk groups and determine whether those groups can be predicted ",
              "using routinely available wearable and demographic information."
            ),
            p(
              "First, Latent Class Analysis (LCA) will identify subgroups based on metabolic syndrome components, ",
              "mental health burden, and chronic disease status, initially focusing on osteoarthritis and cancer. ",
              "The resulting classes represent distinct combinations of mental, metabolic, and chronic disease burden."
            ),
            p(
              "Next, machine-learning models will use Fitbit-derived physical activity measures and demographic ",
              "characteristics to predict an individual's LCA-defined risk group. Uncertainty quantification will ",
              "accompany each prediction so MetaSense can distinguish confident predictions from cases requiring ",
              "additional clinical assessment."
            )
          )
        ),
        card(
          card_header("Cohorts"),
         card_body(
            p(strong("Full National Cohort"), " — All of Us participants meeting study criteria."),
            p(strong("Northwest Florida Cohort"), " — Participants in ZIP codes 324** and 325** for regional comparisons."),
            p(
              class = "text-muted small",
              "National data support model development; the Northwest Florida subset supports local relevance and community translation."
            )
          )
        )
      ),

      card(
        card_header("How MetaSense Works"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            card(class = "workflow-step", card_body(h5("1. Discover"), p("Use LCA to identify subgroups defined by metabolic syndrome, mental health, and chronic disease burden."))),
            card(class = "workflow-step", card_body(h5("2. Predict"), p("Use wearable physical activity and demographics to predict LCA-defined risk-group membership."))),
            card(class = "workflow-step", card_body(h5("3. Quantify Confidence"), p("Use uncertainty quantification to distinguish reliable predictions from uncertain classifications."))),
            card(class = "workflow-step", card_body(h5("4. Stratify Risk"), p("Translate predictions into interpretable information for prevention and community-health decision support.")))
          )
        )
      ),
    
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Data & Modeling Framework"),
          card_body(
            tags$ul(
              tags$li(strong("LCA Class-Defining Data: "), "Metabolic syndrome components, depression/anxiety indicators, and chronic disease status including osteoarthritis and cancer."),
              tags$li(strong("Wearable Predictors: "), "Fitbit-derived daily steps, active minutes, sedentary behavior, activity variability, and other available physical-activity measures."),
              tags$li(strong("Demographic Predictors: "), "Age, sex, race/ethnicity, socioeconomic, and other available demographic characteristics."),
              tags$li(strong("Primary Data Source: "), "NIH All of Us Research Program EHR, survey, demographic, and Fitbit data.")
            )
          )
        ),
        card(
          card_header("Dashboard Guide"),
          card_body(
            tags$ul(
              tags$li(strong("Descriptive Data: "), "Characterize metabolic, mental-health, chronic disease, demographic, and behavioral burden."),
              tags$li(strong("LCA Risk Groups: "), "Inspect the clinically derived mental-metabolic chronic disease subgroups."),
              tags$li(strong("Risk Prediction: "), "Predict LCA-defined group membership from wearable physical activity and demographics."),
              tags$li(strong("Uncertainty: "), "Report model confidence so users can distinguish reliable predictions from uncertain cases."),
              tags$li(strong("Team: "), "View interdisciplinary contributors and the community advisor." )
            )
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # DESCRIPTIVE DATA
  # -----------------------------------------------------------------
  nav_panel("Descriptive Data",
    div(class = "container-fluid py-4",
      section_header(
        "Metabolic Syndrome Indicators",
        "Sociodemographic and clinical variables for the cohort: participants presenting with both Metabolic Syndrome (MetS) and Head and Neck Cancer (HNC)."
      ),
      card(card_body(cohort_toggle("desc_region_toggle"))),

      card(card_header("Demographics"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("dem1"), plotOutput("dem2")),
          plotOutput("dem3")
        )
      ),
      card(card_header("MetS Symptoms Prevalence by Subject Group"),
        card_body(plotOutput("mets_prev"))
      ),
      card(card_header("Metabolic Count Distribution"),
        card_body(plotOutput("metcount"))
      ),
      card(card_header("Physical / Mental Impact of MetS"),
        card_body(plotOutput("impacts"))
      ),
      card(card_header("Behavioral Data"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("smoking"), plotOutput("activity"))
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # LCA SUBGROUPS
  # -----------------------------------------------------------------
  nav_panel("LCA Subgroups",
    div(class = "container-fluid py-4",
      section_header(
        "Latent Class Analysis of Mental-Metabolic Profiles",
        "Distinct latent subgroups based on depression, anxiety, and individual metabolic syndrome components."
      ),

      card(card_header("Five Mental-Metabolic Profiles Identified"),
        card_body(
          layout_columns(col_widths = c(12, 6, 6, 6, 6), !!!lapply(lca_classes, lca_value_box))
        )
      ),

      card(card_header("Model Diagnostics"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(h5("Conditional Item-Response Probabilities", class = "text-center"),
                imageOutput("bubble_chart", height = "auto")),
            div(h5("Multiple Correspondence Analysis", class = "text-center"),
                imageOutput("mca_plot", height = "auto"))
          )
        )
      ),

      card(
        card_header("Sociodemographic & Geographic Breakdown"),
        card_body(
          p(class = "section-lede", "Compare LCA subgroup composition between the general MetS population and those located in Northwest Florida (ZIP codes 324**, 325**)."),
          cohort_toggle("region_toggle"),
          plotOutput("geographic_distribution")
        )
      ),

      card(card_header("Class Composition by Demographic"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("age_plot"), plotOutput("sex_plot")),
          layout_columns(col_widths = c(6, 6), plotOutput("race_plot"), plotOutput("education_plot")),
          plotOutput("marital_plot")
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # ML MODEL INPUT (mockup)
  # -----------------------------------------------------------------
  nav_panel("ML Model Input",
    div(class = "container-fluid py-4",
      section_header(
        "Metabolic-Cancer Risk Model: Input Data",
        "Preview the inputs used by the MetaSense risk-prediction model. This form is a mockup and is not yet connected to a trained model."
      ),

      layout_sidebar(
        sidebar = sidebar(
          width = 380,
          title = "Patient Inputs",
          open = "always",

          h6("Demographics & Cohort"),
          selectInput("ml_cohort", "Cohort", choices = c("Full National Cohort", "Northwest Florida Cohort")),
          numericInput("ml_age", "Age (years)", value = 55, min = 18, max = 100),
          selectInput("ml_sex", "Sex", choices = c("Female", "Male", "Other/Unknown")),
          selectInput("ml_race", "Race/Ethnicity", choices = c("White", "Black/African American", "Hispanic/Latino", "Asian", "Other/Multiple")),
          selectInput("ml_marital", "Marital Status", choices = c("Married", "Single", "Divorced", "Widowed", "Separated", "Other/Unknown")),

          tags$hr(),
          h6("Fitbit Wearable Metrics"),
          numericInput("ml_steps", "Avg. Daily Steps", value = 6000, min = 0, max = 40000, step = 100),
          numericInput("ml_rhr", "Resting Heart Rate (bpm)", value = 72, min = 30, max = 150),
          numericInput("ml_sleep", "Avg. Sleep Duration (hrs/night)", value = 6.5, min = 0, max = 14, step = 0.1),
          numericInput("ml_active_min", "Avg. Active Minutes/Day", value = 20, min = 0, max = 300),

          tags$hr(),
          h6("EHR / Clinical Labs"),
          numericInput("ml_sbp", "Systolic BP (mmHg)", value = 130, min = 70, max = 240),
          numericInput("ml_dbp", "Diastolic BP (mmHg)", value = 85, min = 40, max = 150),
          numericInput("ml_glucose", "Fasting Glucose (mg/dL)", value = 100, min = 40, max = 400),
          numericInput("ml_trig", "Triglycerides (mg/dL)", value = 150, min = 20, max = 1000),
          numericInput("ml_hdl", "HDL Cholesterol (mg/dL)", value = 45, min = 10, max = 150),

          tags$hr(),
          h6("Treatment, Medications & PROs"),
          checkboxGroupInput("ml_meds", "Current Medications",
                              choices = c("Antihypertensive", "Statin", "Metformin/Diabetes Medication", "Insulin", "NSAID/Pain Medication")),
          selectInput("ml_oa", "Osteoarthritis Diagnosis", choices = c("No", "Yes")),
          selectInput("ml_cancer_hx", "Cancer History", choices = c("None", "Head & Neck", "Other", "Multiple")),
          sliderInput("ml_fatigue", "Patient-Reported Fatigue (0\u201310)", min = 0, max = 10, value = 4),
          sliderInput("ml_pain", "Patient-Reported Pain (0\u201310)", min = 0, max = 10, value = 3),

          tags$hr(),
          actionButton("ml_predict", "Preview Model Input", class = "btn-primary w-100")
        ),

        card(
          card_header("Model Input Preview"),
          card_body(
            uiOutput("ml_output")
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # TEAM
  # -----------------------------------------------------------------
  nav_panel("Team",
    div(class = "container-fluid py-4",
      section_header("Project Team", "Researchers and collaborators behind MetaSense at the University of West Florida."),
      layout_columns(col_widths = c(4, 4, 4), !!!lapply(team_members, team_card)),
      br(),
      h4("Community Advisor"),
      tags$hr(class = "accent-rule"),
      layout_columns(col_widths = c(4), !!!lapply(community_advisors, team_card))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text text-white-50 small", "University of West Florida"))
)

ui <- tagList(
  ui,
  tags$footer(class = "app-footer", "MetaSense AI \u00b7 University of West Florida")
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # -------------------------------------------------------------
  # Descriptive Data (region-aware; Northwest Florida versions
  # of these plots are not yet available)
  # -------------------------------------------------------------
  is_national_desc <- reactive(input$desc_region_toggle == "Full National Cohort")

  render_desc_plot <- function(path) {
    renderPlot({
      if (is_national_desc()) {
        readRDS(path)
      } else {
        placeholder_plot("Northwest Florida data for this metric is not yet available.")
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

  # -------------------------------------------------------------
  # LCA Subgroups
  # -------------------------------------------------------------
  # Note: renderImage is used for non-RDS image files like PNG/TIFF
  output$bubble_chart <- renderImage({
    list(
      src = "data/lca/Classes_5_class_full_cohort.png",
      width = "100%",
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
    else readRDS("data/lca/class_dist_panhandle.rds")
  })

  output$age_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_age.rds")
    else readRDS("data/lca/class_age_panhandle.rds")
  })

  output$sex_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_sex.rds")
    else readRDS("data/lca/class_sex_panhandle.rds")
  })

  output$race_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_race.rds")
    else readRDS("data/lca/class_race_panhandle.rds")
  })

  output$education_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_education.rds")
    else readRDS("data/lca/class_edu_panhandle.rds")
  })

  output$marital_plot <- renderPlot({
    if (is_national_lca()) readRDS("data/lca/class_marital.rds")
    else readRDS("data/lca/class_mar_panhandle.rds")
  })

  # -------------------------------------------------------------
  # ML Model Input (mockup)
  # -------------------------------------------------------------
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
            tags$tr(tags$td("Marital Status"), tags$td(input$ml_marital)),
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
