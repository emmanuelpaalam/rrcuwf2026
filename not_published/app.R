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

# Five LCA phenotypes from technical report (Table 3)
lca_classes <- list(
  list(pct = "33.2%", title = "High Multidomain Burden", color = navy,
       desc = "Broadest concurrent burden: mental health (72%), obesity (72%), hypertension (95%), diabetes (60%), dyslipidemia (45%), OA (85%), cancer (25%)"),
  list(pct = "21.9%", title = "Hypertension-Dominant", color = uwf_green,
       desc = "Hypertension (100%), moderate OA (35%), low metabolic/mental burden otherwise"),
  list(pct = "19.6%", title = "Obesity-Dominant", color = navy,
       desc = "Obesity (100%), elevated mental health burden (54%), moderate OA (30%)"),
  list(pct = "14.9%", title = "Diabetes-Dominant", color = uwf_green,
       desc = "Diabetes (100%), hypertension (71%), moderate mental health burden (34%)"),
  list(pct = "10.5%", title = "Dyslipidemia/OA-Dominant", color = navy,
       desc = "Dyslipidemia (100%), OA (53%), hypertension (54%), cancer (21%)")
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
        card_header("MetaSense Framework: Discover \u2192 Prioritize \u2192 Predict \u2192 Trust + Translate"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            card(class = "workflow-step", card_body(h5("1. Discover"), p("LCA identifies five mental-metabolic chronic disease phenotypes from 218,619 participants using clinical indicators."))),
            card(class = "workflow-step", card_body(h5("2. Prioritize"), p("The High Multidomain Burden phenotype (33.2%) is selected as the clinically actionable prediction target."))),
            card(class = "workflow-step", card_body(h5("3. Predict"), p("XGBoost uses wearable activity and demographics to estimate probability of High Multidomain Burden membership (AUC = 0.691)."))),
            card(class = "workflow-step", card_body(h5("4. Trust + Translate"), p("Calibration, conformal prediction, and risk deciles support trustworthy interpretation and Northwest Florida translation.")))
          )
        )
      ),
    
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Data & Cohorts"),
          card_body(
            tags$ul(
              tags$li(strong("LCA Analytic Cohort: "), "218,619 participants with complete data on 7 binary indicators (mental health, obesity, hypertension, diabetes, dyslipidemia, OA, cancer)."),
              tags$li(strong("Wearable Prediction Cohort: "), "16,065 participants with \u226530 valid Fitbit activity days merged with LCA-derived outcomes."),
              tags$li(strong("Wearable Features: "), "Mean daily steps, step variability (CV), % low-activity days (<3K), % high-activity days (\u226510K), weekend-weekday difference, observation density."),
              tags$li(strong("Demographic Predictors: "), "Age, sex at birth, race/ethnicity, education, marital status, broad geographic region."),
              tags$li(strong("Primary Data Source: "), "NIH All of Us Research Program.")
            )
          )
        ),
        card(
          card_header("Dashboard Guide"),
          card_body(
            tags$ul(
              tags$li(strong("Descriptive Data: "), "Explore sociodemographic and clinical characteristics by cohort."),
              tags$li(strong("LCA Subgroups: "), "View the five mental-metabolic chronic disease phenotypes and their indicator profiles."),
              tags$li(strong("Risk Prediction: "), "Preview inputs for the High Multidomain Burden prediction model."),
              tags$li(strong("Team: "), "Meet the interdisciplinary research team and community advisor." )
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
        "Latent Class Analysis of Mental-Metabolic Chronic Disease Phenotypes",
        "LCA identified five clinically interpretable multimorbidity phenotypes from 218,619 participants using seven binary indicators: mental-health burden, obesity, hypertension, diabetes, dyslipidemia, osteoarthritis, and cancer."
      ),

      card(card_header("Five Mental-Metabolic Phenotypes (N = 218,619)"),
        card_body(
          p(class = "section-lede", "The five-class solution was selected based on entropy (0.769), mean posterior probability (0.849), and clinical interpretability. The High Multidomain Burden phenotype represents the broadest concurrent burden and serves as the primary prediction target."),
          layout_columns(col_widths = c(12, 6, 6, 6, 6), !!!lapply(lca_classes, lca_value_box))
        )
      ),

      card(card_header("LCA Indicator Distribution"),
        card_body(
          p("Seven binary indicators used to define latent phenotypes:"),
          tags$table(class = "table table-sm table-striped",
            tags$thead(tags$tr(tags$th("Indicator"), tags$th("Present, n (%)"), tags$th("Absent, n (%)"))),
            tags$tbody(
              tags$tr(tags$td("Mental-health burden"), tags$td("113,193 (51.8%)"), tags$td("105,426 (48.2%)")),
              tags$tr(tags$td("Obesity"), tags$td("111,545 (51.0%)"), tags$td("107,074 (49.0%)")),
              tags$tr(tags$td("Hypertension"), tags$td("167,351 (76.5%)"), tags$td("51,268 (23.5%)")),
              tags$tr(tags$td("Diabetes"), tags$td("77,666 (35.5%)"), tags$td("140,953 (64.5%)")),
              tags$tr(tags$td("Dyslipidemia"), tags$td("61,888 (28.3%)"), tags$td("156,731 (71.7%)")),
              tags$tr(tags$td("Osteoarthritis"), tags$td("109,539 (50.1%)"), tags$td("109,080 (49.9%)")),
              tags$tr(tags$td("Cancer (top 10)"), tags$td("34,183 (15.6%)"), tags$td("184,436 (84.4%)"))
            )
          )
        )
      ),

      card(card_header("LCA Phenotype Profiles"),
        card_body(
          div(class = "text-center",
              imageOutput("lca_figure", height = "auto")
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
  nav_panel("Risk Prediction",
    div(class = "container-fluid py-4",
      section_header(
        "High Multidomain Burden Risk Prediction",
        "MetaSense predicts probability of High Multidomain Burden phenotype membership using wearable-derived activity and demographic features. Model AUC = 0.691 on held-out test data (N = 16,065 wearable cohort)."
      ),

      # Model performance summary card
      card(card_header("Model Performance Summary"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(title = "Test ROC AUC", value = "0.691", theme = value_box_theme(bg = navy, fg = "white")),
            value_box(title = "Sensitivity", value = "71.5%", theme = value_box_theme(bg = uwf_green, fg = "white")),
            value_box(title = "Specificity", value = "55.1%", theme = value_box_theme(bg = navy, fg = "white")),
            value_box(title = "NPV", value = "82.9%", theme = value_box_theme(bg = uwf_green, fg = "white"))
          ),
          p(class = "text-muted small", "Operating threshold = 0.266 (Youden's index). MetaSense is designed as a screening/stratification tool, not a diagnostic classifier. Positive results should prompt additional assessment.")
        )
      ),

      layout_sidebar(
        sidebar = sidebar(
          width = 400,
          title = "Input Features",
          open = "always",

          h6("Demographics"),
          numericInput("ml_age", "Age (years)", value = 65, min = 18, max = 100),
          selectInput("ml_sex", "Sex at Birth", choices = c("Female", "Male")),
          selectInput("ml_race", "Race/Ethnicity", choices = c("White", "Black or African American", "Hispanic", "Other")),
          selectInput("ml_education", "Education", choices = c("College graduate or higher", "Some college", "High school or less")),
          selectInput("ml_marital", "Marital Status", choices = c("Married/Partnered", "Never Married", "Previously Married")),
          selectInput("ml_region", "Broad Geographic Region", choices = c("South", "Northeast", "Midwest", "West")),

          tags$hr(),
          h6("Fitbit Wearable Activity Features"),
          numericInput("ml_steps", "Mean Daily Steps", value = 5968, min = 0, max = 30000, step = 100),
          numericInput("ml_step_cv", "Daily Step Variability (CV)", value = 0.49, min = 0, max = 2, step = 0.01),
          numericInput("ml_low_days", "% Days < 3,000 Steps", value = 15.7, min = 0, max = 100, step = 0.1),
          numericInput("ml_high_days", "% Days \u2265 10,000 Steps", value = 9.7, min = 0, max = 100, step = 0.1),
          numericInput("ml_wknd_diff", "Weekend\u2013Weekday Step Difference", value = -208, min = -5000, max = 5000, step = 10),
          numericInput("ml_activity_days", "Valid Fitbit Activity Days", value = 353, min = 30, max = 2000),

          tags$hr(),
          actionButton("ml_predict", "Estimate Risk", class = "btn-primary w-100")
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
  output$lca_figure <- renderImage({
    list(
      src = "data/lca/LCA.png",
      width = "60%",
      alt = "LCA Mental-Metabolic Phenotype Profiles"
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
  # Risk Prediction (mockup - model not yet connected)
  # -------------------------------------------------------------
  observeEvent(input$ml_predict, {
    output$ml_output <- renderUI({
      tagList(
        div(class = "alert alert-warning",
            strong("Model not yet connected."),
            " Below is the input feature vector that would be passed to the MetaSense XGBoost model for High Multidomain Burden prediction."
        ),
        h5("Demographic Features"),
        tags$table(class = "table table-striped table-sm",
          tags$tbody(
            tags$tr(tags$td("Age"), tags$td(input$ml_age, " years")),
            tags$tr(tags$td("Sex at Birth"), tags$td(input$ml_sex)),
            tags$tr(tags$td("Race/Ethnicity"), tags$td(input$ml_race)),
            tags$tr(tags$td("Education"), tags$td(input$ml_education)),
            tags$tr(tags$td("Marital Status"), tags$td(input$ml_marital)),
            tags$tr(tags$td("Broad Region"), tags$td(input$ml_region))
          )
        ),
        h5("Wearable Activity Features"),
        tags$table(class = "table table-striped table-sm",
          tags$tbody(
            tags$tr(tags$td("Mean Daily Steps"), tags$td(format(input$ml_steps, big.mark = ","))),
            tags$tr(tags$td("Daily Step Variability (CV)"), tags$td(input$ml_step_cv)),
            tags$tr(tags$td("% Days < 3,000 Steps"), tags$td(paste0(input$ml_low_days, "%"))),
            tags$tr(tags$td("% Days \u2265 10,000 Steps"), tags$td(paste0(input$ml_high_days, "%"))),
            tags$tr(tags$td("Weekend\u2013Weekday Step Diff"), tags$td(input$ml_wknd_diff)),
            tags$tr(tags$td("Valid Activity Days"), tags$td(input$ml_activity_days))
          )
        ),
        div(class = "alert alert-info mt-3",
            p(strong("Interpretation Note:"), " Once connected, the model will output:"),
            tags$ul(
              tags$li("Predicted probability of High Multidomain Burden membership"),
              tags$li("Risk stratum (decile 1\u201310, where decile 10 = ~55% observed prevalence)"),
              tags$li("Conformal prediction set: {HighBurden}, {Other}, or {Both} indicating prediction certainty")
            )
        )
      )
    })
  })
}

shinyApp(ui, server)
