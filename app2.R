library(shiny)
library(ggplot2)
library(bslib)

# =====================================================================
# MetaSense
# Trustworthy wearable intelligence for mental-metabolic chronic disease
# risk stratification.
#
# Analytic framework:
#   1) LCA defines clinically meaningful risk groups using metabolic
#      syndrome components, mental health burden, and chronic disease
#      status (initially OA and cancer).
#   2) Machine learning predicts LCA-derived group membership using
#      wearable-derived physical activity and demographic predictors.
#   3) Uncertainty quantification reports confidence in each prediction.
#
# NOTE: The LCA labels and percentages below are placeholders until the
# revised LCA is rerun with chronic disease indicators included.
# =====================================================================

# ---------------------------------------------------------------------
# THEME
# ---------------------------------------------------------------------
navy      <- "#0B3D62"
uwf_green <- "#146C43"
bg_grey   <- "#F5F7F8"
ink       <- "#22303F"

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
    .section-lede { color: #5A6B7A; max-width: 980px; }
    .accent-rule { border: none; border-top: 3px solid %s; width: 64px; margin: 0.25rem 0 1.25rem 0; }
    .cohort-toggle .form-check-label { font-weight: 500; }
    .btn-primary { background-color: %s; border-color: %s; }
    .btn-primary:hover { background-color: #082C48; border-color: #082C48; }
    .workflow-step { min-height: 150px; }
    .prediction-label { font-size: 1.5rem; font-weight: 700; color: %s; }
    .uncertainty-low { color: %s; font-weight: 700; }
    .uncertainty-high { color: #A94442; font-weight: 700; }
    footer.app-footer { color: #8A97A3; font-size: 0.85rem; padding: 2rem 0 1rem 0; text-align: center; }
  ", bg_grey, navy, uwf_green, navy, navy, navy, uwf_green))

# ---------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------
placeholder_plot <- function(msg = "Data not yet connected for this view.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, color = "grey45") +
    xlim(-1, 1) + ylim(-1, 1) +
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
  div(
    class = "cohort-toggle",
    radioButtons(
      input_id,
      "Cohort",
      choices = c("Full National Cohort", "Northwest Florida Cohort"),
      selected = "Full National Cohort",
      inline = TRUE
    )
  )
}

# Placeholder labels until revised LCA is rerun.
lca_classes <- list(
  list(pct = "--", title = "Risk Group 1", color = navy),
  list(pct = "--", title = "Risk Group 2", color = uwf_green),
  list(pct = "--", title = "Risk Group 3", color = navy),
  list(pct = "--", title = "Risk Group 4", color = uwf_green),
  list(pct = "--", title = "Risk Group 5", color = navy)
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
  list(
    name = "Achraf Cohen, PhD",
    role = "Statistics and AI",
    affiliation = "Machine learning, uncertainty quantification, wearable analytics, statistical modeling",
    email = "acohen@uwf.edu",
    pic = "team/Cohen.png"
  ),
  list(
    name = "Karishma Chhabria Unrue, PhD",
    role = "Public Health",
    affiliation = "Cancer, metabolic burden, mental health, population health",
    email = "kchhabria@uwf.edu",
    pic = "team/Chabbria.png"
  ),
  list(
    name = "Armaghan Mahmoudian, PhD",
    role = "Movement Sciences and Health",
    affiliation = "Osteoarthritis, physical therapy, movement and function",
    email = "amahmoudian@uwf.edu",
    pic = "team/Mahmoudian.png"
  ),
  list(
    name = "Shrishti Sharma",
    role = "Statistics and Data Science (Student)",
    affiliation = "Latent class analysis, statistical modeling, machine learning",
    email = "fs56@students.uwf.edu",
    pic = "team/shrishti.png"
  ),
  list(
    name = "Emmanuel Paalam",
    role = "Computer Science and Data Science (Student)",
    affiliation = "Dashboard development, data engineering, visualization",
    email = "ejp25@students.uwf.edu",
    pic = "team/emmanuel.png"
  )
)

community_advisors <- list(
  list(
    name = "Licheng \"Tony\" Lee, M.D., FACC",
    role = "Community Advisor",
    affiliation = "Baptist Health",
    email = "licheng.lee@bhcpns.org"
  )
)

team_card <- function(member) {
  avatar <- if (!is.null(member$pic)) {
    tags$img(
      src = member$pic,
      style = "width:120px; height:120px; border-radius:50%; object-fit:cover; margin:0 auto 12px auto; display:block;"
    )
  } else {
    div(
      style = paste0(
        "width:120px; height:120px; border-radius:50%; margin:0 auto 12px auto;",
        "background:", navy, "; color:white; display:flex; align-items:center;",
        "justify-content:center; font-size:2rem; font-weight:600;"
      ),
      toupper(substr(member$name, 1, 1))
    )
  }

  card(
    class = "text-center",
    card_body(
      avatar,
      h5(member$name),
      p(class = "mb-0", strong(member$role)),
      p(class = "text-muted", member$affiliation),
      if (!is.null(member$email)) {
        p(class = "small", tags$a(href = paste0("mailto:", member$email), member$email))
      }
    )
  )
}

advisor_card <- function(advisor) {
  card(
    card_body(
      h5(advisor$name),
      p(class = "mb-0", strong(advisor$role)),
      p(advisor$affiliation),
      tags$a(href = paste0("mailto:", advisor$email), advisor$email)
    )
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_navbar(
  title = "MetaSense",
  theme = app_theme,
  fillable = FALSE,
  bg = navy,

  # -----------------------------------------------------------------
  # ABOUT
  # -----------------------------------------------------------------
  nav_panel(
    "About",
    div(
      class = "container-fluid py-4",
      section_header(
        "MetaSense: Trustworthy Wearable Intelligence for Chronic Disease Risk Stratification",
        paste(
          "Discovering mental-metabolic chronic disease risk groups and predicting individual",
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
          card_header("Study Cohorts"),
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
  nav_panel(
    "Descriptive Data",
    div(
      class = "container-fluid py-4",
      section_header(
        "Mental-Metabolic and Chronic Disease Profile",
        paste(
          "Describe the population used to define MetaSense risk groups, including metabolic syndrome components,",
          "mental health burden, chronic disease status, demographics, and wearable-derived behavior."
        )
      ),

      card(card_body(cohort_toggle("desc_region_toggle"))),

      card(
        card_header("Demographics"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("dem1"), plotOutput("dem2")),
          plotOutput("dem3")
        )
      ),

      card(
        card_header("Metabolic Syndrome Components"),
        card_body(plotOutput("mets_prev"))
      ),

      card(
        card_header("Metabolic Burden"),
        card_body(plotOutput("metcount"))
      ),

      card(
        card_header("Mental Health and Chronic Disease Burden"),
        card_body(plotOutput("impacts"))
      ),

      card(
        card_header("Wearable-Derived Physical Activity"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("activity"), plotOutput("sedentary"))
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # LCA RISK GROUPS
  # -----------------------------------------------------------------
  nav_panel(
    "LCA Risk Groups",
    div(
      class = "container-fluid py-4",
      section_header(
        "Mental-Metabolic Chronic Disease Risk Groups",
        paste(
          "Latent Class Analysis identifies clinically distinct subgroups using metabolic syndrome components,",
          "mental health burden, and chronic disease status, initially including osteoarthritis and cancer."
        )
      ),

      card(
        card_header("LCA-Derived Risk Groups"),
        card_body(
          p(
            class = "section-lede",
            "Risk-group labels and prevalence will be assigned after the revised LCA is fit and the class-specific response probabilities are reviewed."
          ),
          layout_columns(col_widths = c(12, 6, 6, 6, 6), !!!lapply(lca_classes, lca_value_box))
        )
      ),

      card(
        card_header("Class-Defining Profiles"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(h5("Conditional Item-Response Probabilities", class = "text-center"), plotOutput("bubble_chart")),
            div(h5("Class Separation / Model Diagnostics", class = "text-center"), plotOutput("mca_plot"))
          )
        )
      ),

      card(
        card_header("Sociodemographic & Geographic Breakdown"),
        card_body(
          p(
            class = "section-lede",
            "Compare LCA risk-group composition in the national cohort and Northwest Florida participants (ZIP codes 324** and 325**)."
          ),
          cohort_toggle("region_toggle"),
          plotOutput("geographic_distribution")
        )
      ),

      card(
        card_header("Risk-Group Composition by Demographic Characteristics"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("age_plot"), plotOutput("sex_plot")),
          layout_columns(col_widths = c(6, 6), plotOutput("race_plot"), plotOutput("education_plot")),
          plotOutput("marital_plot")
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # RISK PREDICTION
  # -----------------------------------------------------------------
  nav_panel(
    "Risk Prediction",
    div(
      class = "container-fluid py-4",
      section_header(
        "Predict LCA-Defined Risk-Group Membership",
        paste(
          "Machine-learning models use wearable-derived physical activity and demographic characteristics",
          "to predict membership in the clinically defined LCA risk groups."
        )
      ),

      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Example Prediction Inputs"),
          card_body(
            numericInput("pred_age", "Age", value = 62, min = 18, max = 100),
            selectInput("pred_sex", "Sex", choices = c("Female", "Male")),
            numericInput("pred_steps", "Average daily steps", value = 4800, min = 0, max = 30000, step = 100),
            numericInput("pred_active", "Active minutes/day", value = 28, min = 0, max = 300),
            numericInput("pred_sedentary", "Sedentary minutes/day", value = 650, min = 0, max = 1440),
            numericInput("pred_variability", "Activity variability (SD of daily steps)", value = 1500, min = 0, max = 10000),
            actionButton("predict_btn", "Generate Demonstration Prediction", class = "btn-primary")
          )
        ),
        card(
          card_header("Trustworthy AI Output"),
          card_body(
            uiOutput("prediction_output"),
            tags$hr(),
            h5("Planned Modeling Strategy"),
            tags$ul(
              tags$li(strong("Outcome: "), "LCA-derived mental-metabolic chronic disease risk group."),
              tags$li(strong("Predictors: "), "Wearable physical activity and demographic characteristics."),
              tags$li(strong("Models: "), "Candidate machine-learning approaches such as random forest and gradient boosting."),
              tags$li(strong("Trustworthiness: "), "Uncertainty quantification accompanies each prediction rather than reporting an unqualified class label."),
              tags$li(strong("Interpretability: "), "Feature-importance or explainability methods will identify the wearable and demographic signals driving predictions.")
            ),
            p(
              class = "text-muted small",
              "The current interface is a prototype. Final class labels, fitted probabilities, uncertainty intervals, and feature contributions will be populated after model training and validation."
            )
          )
        )
      ),

      card(
        card_header("Model Performance & Uncertainty"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            plotOutput("model_performance"),
            plotOutput("uncertainty_plot")
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # TEAM
  # -----------------------------------------------------------------
  nav_panel(
    "Team",
    div(
      class = "container-fluid py-4",
      section_header(
        "Interdisciplinary MetaSense Team",
        "Statistics, AI, public health, movement science, cancer research, software development, and community clinical expertise."
      ),

      layout_columns(
        col_widths = c(4, 4, 4, 6, 6),
        !!!lapply(team_members, team_card)
      ),

      section_header(
        "Community Advisor",
        "Community clinical input will help ensure the project remains relevant to prevention, chronic disease management, and implementation needs in Northwest Florida."
      ),
      layout_columns(
        col_widths = c(6),
        !!!lapply(community_advisors, advisor_card)
      )
    )
  ),

  footer = tags$footer(
    class = "app-footer",
    "MetaSense | University of West Florida | Prototype for research and demonstration purposes"
  )
)

# ---------------------------------------------------------------------
# SERVER
# Replace placeholder renderPlot calls with your existing analysis plots.
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  output$dem1 <- renderPlot({
    placeholder_plot("Age distribution")
  })

  output$dem2 <- renderPlot({
    placeholder_plot("Sex distribution")
  })

  output$dem3 <- renderPlot({
    placeholder_plot("Race/ethnicity and socioeconomic characteristics")
  })

  output$mets_prev <- renderPlot({
    placeholder_plot("Prevalence of metabolic syndrome components")
  })

  output$metcount <- renderPlot({
    placeholder_plot("Distribution of metabolic syndrome burden")
  })

  output$impacts <- renderPlot({
    placeholder_plot("Mental health and chronic disease burden (OA / cancer)")
  })

  output$activity <- renderPlot({
    placeholder_plot("Wearable-derived physical activity")
  })

  output$sedentary <- renderPlot({
    placeholder_plot("Wearable-derived sedentary behavior")
  })

  output$bubble_chart <- renderPlot({
    placeholder_plot("Revised LCA conditional item-response probabilities")
  })

  output$mca_plot <- renderPlot({
    placeholder_plot("LCA model diagnostics / class separation")
  })

  output$geographic_distribution <- renderPlot({
    placeholder_plot(
      if (identical(input$region_toggle, "Northwest Florida Cohort")) {
        "Northwest Florida LCA risk-group distribution"
      } else {
        "National LCA risk-group distribution"
      }
    )
  })

  output$age_plot <- renderPlot({ placeholder_plot("Age by LCA risk group") })
  output$sex_plot <- renderPlot({ placeholder_plot("Sex by LCA risk group") })
  output$race_plot <- renderPlot({ placeholder_plot("Race/ethnicity by LCA risk group") })
  output$education_plot <- renderPlot({ placeholder_plot("Education by LCA risk group") })
  output$marital_plot <- renderPlot({ placeholder_plot("Other demographic characteristics by LCA risk group") })

  # Demonstration only: deliberately not presented as a fitted clinical model.
  demo_prediction <- eventReactive(input$predict_btn, {
    score <-
      0.00008 * pmax(0, 7000 - input$pred_steps) +
      0.002 * pmax(0, input$pred_sedentary - 500) +
      0.006 * pmax(0, 35 - input$pred_active) +
      0.003 * pmax(0, input$pred_age - 50)

    if (score > 1.6) {
      list(group = "Higher-Burden Risk Group (Demo)", probability = 0.82, uncertainty = "Low")
    } else if (score > 0.8) {
      list(group = "Intermediate Risk Group (Demo)", probability = 0.61, uncertainty = "Moderate")
    } else {
      list(group = "Lower-Burden Risk Group (Demo)", probability = 0.76, uncertainty = "Low")
    }
  })

  output$prediction_output <- renderUI({
    pred <- demo_prediction()

    if (is.null(pred)) {
      return(tagList(
        p(class = "section-lede", "Enter example wearable and demographic inputs, then generate a demonstration prediction."),
        p(class = "text-muted small", "No trained model is connected to this prototype yet.")
      ))
    }

    uncertainty_class <- if (pred$uncertainty == "Low") "uncertainty-low" else "uncertainty-high"

    tagList(
      div(class = "prediction-label", pred$group),
      p(strong("Prediction probability: "), sprintf("%.0f%%", 100 * pred$probability)),
      p(strong("Uncertainty: "), span(class = uncertainty_class, pred$uncertainty)),
      p(
        class = "text-muted small",
        "Demonstration output only. Final predictions will come from validated ML models trained to reproduce the LCA-derived risk groups."
      )
    )
  })

  output$model_performance <- renderPlot({
    placeholder_plot("Cross-validated class-prediction performance")
  })

  output$uncertainty_plot <- renderPlot({
    placeholder_plot("Prediction confidence / uncertainty distribution")
  })
}

shinyApp(ui = ui, server = server)
