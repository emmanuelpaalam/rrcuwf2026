library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

# XGBoost is only required when the saved model bundle is present.
has_xgboost <- requireNamespace("xgboost", quietly = TRUE)

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
    .section-lede { color: #5A6B7A; max-width: 1000px; }
    .accent-rule { border: none; border-top: 3px solid %s; width: 64px; margin: 0.25rem 0 1.25rem 0; }
    .cohort-toggle .form-check-label { font-weight: 500; }
    .btn-primary { background-color: %s; border-color: %s; }
    .btn-primary:hover { background-color: #082C48; border-color: #082C48; }
    .metric-label { color:#64748B; font-size:0.82rem; text-transform:uppercase; letter-spacing:.04em; }
    .metric-value { color:%s; font-size:1.8rem; font-weight:700; }
    .status-good { color:%s; font-weight:700; }
    .status-warn { color:#A05A00; font-weight:700; }
    footer.app-footer { color: #8A97A3; font-size: 0.85rem; padding: 2rem 0 1rem 0; text-align: center; }
  ", bg_grey, navy, uwf_green, navy, navy, navy, uwf_green))

# ---------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------
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
      input_id, "Population view",
      choices = c("Full National Cohort", "Florida", "North Florida (broad region)", "Northwest Florida"),
      selected = "Full National Cohort",
      inline = TRUE
    )
  )
}

metric_box <- function(label, value, note = NULL) {
  card(
    card_body(
      div(class = "metric-label", label),
      div(class = "metric-value", value),
      if (!is.null(note)) p(class = "text-muted small mb-0", note)
    )
  )
}

placeholder_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 4.5, color = "grey40", lineheight = 1.15) +
    xlim(-1, 1) + ylim(-1, 1) +
    theme_void()
}

safe_rds_plot <- function(path, missing_msg) {
  if (file.exists(path)) readRDS(path) else placeholder_plot(missing_msg)
}

team_members <- list(
  list(name = "Achraf Cohen, PhD", role = "Statistics and AI", affiliation = "Machine learning, uncertainty quantification, statistical modeling", email = "acohen@uwf.edu", pic = "team/Cohen.png"),
  list(name = "Karishma Chhabria Unrue, PhD", role = "Public Health", affiliation = "Cancer, metabolic burden, mental health", email = "kchhabria@uwf.edu", pic = "team/Chabbria.png"),
  list(name = "Armaghan Mahmoudian, PhD", role = "Movement Sciences and Health", affiliation = "Osteoarthritis, movement science, physical health", email = "amahmoudian@uwf.edu", pic = "team/Mahmoudian.png"),
  list(name = "Shrishti Sharma", role = "Statistics and Data Science (Student)", affiliation = "Statistical modeling, machine learning", email = "fs56@students.uwf.edu", pic = "team/shrishti.png"),
  list(name = "Emmanuel Paalam", role = "Computer Science and Data Science (Student)", affiliation = "Dashboard development, data engineering", email = "ejp25@students.uwf.edu", pic = "team/emmanuel.png")
)

community_advisors <- list(
  list(name = "Licheng \u201cTony\u201d Lee, M.D., FACC", role = "Community Advisor", affiliation = "Baptist Health", email = "licheng.lee@bhcpns.org", pic = NULL)
)

team_card <- function(member) {
  avatar <- if (!is.null(member$pic) && file.exists(file.path("www", member$pic))) {
    tags$img(src = member$pic, style = "width:120px; height:120px; border-radius:50%; object-fit:cover; margin:0 auto 12px auto; display:block;")
  } else {
    div(
      style = paste0("width:120px; height:120px; border-radius:50%; margin:0 auto 12px auto; background:", navy,
                     "; color:white; display:flex; align-items:center; justify-content:center; font-size:2rem; font-weight:600;"),
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
      if (!is.null(member$email)) p(class = "small", tags$a(href = paste0("mailto:", member$email), member$email))
    )
  )
}

# ---------------------------------------------------------------------
# RESULTS ALREADY ESTABLISHED IN THE TECHNICAL REPORT
# ---------------------------------------------------------------------
lca_indicator <- data.frame(
  Indicator = c("Mental-health burden", "Obesity", "Hypertension", "Diabetes", "Dyslipidemia", "Osteoarthritis", "Cancer"),
  PresentPct = c(51.8, 51.0, 76.5, 35.5, 28.3, 50.1, 15.6)
)

lca_profile <- data.frame(
  Profile = c("Diabetes-Dominant", "Obesity-Dominant", "High Multidomain Burden", "Dyslipidemia/OA-Dominant", "Hypertension-Dominant"),
  Prevalence = c(14.9, 19.6, 33.2, 10.5, 21.9),
  MH = c(.342, .535, .715, .406, .377),
  Obesity = c(.368, 1.000, .723, .081, .052),
  HTN = c(.710, .348, .954, .540, 1.000),
  Diabetes = c(1.000, .034, .597, .016, 0.000),
  Dyslipidemia = c(.134, .055, .445, 1.000, 0.000),
  OA = c(.208, .297, .845, .527, .351),
  Cancer = c(.087, .056, .245, .210, .134)
)

modal_counts <- data.frame(
  Profile = c("Diabetes-Dominant", "Obesity-Dominant", "High Multidomain Burden", "Dyslipidemia/OA-Dominant", "Hypertension-Dominant"),
  n = c(36079, 40174, 68574, 23742, 50050)
)

florida_profile <- data.frame(
  Profile = c("Diabetes-Dominant", "Dyslipidemia/OA-Dominant", "High Multidomain Burden", "Hypertension-Dominant", "Obesity-Dominant"),
  n = c(52, 75, 160, 110, 100),
  pct = c(10.5, 15.1, 32.2, 22.1, 20.1)
)

wearable_compare <- data.frame(
  Group = c("Other", "High Multidomain Burden"),
  MedianSteps = c(6351, 5003),
  LowActivity = c(13.3, 23.2),
  HighActivity = c(12.6, 4.2)
)

florida_compare <- data.frame(
  Group = c("Florida", "Other states"),
  N = c(497, 15568),
  HighBurdenPct = c(32.2, 28.4),
  MedianAge = c(63, 65),
  MedianSteps = c(5670, 5982),
  LowActivity = c(17.3, 15.6),
  HighActivity = c(7.69, 9.78),
  StepCV = c(.499, .490)
)

model_perf <- data.frame(
  Metric = c("ROC AUC", "Sensitivity", "Specificity", "PPV", "NPV", "F1 score", "Brier score"),
  Value = c(.691, .715, .551, .389, .829, .504, .185)
)

# ---------------------------------------------------------------------
# PLOTS BUILT FROM ESTABLISHED RESULTS
# ---------------------------------------------------------------------
plot_indicator_prevalence <- function() {
  ggplot(lca_indicator, aes(x = reorder(Indicator, PresentPct), y = PresentPct)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "Participants with indicator (%)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

plot_wearable_compare <- function() {
  dat <- bind_rows(
    wearable_compare |> transmute(Group, Measure = "Days <3,000 steps", Value = LowActivity),
    wearable_compare |> transmute(Group, Measure = "Days >=10,000 steps", Value = HighActivity)
  )
  ggplot(dat, aes(x = Group, y = Value, fill = Measure)) +
    geom_col(position = "dodge") +
    labs(x = NULL, y = "Median proportion of days (%)", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")
}

plot_lca_profiles <- function(dat, value_col = "Prevalence", ylab = "Estimated class prevalence (%)") {
  ggplot(dat, aes(x = reorder(Profile, .data[[value_col]]), y = .data[[value_col]])) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

plot_lca_bubble <- function() {
  long <- tidyr::pivot_longer(
    lca_profile,
    cols = c(MH, Obesity, HTN, Diabetes, Dyslipidemia, OA, Cancer),
    names_to = "Indicator", values_to = "Probability"
  )
  ggplot(long, aes(x = Indicator, y = Profile, size = Probability, fill = Probability)) +
    geom_point(shape = 21, color = "grey25") +
    scale_size(range = c(2, 11), limits = c(0, 1)) +
    scale_fill_gradient(low = "white", high = navy, limits = c(0, 1)) +
    labs(x = NULL, y = NULL, size = "Probability", fill = "Probability") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1))
}

plot_florida_compare <- function() {
  ggplot(florida_compare, aes(x = Group, y = HighBurdenPct)) +
    geom_col() +
    geom_text(aes(label = paste0(HighBurdenPct, "%")), vjust = -0.4, size = 4) +
    ylim(0, 40) +
    labs(x = NULL, y = "High Multidomain Burden (%)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ---------------------------------------------------------------------
# MODEL BUNDLE
# Expected file: data/model/metasense_model_bundle.rds
# Required fields:
#   model       : fitted xgb.Booster
#   columns     : colnames(x_train_clean)
#   factor_levels: named list of factor levels
#   qhat_other  : class-conditional conformal threshold
#   qhat_high   : class-conditional conformal threshold
#   threshold   : operating threshold (0.266 in final report)
# ---------------------------------------------------------------------
model_bundle_path <- "data/model/metasense_model_bundle.rds"
model_bundle <- NULL
if (file.exists(model_bundle_path)) {
  model_bundle <- tryCatch(readRDS(model_bundle_path), error = function(e) NULL)
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
  # ABOUT
  # -----------------------------------------------------------------
  nav_panel("About",
    div(class = "container-fluid py-4",
      section_header(
        "MetaSense: Trustworthy Wearable Intelligence for Multimorbidity Stratification",
        "A research prototype that discovers clinically meaningful multimorbidity patterns and uses wearable activity plus demographics to estimate High Multidomain Burden with explicit uncertainty."
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("What MetaSense Does"),
          card_body(
            p("MetaSense separates phenotype discovery from prediction. Clinical indicators first define five multimorbidity phenotypes using latent class analysis. A separate machine-learning model then uses wearable-derived activity and demographic information to estimate the probability that a participant belongs to the High Multidomain Burden phenotype."),
            p("The model does not use the diagnoses that define the latent classes as predictors. This prevents target leakage and makes the wearable model a true screening and stratification prototype rather than a diagnostic tool."),
            p("Class-conditional conformal prediction adds a trust layer so the system can return a lower-burden pattern supported, an elevated-burden signal, or an uncertain result that recommends additional information rather than forcing a classification.")
          )
        ),
        card(
          card_header("Current Evidence Base"),
          card_body(
            p(strong("Phenotype discovery:"), " N = 218,619"),
            p(strong("Wearable prediction:"), " N = 16,065"),
            p(strong("Florida wearable cohort:"), " N = 497"),
            p(strong("Northwest Florida (ZIP3 324/325):"), " N = 5"),
            div(class = "alert alert-warning small",
                "Northwest Florida is too sparse for reliable local estimates in the current All of Us wearable cohort. Florida-wide results are shown as the current regional benchmark.")
          )
        )
      ),
      card(
        card_header("How MetaSense Works"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            card(card_body(h5("1. Discover"), p("Identify five multimorbidity phenotypes from mental-health, metabolic, OA, and cancer indicators."))),
            card(card_body(h5("2. Prioritize"), p("Focus prediction on High Multidomain Burden, the broadest and most clinically actionable phenotype."))),
            card(card_body(h5("3. Predict"), p("Use demographics and seven wearable activity features to estimate High Multidomain Burden probability."))),
            card(card_body(h5("4. Trust + Translate"), p("Apply calibration and conformal prediction, then translate findings to Florida and future Northwest Florida validation.")))
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Inputs Used by the Final ML Model"),
          card_body(
            tags$ul(
              tags$li(strong("Demographics: "), "age, sex at birth, race/ethnicity, education, marital status, broad geographic region."),
              tags$li(strong("Wearable activity: "), "mean steps, step CV, proportion of days <3,000 steps, proportion of days >=10,000 steps, weekend-weekday difference, valid activity days, observation density."),
              tags$li(strong("Not used as predictors: "), "mental-health burden, obesity, hypertension, diabetes, dyslipidemia, OA, cancer, or ZIP code.")
            )
          )
        ),
        card(
          card_header("For Health Leadership"),
          card_body(
            p(strong("Interpretation:"), " MetaSense is a screening and population-stratification tool, not a diagnostic classifier."),
            p(strong("Best current use:"), " identify groups with meaningfully different multimorbidity burden and flag when wearable information is insufficient for a definitive signal."),
            p(strong("Local translation:"), " Florida-wide results are available now; Northwest Florida requires a larger local validation dataset.")
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
        "Population Overview",
        "Simple descriptive summaries for leadership. National results describe the full MetaSense cohorts; Florida is an exploratory regional benchmark; Northwest Florida is explicitly flagged when the data are too sparse."
      ),
      card(card_body(cohort_toggle("desc_region_toggle"))),
      uiOutput("desc_summary_boxes"),
      uiOutput("desc_message"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Multimorbidity Burden"), card_body(plotOutput("desc_plot1", height = 360))),
        card(card_header("Wearable Activity"), card_body(plotOutput("desc_plot2", height = 360)))
      ),
      card(
        card_header("Data Needed for North / Northwest Florida Expansion"),
        card_body(
          p("To populate local descriptive panels beyond the current Florida benchmark, create a participant-level file using the same wearable cohort definitions and add:"),
          tags$ul(
            tags$li("ZIP3 and broad location_region"),
            tags$li("age, sex at birth, race/ethnicity, education, marital status"),
            tags$li("LCA profile / High Multidomain Burden indicator"),
            tags$li("activity_days, steps_mean, steps_cv, pct_lt_3000, pct_ge_10000, weekend_difference, wear_density")
          ),
          p(class = "small text-muted", "Recommended saved object: data/region/region_descriptives.rds with one row per participant. Northwest Florida should be defined using ZIP3 324 or 325. Keep broad 'North Florida' as a separate descriptive region if used; it is not equivalent to Northwest Florida.")
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
        "Latent Class Analysis of Multimorbidity Profiles",
        "Five reproducible, clinically interpretable profiles were identified from seven binary mental-health, metabolic, musculoskeletal, and cancer indicators."
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        metric_box("Selected solution", "5 classes", "Relative entropy = 0.769"),
        metric_box("LCA cohort", "218,619", "120 of 128 possible response patterns observed"),
        metric_box("Largest profile", "33.2%", "High Multidomain Burden")
      ),
      card(card_header("Five Multimorbidity Profiles"), card_body(plotOutput("lca_distribution", height = 380))),
      card(
        card_header("Model Diagnostics and Clinical Pattern"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(h5("Conditional Item-Response Probabilities", class = "text-center"), plotOutput("bubble_chart", height = 470)),
            div(h5("Multiple Correspondence Analysis", class = "text-center"), plotOutput("mca_plot", height = 470))
          )
        )
      ),
      card(
        card_header("Regional Profile Distribution"),
        card_body(
          cohort_toggle("region_toggle"),
          uiOutput("lca_region_note"),
          plotOutput("geographic_distribution", height = 380)
        )
      ),
      card(
        card_header("Class Composition by Demographic"),
        card_body(
          p(class = "section-lede", "These panels require participant-level class-by-demographic summaries. If the saved RDS files are present they are displayed; otherwise the dashboard states exactly which file is needed."),
          layout_columns(col_widths = c(6, 6), plotOutput("age_plot"), plotOutput("sex_plot")),
          layout_columns(col_widths = c(6, 6), plotOutput("race_plot"), plotOutput("education_plot")),
          plotOutput("marital_plot")
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # ML MODEL INPUT
  # -----------------------------------------------------------------
  nav_panel("ML Model Input",
    div(class = "container-fluid py-4",
      section_header(
        "Interactive MetaSense Burden Prediction",
        "Enter the same demographic and wearable variables used by the final model. The application returns the predicted probability of High Multidomain Burden plus an uncertainty-aware conformal prediction set."
      ),
      layout_sidebar(
        sidebar = sidebar(
          width = 390,
          title = "Participant Inputs",
          open = "always",

          h6("Demographics"),
          numericInput("ml_age", "Age (years)", value = 65, min = 18, max = 100),
          selectInput("ml_sex", "Sex at birth", choices = c("Female", "Male")),
          selectInput("ml_race", "Race/Ethnicity", choices = c("White", "Black or African American", "Hispanic", "Other")),
          selectInput("ml_education", "Education", choices = c("College graduate or higher", "Some college", "High school or less")),
          selectInput("ml_marital", "Marital status", choices = c("Married/Partnered", "Never Married", "Previously Married")),
          selectInput("ml_region", "Broad geographic region", choices = c(
            "Pacific/West", "Central/South Central", "Upper Midwest/Northern Plains",
            "Mid-Atlantic/Southeast", "Great Lakes/Ohio Valley", "Eastern ZIP Zone 1",
            "Mountain/Southwest", "Eastern ZIP Zone 0", "North Florida"
          )),

          tags$hr(),
          h6("Wearable Activity"),
          numericInput("ml_steps_mean", "Mean daily steps", value = 6000, min = 100, max = 100000, step = 100),
          numericInput("ml_steps_cv", "Daily step variability (CV)", value = 0.49, min = 0, max = 3, step = 0.01),
          numericInput("ml_pct_lt_3000", "% days with <3,000 steps", value = 16, min = 0, max = 100, step = 1),
          numericInput("ml_pct_ge_10000", "% days with >=10,000 steps", value = 10, min = 0, max = 100, step = 1),
          numericInput("ml_weekend_difference", "Weekend - weekday steps", value = -200, min = -20000, max = 20000, step = 100),
          numericInput("ml_activity_days", "Valid activity days", value = 353, min = 30, max = 5000),
          numericInput("ml_wear_density", "Wearable observation density (0-1)", value = 0.80, min = 0, max = 1, step = 0.01),

          tags$hr(),
          actionButton("ml_predict", "Run MetaSense Prediction", class = "btn-primary w-100")
        ),
        div(
          uiOutput("ml_model_status"),
          uiOutput("ml_output"),
          card(
            card_header("Model Performance Context"),
            card_body(
              layout_columns(
                col_widths = c(4, 4, 4),
                metric_box("Held-out ROC AUC", "0.691", "Moderate discrimination"),
                metric_box("Sensitivity", "71.5%", "At calibration-selected threshold 0.266"),
                metric_box("NPV", "82.9%", "Cohort-specific; prevalence dependent")
              ),
              p(class = "small text-muted", "Class-conditional conformal prediction achieved 90.8% coverage for High Multidomain Burden and 91.2% for Other in the held-out test set. Approximately 62.2% of participants received an intentionally ambiguous two-class set.")
            )
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
      br(), h4("Community Advisor"), tags$hr(class = "accent-rule"),
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

  # ---------------------------
  # DESCRIPTIVE DATA
  # ---------------------------
  output$desc_summary_boxes <- renderUI({
    view <- input$desc_region_toggle
    if (view == "Full National Cohort") {
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        metric_box("LCA discovery cohort", "218,619", "Clinical-indicator cohort"),
        metric_box("Wearable ML cohort", "16,065", "At least 30 valid activity days"),
        metric_box("High burden", "28.5%", "4,581 of 16,065 wearable participants"),
        metric_box("Median daily steps", "5,968", "Wearable prediction cohort")
      )
    } else if (view == "Florida") {
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        metric_box("Florida wearable N", "497"),
        metric_box("High burden", "32.2%", "160 of 497"),
        metric_box("Median daily steps", "5,670"),
        metric_box("Low-activity days", "17.3%", "Median % of days <3,000 steps")
      )
    } else if (view == "North Florida (broad region)") {
      layout_columns(
        col_widths = c(4, 4, 4),
        metric_box("North Florida N", "161", "Broad location_region category"),
        metric_box("High burden", "37.9%", "61 of 161"),
        metric_box("Status", "Exploratory", "Not equivalent to ZIP3-defined Northwest Florida")
      )
    } else {
      layout_columns(
        col_widths = c(4, 4, 4),
        metric_box("Northwest Florida N", "5", "ZIP3 324/325"),
        metric_box("ZIP3 324", "2"),
        metric_box("ZIP3 325", "3")
      )
    }
  })

  output$desc_message <- renderUI({
    view <- input$desc_region_toggle
    if (view == "Florida") {
      div(class = "alert alert-info",
          "Florida is presented as an exploratory regional benchmark. High Multidomain Burden was 32.2% in Florida versus 28.4% outside Florida; these are descriptive comparisons and not evidence of a geographic disparity.")
    } else if (view == "North Florida (broad region)") {
      div(class = "alert alert-info",
          "The broad All of Us location_region category labeled 'North Florida' includes 161 wearable-cohort participants, with 37.9% classified as High Multidomain Burden. This category is broader than Northwest Florida and should not be presented as the Pensacola/Panhandle service area. Additional participant-level summaries are still needed for age, demographics, and wearable activity.")
    } else if (view == "Northwest Florida") {
      div(class = "alert alert-warning",
          strong("Local data are too sparse for reliable estimates. "),
          "Only five wearable-cohort participants were identified in ZIP3 324/325. No Northwest Florida prevalence, demographic, or model-performance estimates are displayed. A larger regional dataset is needed for validation.")
    } else {
      NULL
    }
  })

  output$desc_plot1 <- renderPlot({
    view <- input$desc_region_toggle
    if (view == "Full National Cohort") {
      plot_indicator_prevalence()
    } else if (view == "Florida") {
      plot_florida_compare()
    } else if (view == "North Florida (broad region)") {
      ggplot(data.frame(Group = c("North Florida", "Full wearable cohort"), Value = c(37.9, 28.5)),
             aes(x = Group, y = Value)) +
        geom_col() +
        geom_text(aes(label = paste0(Value, "%")), vjust = -0.4) +
        ylim(0, 45) +
        labs(x = NULL, y = "High Multidomain Burden (%)") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    } else {
      placeholder_plot("Northwest Florida: n = 5\nNo stable local burden estimate is reported.")
    }
  })

  output$desc_plot2 <- renderPlot({
    view <- input$desc_region_toggle
    if (view == "Full National Cohort") {
      plot_wearable_compare()
    } else if (view == "Florida") {
      dat <- bind_rows(
        florida_compare |> transmute(Group, Measure = "Days <3,000 steps", Value = LowActivity),
        florida_compare |> transmute(Group, Measure = "Days >=10,000 steps", Value = HighActivity)
      )
      ggplot(dat, aes(x = Group, y = Value, fill = Measure)) +
        geom_col(position = "dodge") +
        labs(x = NULL, y = "Median proportion of days (%)", fill = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(), legend.position = "top")
    } else if (view == "North Florida (broad region)") {
      placeholder_plot("North Florida wearable summaries are not yet available.\nPull the same activity features used for the national and Florida cohorts.")
    } else {
      placeholder_plot("Add a larger Northwest Florida participant-level wearable extract\nbefore displaying local activity distributions.")
    }
  })

  # ---------------------------
  # LCA SUBGROUPS
  # ---------------------------
  output$lca_distribution <- renderPlot({
    plot_lca_profiles(lca_profile)
  })

  output$bubble_chart <- renderPlot({
    plot_lca_bubble()
  })

  output$mca_plot <- renderPlot({
    # Preferred: save the grouped MCA ggplot object as data/lca/mca_grouped.rds
    if (file.exists("data/lca/mca_grouped.rds")) {
      readRDS("data/lca/mca_grouped.rds")
    } else {
      placeholder_plot("MCA plot not yet added.\nSave the grouped factoextra/ggplot object as:\ndata/lca/mca_grouped.rds")
    }
  })

  output$lca_region_note <- renderUI({
    view <- input$region_toggle
    if (view == "Florida") {
      div(class = "alert alert-info small",
          "Florida N = 497. The High Multidomain Burden phenotype was the most common profile (32.2%), followed by Hypertension-Dominant (22.1%) and Obesity-Dominant (20.1%).")
    } else if (view == "North Florida (broad region)") {
      div(class = "alert alert-info small",
          "North Florida is a broad geographic category (n = 161 in the wearable cohort; 37.9% High Multidomain Burden). Five-class profile counts by this region have not yet been added to the dashboard.")
    } else if (view == "Northwest Florida") {
      div(class = "alert alert-warning small",
          "Northwest Florida ZIP3 324/325 contains only five participants in the current wearable cohort; profile percentages are intentionally suppressed.")
    } else {
      NULL
    }
  })

  output$geographic_distribution <- renderPlot({
    view <- input$region_toggle
    if (view == "Full National Cohort") {
      plot_lca_profiles(lca_profile)
    } else if (view == "Florida") {
      plot_lca_profiles(florida_profile, value_col = "pct", ylab = "Florida participants (%)")
    } else if (view == "North Florida (broad region)") {
      placeholder_plot("North Florida five-class profile distribution not yet added.\nPull Classes by location_region == 'North Florida'.")
    } else {
      placeholder_plot("Northwest Florida profile distribution suppressed\nbecause n = 5.")
    }
  })

  output$age_plot <- renderPlot({
    safe_rds_plot(
      if (input$region_toggle == "Florida") "data/lca/class_age_florida.rds" else "data/lca/class_age.rds",
      "Age-by-class plot not yet available.\nAdd data/lca/class_age.rds (national)\nand class_age_florida.rds (Florida)."
    )
  })
  output$sex_plot <- renderPlot({
    safe_rds_plot(
      if (input$region_toggle == "Florida") "data/lca/class_sex_florida.rds" else "data/lca/class_sex.rds",
      "Sex-by-class plot not yet available.\nAdd data/lca/class_sex.rds and class_sex_florida.rds."
    )
  })
  output$race_plot <- renderPlot({
    safe_rds_plot(
      if (input$region_toggle == "Florida") "data/lca/class_race_florida.rds" else "data/lca/class_race.rds",
      "Race/ethnicity-by-class plot not yet available.\nAdd data/lca/class_race.rds and class_race_florida.rds."
    )
  })
  output$education_plot <- renderPlot({
    safe_rds_plot(
      if (input$region_toggle == "Florida") "data/lca/class_education_florida.rds" else "data/lca/class_education.rds",
      "Education-by-class plot not yet available.\nAdd data/lca/class_education.rds and class_education_florida.rds."
    )
  })
  output$marital_plot <- renderPlot({
    safe_rds_plot(
      if (input$region_toggle == "Florida") "data/lca/class_marital_florida.rds" else "data/lca/class_marital.rds",
      "Marital-status-by-class plot not yet available.\nAdd data/lca/class_marital.rds and class_marital_florida.rds."
    )
  })

  # ---------------------------
  # ML MODEL
  # ---------------------------
  output$ml_model_status <- renderUI({
    if (is.null(model_bundle)) {
      div(class = "alert alert-warning",
          strong("Model file not yet connected. "),
          "Add data/model/metasense_model_bundle.rds. The interface is already configured for the final model predictors and conformal output.")
    } else if (!has_xgboost) {
      div(class = "alert alert-danger",
          "The model bundle is present, but the R package 'xgboost' is not installed on the deployment environment.")
    } else {
      div(class = "alert alert-success",
          strong("MetaSense model connected. "),
          "Predictions use the saved XGBoost model and class-conditional conformal thresholds.")
    }
  })

  observeEvent(input$ml_predict, {
    if (is.null(model_bundle) || !has_xgboost) {
      output$ml_output <- renderUI({
        card(
          card_header("Prediction unavailable until the saved model is added"),
          card_body(
            p("The dashboard has all required user inputs, but it will not fabricate a probability without the trained model object."),
            p(strong("Required file:"), " data/model/metasense_model_bundle.rds"),
            p(class = "small text-muted", "Create this file from the R session used to train xgb_tuned. See the accompanying setup instructions provided with this app.")
          )
        )
      })
      return()
    }

    # Build exactly the variables used by the final model.
    newdata <- data.frame(
      age = input$ml_age,
      sex_at_birth = input$ml_sex,
      race_ethnicity = input$ml_race,
      education = input$ml_education,
      marital_status = input$ml_marital,
      location_region = input$ml_region,
      steps_mean = input$ml_steps_mean,
      steps_cv = input$ml_steps_cv,
      pct_lt_3000 = input$ml_pct_lt_3000 / 100,
      pct_ge_10000 = input$ml_pct_ge_10000 / 100,
      weekend_difference = input$ml_weekend_difference,
      activity_days = input$ml_activity_days,
      wear_density = input$ml_wear_density,
      stringsAsFactors = FALSE
    )

    # Apply saved factor levels if present in the bundle.
    if (!is.null(model_bundle$factor_levels)) {
      for (v in names(model_bundle$factor_levels)) {
        if (v %in% names(newdata)) {
          newdata[[v]] <- factor(newdata[[v]], levels = model_bundle$factor_levels[[v]])
        }
      }
    }

    mm <- model.matrix(~ . - 1, data = newdata)
    needed <- model_bundle$columns
    if (is.null(needed)) {
      output$ml_output <- renderUI(div(class = "alert alert-danger", "Model bundle is missing the training matrix column names."))
      return()
    }

    missing_cols <- setdiff(needed, colnames(mm))
    if (length(missing_cols) > 0) {
      zeros <- matrix(0, nrow = 1, ncol = length(missing_cols), dimnames = list(NULL, missing_cols))
      mm <- cbind(mm, zeros)
    }
    mm <- mm[, needed, drop = FALSE]

    p_high <- as.numeric(predict(model_bundle$model, xgboost::xgb.DMatrix(mm)))

    q_other <- if (!is.null(model_bundle$qhat_other)) model_bundle$qhat_other else 0.4618295
    q_high  <- if (!is.null(model_bundle$qhat_high))  model_bundle$qhat_high  else 0.8234986
    threshold <- if (!is.null(model_bundle$threshold)) model_bundle$threshold else 0.266

    include_other <- p_high <= q_other
    include_high  <- (1 - p_high) <= q_high

    conformal_set <- if (include_high && include_other) {
      "Uncertain — additional information recommended"
    } else if (include_high) {
      "Elevated-burden signal"
    } else if (include_other) {
      "Lower-burden pattern supported"
    } else {
      "No class met the conformal criterion"
    }

    threshold_signal <- if (p_high >= threshold) "Above screening threshold" else "Below screening threshold"

    output$ml_output <- renderUI({
      tagList(
        layout_columns(
          col_widths = c(4, 4, 4),
          metric_box("Predicted High Burden", paste0(round(100 * p_high, 1), "%"), "XGBoost probability"),
          metric_box("Screening signal", threshold_signal, paste0("Operating threshold = ", round(threshold, 3))),
          metric_box("Conformal interpretation", conformal_set, "90% class-conditional framework")
        ),
        card(
          card_header("How to Interpret This Result"),
          card_body(
            if (conformal_set == "Elevated-burden signal") {
              div(class = "alert alert-warning", "The wearable and demographic pattern supports an elevated-burden signal. This is not confirmation of disease or phenotype membership and should prompt additional assessment rather than automated action.")
            } else if (conformal_set == "Lower-burden pattern supported") {
              div(class = "alert alert-success", "The available wearable and demographic information supports the lower-burden pattern. This does not rule out individual chronic conditions.")
            } else if (grepl("Uncertain", conformal_set)) {
              div(class = "alert alert-info", "The model intentionally retains both possible classes. Additional clinical or contextual information is recommended rather than forcing a binary classification.")
            } else {
              div(class = "alert alert-secondary", "The current input falls outside both class-conditional conformal criteria. Review model preprocessing and input ranges.")
            },
            p(class = "small text-muted mb-0", "MetaSense estimates current High Multidomain Burden phenotype membership. It does not predict future disease onset and is not intended to diagnose or replace clinician judgment.")
          )
        )
      )
    })
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
