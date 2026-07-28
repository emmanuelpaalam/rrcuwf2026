library(shiny)

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
               p("Distributions of key clinical variables in the All of Us cohort."),
               
               fluidRow(
                 column(6, plotOutput("bp_histogram")),
                 column(6, plotOutput("glucose_histogram"))
               )
      ),
      
      tabPanel("LCA Subgroups", 
               br(),
               h3("Latent Class Analysis"),
               p("Visualization of distinct metabolic risk subgroups and chronic disease burden."),
               
               fluidRow(
                 column(12, plotOutput("lca_cluster_plot"))
               )
      )
    )
  )
)


server <- function(input, output, session) {
  # Uses fake data for now
  output$bp_histogram <- renderPlot({
    # Generates a fake histogram for blood pressure
    hist(rnorm(500, mean = 130, sd = 15), 
         main = "Systolic Blood Pressure", 
         xlab = "mmHg", col = "#a6cee3", border = "white")
  })
  
  output$glucose_histogram <- renderPlot({
    # Generates a fake histogram for fasting glucose
    hist(rnorm(500, mean = 105, sd = 25), 
         main = "Fasting Blood Sugar", 
         xlab = "mg/dL", col = "#b2df8a", border = "white")
  })
  
  # --- TAB 2 Output: Dummy LCA Plot ---
  output$lca_cluster_plot <- renderPlot({
    # Generates a fake scatter plot showing 3 'subgroups'
    x <- rnorm(300)
    y <- rnorm(300)
    clusters <- sample(c("Class 1", "Class 2", "Class 3"), 300, replace = TRUE)
    color_map <- c("Class 1" = "#e41a1c", "Class 2" = "#377eb8", "Class 3" = "#4daf4a")
    
    plot(x, y, 
         main = "Metabolic Risk Subgroups (Dummy Data)",
         xlab = "Latent Variable 1", ylab = "Latent Variable 2",
         col = color_map[clusters],
         pch = 19, cex = 1.5)
    legend("topright", legend = names(color_map), fill = color_map, bty = "n")
  })
}
shinyApp(ui, server)