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
               p("Distributions of key clinical variables in the selected All of Us cohort."),
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
               h3("Latent Class Analysis"),
               p("Visualization of distinct metabolic risk subgroups and chronic disease burden."),
               
      )
    )
  )
)


server <- function(input, output, session) {
  output$dem1 <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/dem_birthsex.rds")})
  output$dem2 <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/dem_raceeth.rds")})
  output$dem3 <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/dem_age.rds")})
  output$mets_prev <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/metsprev.rds")})
  output$metcount <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/metcount.rds")})
  output$impacts <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/fatiguemet.rds")})
  output$smoking <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/smokrisk.rds")})
  output$activity <- renderPlot({readRDS("~/repos/MetaSenseRepo/data/actlevels.rds")})
}
shinyApp(ui, server)