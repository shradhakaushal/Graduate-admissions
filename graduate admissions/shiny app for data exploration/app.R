
df <- read.csv("C://Users//HP//Downloads//graduate-admissions//Admission_Predict.csv")

library(shiny)
library(ggplot2)
ui <- fluidPage(
    titlePanel("Regression Model (Dataset: Graduate Admissions)"),
    sidebarLayout(
        sidebarPanel(
            selectInput("outcome", label = h3("Outcome"),
                        choices = list("GRE.Score" = "GRE.Score",
                                       "TOEFL.Score" = "TOEFL.Score",
                                       "CGPA" = "CGPA",
                                       "LOR" = "LOR",
                                       "University.Rating" = "University.Rating",
                                       "Chance.of.Admit" = "Chance.of.Admit"), selected = 1),
            
            selectInput("indepvar", label = h3("explantory variable"),
                        choices = list("GRE.Score" = "GRE.Score",
                                       "TOEFL.Score" = "TOEFL.Score",
                                       "CGPA" = "CGPA",
                                       "LOR" = "LOR",
                                       "University.Rating" = "University.Rating",
                                       "Chance.of.Admit" = "Chance.of.Admit"), selected = 1),
        ),
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Scatterplot", plotOutput("varplot")), # Plot
                        
                        tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                        tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                        tabPanel("Correlation between scores", verbatimTextOutput("Correlation"))
            )
        )
    ))


server <- function(input, output) {
    
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(df[,input$outcome] ~ df[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(df, options = list(lengthChange = FALSE))
    })
    
    
    # Scatterplot output
    output$varplot <- renderPlot({
        ggplot(df, mapping=aes(x=df[,input$indepvar],y= df[,input$outcome]))+geom_point()+geom_smooth()})
    
    
    # Histogram output var 1
    output$Correlation <- renderPrint({
        cor.test(df[,input$outcome], df[,input$indepvar], method = "pearson")
    })
}

shinyApp(ui = ui, server = server)
