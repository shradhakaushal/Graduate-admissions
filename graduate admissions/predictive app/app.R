df <- read.csv("C://Users//HP//Downloads//graduate-admissions//Admission_Predict.csv")
df1 <- (df[,2:9])
    
library(shiny)
ui <- fluidPage(
    titlePanel("Chance of Admit "),
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "GRE.Score", label = "Your Gre Score", min= 270, max=340, value = 270),
            numericInput(inputId = "TOEFL.Score", label = "Your TOEFL Score", min= 90, max=120, value = 90),
            numericInput(inputId = "University.Rating", label = "University rank", min=1, max=5, value = 1),
            numericInput(inputId = "SOP", label = "SOP Strength (out of 5)", min =1, max=5, value = 1, step = 0.5),
            numericInput(inputId = "LOR", label = "LOR Strength (out of 5)", min =1, max=5, value = 1, step = 0.5),
            numericInput(inputId = "CGPA", label = "Your CGPA", min =6.8, max=10, step = 0.01, value=6.8),
            numericInput(inputId = "Research", label="Research exp", min=0, max = 1, value = 0),

        ),
        mainPanel (
            h3("Chance of admission is:"),
            textOutput("pred1"))
    )
)
server <- function(input, output, session) {
    model1 = lm(Chance.of.Admit ~ . -SOP -University.Rating, data = df1)
    model1pred <- reactive({
        GRE.Score <- input$GRE.Score
        TOEFL.Score <-input$TOEFL.Score
        University.Rating <- input$University.Rating
        SOP <- input$SOP
        LOR <- input$LOR
        CGPA<-input$CGPA
        Research <- input$Research
        frames = cbind(GRE.Score, TOEFL.Score, University.Rating, SOP, LOR, CGPA, Research)
        predict(model1, newdata = data.frame(frames))
        
    })
    output$pred1 <- renderText({
        model1pred()

    })
    

}

shinyApp(ui, server)
        
            
            