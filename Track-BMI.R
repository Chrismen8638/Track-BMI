## Tracking BMI Application
install.packages("shiny")

install.packages("shiny", dependencies = TRUE)
install.packages("Rcpp")
require(shiny)

 
ui = fluidPage(
  titlePanel("BMI Calculator"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter Your Measurements"),
      numericInput("weight", "Weight (kg):", value = 70, min = 1, max = 300),
      numericInput("height", "Height (m):", value = 1.75, min = 0.1, max = 3.0),
      actionButton("calculate", "Calculate BMI")
    ),
    mainPanel(
      h4("Results"),
      textOutput("bmi_result"),
      textOutput("bmi_category")
    )
  )
)

# server logic
server <- function(input, output) {
  # Reactive expression to calculate BMI when the button is clicked
  bmi <- eventReactive(input$calculate, {
    weight <- input$weight
    height <- input$height
    if (height > 0) {
      bmi_value <- weight / (height^2)
  round(bmi_value, 5) # Round to 5 decimal places
    } else {
      NA # Return NA if height is 0 or negative
    }
  })


output$bmi_result <- renderText({
  if (is.na(bmi())) {
    "Please enter a valid height (greater than 0)."
  } else {
    paste("Your BMI is:", bmi())
  }
})


output$bmi_category <- renderText({
  if (is.na(bmi())) {
    ""
  } else {
    bmi_value <- bmi()
    if (bmi_value < 18.5) {
      "Category: Underweight"
    } else if (bmi_value >= 18.5 && bmi_value < 25.0) {
      "Category: Normal weight"
    } else if (bmi_value >= 25.0 && bmi_value < 30.0) {
      "Category: Overweight"
    } else {
      "Category: Obese"
    }
  }
})
}


shinyApp(ui = ui, server = server)
