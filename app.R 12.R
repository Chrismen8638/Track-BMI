# Install shiny package if not already installed
if (!require(shiny)) install.packages("shiny")

install.packages("shiny", dependencies = TRUE)
# Load shiny library
install.packages("Rcpp")
require(shiny)

# Define the User Interface (UI)
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

# Define the Server Logic
server <- function(input, output) {
  # Reactive expression to calculate BMI when the button is clicked
  bmi <- eventReactive(input$calculate, {
    weight <- input$weight
    height <- input$height
    if (height > 0) {
      bmi_value <- (weight / (height/100)^2)
  round(bmi_value, 2) # Round to 2 decimal places
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
    } else if (bmi_value >= 18.5 && bmi_value < 25) {
      "Category: Normal weight"
    } else if (bmi_value >= 25 && bmi_value < 30) {
      "Category: Overweight"
    } else {
      "Category: Obese"
    }
  }
})
}


shinyApp(ui = ui, server = server)
