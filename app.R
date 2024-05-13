library(shiny)

# Define UI
ui <- fluidPage(
        titlePanel("Bayesian Probability Calculator for Medical Tests"),
        tabsetPanel(
                tabPanel(
                        "Calculator",
                        sidebarLayout(
                                sidebarPanel(
                                        h5("Prior probability: Population rate"),
                                        numericInput("cases", "Cases", value = 25),
                                        numericInput("population", "per population", value = 1000),
                                        h5("Test Sensitivity/Specificity (%)"),
                                        numericInput("sensitivity_test1", "Sensitivity (Test 1)", 
                                                     value = 95, 
                                                     min = 0, 
                                                     max = 100),
                                        numericInput("specificity_test1", "Specificity (Test 1)", 
                                                     value = 85, 
                                                     min = 0, 
                                                     max = 100),
                                        numericInput("sensitivity_test2", "Sensitivity (Test 2)", 
                                                     value = 95, 
                                                     min = 0, 
                                                     max = 100),
                                        numericInput("specificity_test2", "Specificity (Test 2)", 
                                                     value = 85, 
                                                     min = 0, 
                                                     max = 100),
                                        numericInput("sensitivity_test3", "Sensitivity (Test 3)", 
                                                     value = 95, 
                                                     min = 0, 
                                                     max = 100),
                                        numericInput("specificity_test3", "Specificity (Test 3)", 
                                                     value = 85, 
                                                     min = 0, 
                                                     max = 100),
                        h5("Test Results"),
                        radioButtons("test1_result", "Test 1 Result", 
                                     choices = c("Negative", "Positive"), 
                                     selected = "Negative"),
                        radioButtons("test2_result", "Test 2 Result", 
                                     choices = c("Negative", "Positive"), 
                                     selected = "Negative"),
                        radioButtons("test3_result", "Test 3 Result", 
                                     choices = c("Negative", "Positive"), 
                                     selected = "Negative"),
                        actionButton("calculate", "Calculate")
                        ),
                        
                        mainPanel(
                                h3("Results:"),
                                h4("Probability of actually having the condition after the first test:"),
                                verbatimTextOutput("result_test1"),
                                h4("Probability of having the condition after the second test:"),
                                verbatimTextOutput("result_test2"),
                                h4("Probability of having the condition after the third test:"),
                                verbatimTextOutput("result_test3")
                                )
                        ),
                ),
                
                tabPanel(
                        "FAQ",
                        h4("Question: What is sensitivity and specificity?"),
                        p("Answer: Sensitivity is the propability that someone with the disease will test positive. Specificity is the chance that someone without the disease will test negative."),
                        p("The probability of a false negative (someone with the disease testing negative) is 1 - sensitivity while the probability of a false positive (someone without the disease testing positive) is 1 - specificity."),
                        h4("Question: The probability of having the disease given a positive test result seems low considering how accurate the test is."),
                        p("Answer: If a disease is rare enough in the population, most of the positive results will actually be false positives, hence the low probability of actually having the disease despite getting a positive test result. Think of it like this:"), 
                        p("A disease is present in 1 out of 1,000 people. If we have a population of 100,000 people, that means 100 of them will have the disease and 99,900 will not. We have a test with a 99% sensitivity and 90% specificity. If we test all 100,000, 99 of the ones that have the disease will test positive (true positives). One person with the disease will test negative (false negative). So far, so good."),
                        p("The issue comes when we talk about the people who don't have the disease. With 90% specificity, we expect 89,910 out of the 99,900 negatives to test negative (true negatives) and 9,990 to test positive (false positives)."),
                        p("Therefore, out of the 10,089 people in the population who test positive, only 99 actually have the disease, *despite having a highly accurate test*. That gives a probability of actually having the disease *even with a positive test* of [99 / (99 + 9,990)] * 100 = 0.98%."),
                        p("That is one of the reasons doctors order follow up tests as each positive test decreases the chances that the results are a false positive.")
                )
        ),
        h4("Created by: James Milks"),
        br(),
        "2024 May 13",
        br(),
        "Code available at:",
        a(href = "https://github.com/jrmilks74/Bayesian_probability_calculator", "https://github.com/jrmilks74/Bayesian_probability_calculator")
)

# Define server logic
server <- function(input, output) {
        
        observeEvent(input$calculate, {
                prior_prob <- input$cases / input$population
                
                sensitivity_test1 <- input$sensitivity_test1 / 100
                specificity_test1 <- input$specificity_test1 / 100
                sensitivity_test2 <- input$sensitivity_test2 / 100
                specificity_test2 <- input$specificity_test2 / 100
                sensitivity_test3 <- input$sensitivity_test3 / 100
                specificity_test3 <- input$specificity_test3 / 100
                
                # Determine test results
                test1_result <- input$test1_result
                test2_result <- input$test2_result
                test3_result <- input$test3_result
                
                # Convert radio button selections to "Positive" or "Negative"
                test1_result <- ifelse(test1_result == "Positive", "Positive", "Negative")
                test2_result <- ifelse(test2_result == "Positive", "Positive", "Negative")
                test3_result <- ifelse(test3_result == "Positive", "Positive", "Negative")
                
                # Calculate posterior probability after each test
                posterior_test1 <- if (test1_result == "Positive") {
                        (prior_prob * sensitivity_test1) / ((prior_prob * sensitivity_test1) + ((1 - prior_prob) * (1 - specificity_test1)))
                } else {
                        (prior_prob * (1 - sensitivity_test1)) / ((prior_prob * (1 - sensitivity_test1)) + ((1 - prior_prob) * specificity_test1))
                }
                
                posterior_test2 <- if (test2_result == "Positive") {
                        (posterior_test1 * sensitivity_test2) / ((posterior_test1 * sensitivity_test2) + ((1 - posterior_test1) * (1 - specificity_test2)))
                } else {
                        (posterior_test1 * (1 - sensitivity_test2)) / ((posterior_test1 * (1 - sensitivity_test2)) + ((1 - posterior_test1) * specificity_test2))
                }
                
                posterior_test3 <- if (test3_result == "Positive") {
                        (posterior_test2 * sensitivity_test3) / ((posterior_test2 * sensitivity_test3) + ((1 - posterior_test2) * (1 - specificity_test3)))
                } else {
                        (posterior_test2 * (1 - sensitivity_test3)) / ((posterior_test2 * (1 - sensitivity_test3)) + ((1 - posterior_test2) * specificity_test3))
                }
                
                # Output posterior probabilities after each test
                output$result_test1 <- renderPrint({
                        paste("Probability: ", round(posterior_test1 * 100, 1), "%", sep = "")
                })
                
                output$result_test2 <- renderPrint({
                        paste("Probability: ", round(posterior_test2 * 100, 1), "%", sep = "")
                })
                
                output$result_test3 <- renderPrint({
                        paste("Probability: ", round(posterior_test3 * 100, 1), "%", sep = "")
                })
                
        })
}

# Run the application
shinyApp(ui = ui, server = server)
