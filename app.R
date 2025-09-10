# Bayesian Probability Calculator for Medical Tests
# Cleaned & improved: iterates tests, shows posterior after each step,
# and computes overall false-positive / false-negative given the observed result.

library(shiny)
library(dplyr)
library(scales)

# ---------- Helpers ----------
clamp01 <- function(x) pmin(pmax(x, 0), 1)

as_pct <- function(x, digits = 1) percent(x, accuracy = 10^(-digits))

# Given prior (P[D]), sensitivity (Se), specificity (Sp), and a result ("Positive"/"Negative"),
# return a list with posterior P[D|result], and the sequence-level FP/FN probabilities.
update_posterior <- function(prior, Se, Sp, result) {
  prior <- clamp01(prior); Se <- clamp01(Se); Sp <- clamp01(Sp)
  if (result == "Positive") {
    # Bayes: P(D|+) = Se*P(D) / [Se*P(D) + (1-Sp)*P(~D)]
    num <- Se * prior
    den <- Se * prior + (1 - Sp) * (1 - prior)
    post <- ifelse(den > 0, num / den, NA_real_)
    list(
      posterior = post,
      overall_fp = 1 - post,   # probability it's a false positive given a positive result
      overall_fn = NA_real_    # not applicable for a positive result
    )
  } else {
    # Bayes: P(D|-) = (1-Se)*P(D) / [(1-Se)*P(D) + Sp*P(~D)]
    num <- (1 - Se) * prior
    den <- (1 - Se) * prior + Sp * (1 - prior)
    post <- ifelse(den > 0, num / den, NA_real_)
    list(
      posterior = post,
      overall_fp = NA_real_,    # not applicable for a negative result
      overall_fn = post         # probability it's a false negative given a negative result
    )
  }
}

ui <- fluidPage(
  titlePanel("Bayesian Probability Calculator for Medical Tests"),
  tabsetPanel(
    tabPanel(
      "Calculator",
      sidebarLayout(
        sidebarPanel(
          h5("Prior probability: Population rate"),
          numericInput("cases", "Cases", value = 25, min = 0, step = 1),
          numericInput("population", "per population", value = 1000, min = 1, step = 1),
          h5("Test Sensitivity/Specificity (%)"),
          numericInput("sensitivity_test1", "Sensitivity (Test 1)", value = 95, min = 0, max = 100),
          numericInput("specificity_test1", "Specificity (Test 1)", value = 85, min = 0, max = 100),
          numericInput("sensitivity_test2", "Sensitivity (Test 2)", value = 95, min = 0, max = 100),
          numericInput("specificity_test2", "Specificity (Test 2)", value = 85, min = 0, max = 100),
          numericInput("sensitivity_test3", "Sensitivity (Test 3)", value = 95, min = 0, max = 100),
          numericInput("specificity_test3", "Specificity (Test 3)", value = 85, min = 0, max = 100),

          h5("Test Results"),
          radioButtons("test1_result", "Test 1 Result",
                       choices = c("Negative", "Positive"), selected = "Negative"),
          radioButtons("test2_result", "Test 2 Result",
                       choices = c("Negative", "Positive"), selected = "Negative"),
          radioButtons("test3_result", "Test 3 Result",
                       choices = c("Negative", "Positive"), selected = "Negative"),

          actionButton("calculate", "Calculate")
        ),
        mainPanel(
          h3("Results"),
          tableOutput("results_table"),
          tags$hr(),
          h4("Posterior (probability of having the condition) after each test"),
          h5("After Test 1"),
          verbatimTextOutput("result_test1"),
          h5("After Test 2"),
          verbatimTextOutput("result_test2"),
          h5("After Test 3"),
          verbatimTextOutput("result_test3"),
          tags$hr(),
          tags$small(
            em("Notes:"),
            tags$ul(
              tags$li("Overall false positive after a positive result equals 1 − posterior."),
              tags$li("Overall false negative after a negative result equals posterior."),
              tags$li("If a row shows N/A for FP or FN, it’s not applicable to that result sign.")
            )
          )
        )
      )
    ),
    tabPanel(
      "FAQ",
      h4("What are sensitivity and specificity?"),
      p("Sensitivity is the probability that someone with the disease will test positive. Specificity is the probability that someone without the disease will test negative."),
      p("The probability of a false negative (someone with the disease testing negative) is 1 − sensitivity; the probability of a false positive (someone without the disease testing positive) is 1 − specificity."),
      h4("Why can P(disease | positive) be low even with accurate tests?"),
      p("When a disease is rare, even a specific test can generate many false positives out of the large disease-free group. Bayes’ rule combines the prior (prevalence) with test accuracy to yield the posterior probability after a result."),
      h4("Sequential testing"),
      p("Sequential tests update the prior to the posterior from the previous step. After each result, we recompute the new probability of disease and the overall probability that the observed result is a false positive (for a positive) or a false negative (for a negative).")
    )
  ),
  h4("Created by: James Milks"),
  br(),
  "2024 May 13",
  br(),
  "Code available at: ",
  a(href = "https://github.com/jrmilks74/Bayesian_probability_calculator",
    "https://github.com/jrmilks74/Bayesian_probability_calculator")
)

server <- function(input, output, session) {

  observeEvent(input$calculate, {
    # Validate basic inputs
    pop <- max(1, as.integer(input$population))
    cases <- max(0, as.integer(input$cases))
    prior <- clamp01(cases / pop)

    tests <- tibble::tibble(
      test = paste0("Test ", 1:3),
      result = c(input$test1_result, input$test2_result, input$test3_result),
      sens = clamp01(c(input$sensitivity_test1,
                       input$sensitivity_test2,
                       input$sensitivity_test3) / 100),
      spec = clamp01(c(input$specificity_test1,
                       input$specificity_test2,
                       input$specificity_test3) / 100)
    )

    # Iterate tests, updating posterior each time
    priors <- numeric(nrow(tests))
    posteriors <- numeric(nrow(tests))
    overall_fp <- overall_fn <- rep(NA_real_, nrow(tests))

    current_prior <- prior
    for (i in seq_len(nrow(tests))) {
      priors[i] <- current_prior
      upd <- update_posterior(
        prior = current_prior,
        Se = tests$sens[i],
        Sp = tests$spec[i],
        result = tests$result[i]
      )
      posteriors[i] <- upd$posterior
      overall_fp[i] <- upd$overall_fp
      overall_fn[i] <- upd$overall_fn
      current_prior <- posteriors[i]
    }

    results_tbl <- tests %>%
      mutate(
        `Prior (before test)` = as_pct(priors),
        Sensitivity = as_pct(sens),
        Specificity = as_pct(spec),
        `Observed result` = result,
        `Posterior P(disease)` = as_pct(posteriors),
        `Overall False Positive` = ifelse(is.na(overall_fp), "N/A", as_pct(overall_fp)),
        `Overall False Negative` = ifelse(is.na(overall_fn), "N/A", as_pct(overall_fn))
      ) %>%
      select(
        Test = test,
        `Observed result`,
        `Prior (before test)`,
        Sensitivity,
        Specificity,
        `Posterior P(disease)`,
        `Overall False Positive`,
        `Overall False Negative`
      )

    output$results_table <- renderTable({
      results_tbl
    }, striped = TRUE, hover = TRUE, spacing = "s")

    # Keep your original quick-read outputs
    output$result_test1 <- renderPrint({
      paste0("Probability: ", as_pct(posteriors[1]))
    })
    output$result_test2 <- renderPrint({
      paste0("Probability: ", as_pct(posteriors[2]))
    })
    output$result_test3 <- renderPrint({
      paste0("Probability: ", as_pct(posteriors[3]))
    })
  })
}

shinyApp(ui = ui, server = server)
