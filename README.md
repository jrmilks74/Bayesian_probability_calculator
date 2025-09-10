# Bayesian Probability Calculator for Medical Tests

This Shiny app calculates the probability of actually having a condition after one or more diagnostic tests, using **Bayes’ Theorem**.  

It allows you to input:
- The **population prevalence** (cases per population).
- The **sensitivity** and **specificity** of up to three tests.
- The **observed result** of each test (Positive/Negative).

The app then shows:
- The **posterior probability** of having the disease after each test.
- The **overall false positive probability** (if the observed result is positive).
- The **overall false negative probability** (if the observed result is negative).

---

## Features

- **Sequential testing**: Each posterior becomes the prior for the next test.
- **Adjustable accuracy**: Change sensitivity and specificity for each test.
- **Interactive results**: Select test outcomes (positive/negative) and calculate instantly.
- **Tabular output**: Displays priors, posteriors, test accuracies, and false-positive/false-negative risks in one table.
- **FAQ tab**: Explains the intuition behind Bayes’ theorem in medical testing.

---

## Why This Matters

Even highly accurate tests can produce misleading results when a disease is rare.  
For example, a test with **99% sensitivity** and **90% specificity** applied to a disease affecting **1 in 1,000 people** will result in most positive test results being **false positives**.  

This is why follow-up tests are so important—each additional test recalculates the probability and reduces the chance of error.

---

## Installation

1. Install R (≥ 4.0.0) and [RStudio](https://posit.co/download/rstudio-desktop/) (optional but recommended).
2. Install required packages:

```r
install.packages(c("shiny", "dplyr", "scales"))
