library(shiny)
library(brms)
library(MASS)
library(dplyr)

# Example covariate levels based on the reduced models
levels_list <- list(
  taking_vitd = c("Yes", "No"),
  race = c("1", "2", "3", "4", "6", "7"),
  took_diet_meds = c("Yes", "No"),
  milk_consumption = c("Daily", "NonDaily"),
  education = c("College Educated", "Not College Educated")
)

race_categories <- c(
  "Mexican American" = "1",
  "Other Hispanic" = "2",
  "Non-Hispanic White" = "3",
  "Non-Hispanic Black" = "4",
  "Non-Hispanic Asian" = "6",
  "Other Race - Including Multi-Racial" = "7"
)

# Update the selectInput for race
ui <- fluidPage(
  titlePanel("Vitamin D Deficiency Prediction: Reduced Models"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Covariates"),
      selectInput("taking_vitd", "Taking Vitamin D Supplements", choices = levels_list$taking_vitd),
      selectInput("race", "Race/Ethnicity", choices = race_categories),  # Named list
      selectInput("took_diet_meds", "Took Diet Medications", choices = levels_list$took_diet_meds),
      selectInput("milk_consumption", "Milk Consumption", choices = levels_list$milk_consumption),
      selectInput("education", "Education Level", choices = levels_list$education),
      actionButton("predict", "Predict Vitamin D Deficiency")
    ),
    
    mainPanel(
      h4("Predictions"),
      tableOutput("results_table"),
      verbatimTextOutput("debug_output")  # For debugging purposes
    )
  )
)

server <- function(input, output, session) {
  # Helper function to generate new data for the frequentist model
  generate_new_ordinal_data <- reactive({
    data.frame(
      taking_vitd = factor(input$taking_vitd, levels = levels_list$taking_vitd),
      race = factor(input$race, levels = levels_list$race),
      took_diet_meds = factor(input$took_diet_meds, levels = levels_list$took_diet_meds)
    )
  })
  
  # Helper function to generate new data for the Bayesian model
  generate_new_bayes_data <- reactive({
    data.frame(
      taking_vitd = factor(input$taking_vitd, levels = levels_list$taking_vitd),
      race = factor(input$race, levels = levels_list$race),
      milk_consumption = factor(input$milk_consumption, levels = levels_list$milk_consumption),
      education = factor(input$education, levels = levels_list$education)
    )
  })
  
  # Perform predictions when the button is clicked
  predictions <- eventReactive(input$predict, {
    # Generate new data
    new_ordinal_data <- generate_new_ordinal_data()
    new_bayes_data <- generate_new_bayes_data()
    
    # Debug: Check new data
    cat("New Ordinal Data:\n")
    print(new_ordinal_data)
    cat("New Bayesian Data:\n")
    print(new_bayes_data)
    
    # Frequentist Model Predictions
    freq_probs <- tryCatch({
      predict(reduced_ordinal_model, newdata = new_ordinal_data, type = "probs")
    }, error = function(e) e)
    
    freq_class <- tryCatch({
      predict(reduced_ordinal_model, newdata = new_ordinal_data, type = "class")
    }, error = function(e) e)
    
    # Bayesian Model Predictions
    bayesian_probs <- tryCatch({
      posterior_epred(reduced_bayes_model, newdata = new_bayes_data)
    }, error = function(e) e)
    
    
    # Debug: Check Bayesian predictions
    print("Bayesian Probabilities Dimensions:")
    print(dim(bayesian_probs))
    
    # Compute mean probabilities across iterations
    bayesian_probs_mean <- apply(bayesian_probs, c(2, 3), mean)
    bayesian_class <- colnames(bayesian_probs_mean)[apply(bayesian_probs_mean, 1, which.max)]
    
    # Return predictions
    list(
      error = FALSE,
      freq_probs = freq_probs,
      freq_class = freq_class,
      bayesian_probs = bayesian_probs_mean,
      bayesian_class = bayesian_class
    )
  })
  
  # Display results
  output$results_table <- renderTable({
    preds <- predictions()
    
    # Handle errors in predictions
    if (preds$error) {
      return(data.frame(
        Error = preds$message
      ))
    }
    
    data.frame(
      Model = c("Frequentist", "Bayesian"),
      Predicted_Class = c(as.character(preds$freq_class), preds$bayesian_class),
      Sufficiency_Prob = c(preds$freq_probs[, "Sufficiency"], preds$bayesian_probs[1, 1]),
      Mild_Deficiency_Prob = c(preds$freq_probs[, "Mild Deficiency"], preds$bayesian_probs[1, 2]),
      Severe_Deficiency_Prob = c(preds$freq_probs[, "Severe Deficiency"], preds$bayesian_probs[1, 3])
    )
  }, rownames = FALSE)
  
  # Display debug information
  output$debug_output <- renderPrint({
    predictions()
  })
}

shinyApp(ui, server)

