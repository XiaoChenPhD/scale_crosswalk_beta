# A Shiny App to link HRSD and MADRS
# Users can either provide sum score or item score
# Upload a .csv file with the first row as the header. No requirement for header itself.
# sum score: one column of HRSD scores or MADRS socres
# item score: 17 columns of HRSD 1-17 item scores or 10 columns of MADRS 1-10 item score
# models trained at different time points are also provided
# baseline: score before treatments started
# 30 d: score after receiving 30 days of treatment
# follow up 1-3: scores at different follow-up time points after finishing the treatment
# delta: baseline score minus score at T30
#
# 240619
#
# remove all the negative values and round the predicted value to make them more realistic
# 241214
#
# Xiao Chen
# chenxiaophd@gmail.com

library(shiny)
library(openxlsx)
library(readr)
library(dplyr)
library(e1071)
library(equate)
library(randomForest)


# Define UI for the application
ui <- fluidPage(
  titlePanel("Linking HRSD and MADRS"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Sum Score",
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                 tags$hr(),
                 downloadButton("downloadSample1", "Download Sample CSV Files"),
                 tags$hr(),
                 radioButtons("predictor1", "Select Input Variable:", choices = c("HRSD to MADRS", "MADRS to HRSD")),
                 radioButtons("model_type1", "Select Model Type:", 
                              choices = list("baseline" = 1, 
                                             "30 d" = 2, 
                                             "follow-up 1" = 3, 
                                             "follow-up 2" = 4, 
                                             "follow-up 3" = 5, 
                                             "delta" = 6)),
                 selectInput("models1", "Select Model:", choices = NULL),
                 actionButton("runModel1", "Run Model"),
                 downloadButton("downloadData1", "Download Predictions")
        ),
        tabPanel("Item Scores",
                 fileInput("file2", "Choose CSV File with Item Scores",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                 tags$hr(),
                 downloadButton("downloadSample2", "Download Sample CSV Files"),
                 tags$hr(),
                 radioButtons("predictor2", "Select Input Variable:", choices = c("HRSD to MADRS", "MADRS to HRSD")),
                 radioButtons("model_type2", "Select Model Type:", 
                              choices = list("baseline" = 1, 
                                             "30 d" = 2, 
                                             "follow-up 1" = 3, 
                                             "follow-up 2" = 4, 
                                             "follow-up 3" = 5, 
                                             "delta" = 6)),
                 selectInput("models2", "Select Model:", 
                             choices = c("Linear Regression", "Random Forest Regression", "SVR")),
                 actionButton("runModel2", "Run Model"),
                 downloadButton("downloadData2", "Download Predictions")
        )
      )
    ),
    mainPanel(
      tableOutput("contents"),
      verbatimTextOutput("summary"),
      tableOutput("predictions")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # provide different models according to models trained at different time points
  observeEvent(input$model_type1, {
    updateSelectInput(session, "models1",
                      choices = if (input$model_type1 == 1 || input$model_type1 == 6) {
                        c("Pharmacotherapy Equipercentile Model", "rTMS Equipercentile Model", "Linear Regression", "Random Forest Regression", "SVR")
                      } else {
                        c("rTMS Equipercentile Model", "Linear Regression", "Random Forest Regression", "SVR")
                      })
  })
  
  
  # Reactive expression to read the uploaded file for sum scores
  dataset1 <- reactive({
    req(input$file1)
    df <- read_csv(input$file1$datapath)
    return(df)
  })
  
  # Reactive expression to read the uploaded file for item scores
  dataset2 <- reactive({
    req(input$file2)
    df <- read_csv(input$file2$datapath)
    return(df)
  })
  
  # Display the uploaded file content
  output$contents <- renderTable({
    if (!is.null(input$file1)) {
      dataset1()
    } else if (!is.null(input$file2)) {
      dataset2()
    }
  })
  
  
  # Reactive expression to run the selected model for sum scores
  model1 <- reactive({
    req(input$runModel1)
    df <- dataset1()
    predictor_data <- df[[1]]
    
    # Load pre-trained model and data
    flag <- input$model_type1
    if (flag == 6){
      conversion_table <- read.xlsx(file.path("data", "coversion_table_delta_Leucht2018.xlsx"))
    } else {
      conversion_table <- read.xlsx(file.path("data", "coversion_table_Leucht2018.xlsx"))
    }
    if (input$predictor1 == "HRSD to MADRS") {
      load(file.path("data", paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
    } else if (input$predictor1 == "MADRS to HRSD") {
      load(file.path("data", paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
    }
    
    # use different models to predict
    if (input$predictor1 == "HRSD to MADRS") {
      if (input$models1 == "Pharmacotherapy Equipercentile Model") {
        responses <- sapply(predictor_data, function(value) {
          if (flag == 6){
            if (value < -5) {
              return(-8)
            } else if (value > 27){
              return(37)
            } else {
              match_value <- match(value, conversion_table$HAMD)
              return(conversion_table$MADRS[match_value])
            }
          } else{
            if (value > 40){
              return(53)
            } else if (value < 0){
              return(0)
            }else {
              match_value <- match(value, conversion_table$HAMD)
              return(conversion_table$MADRS[match_value])
            }
          }
        })
      } else if (input$models1 == "rTMS Equipercentile Model") {
        # Placeholder for Percentile model
        responses <- equate(predictor_data, y = equating_result)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 60] <- 60
        if (flag == 6){
          responses[responses < -60] <- -60
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
     } else if (input$models1 == "Linear Regression") {
       df_predictor <- data.frame(hrsd_total = predictor_data)
       responses <- predict(model_lm, newdata = df_predictor)
       # round the predicted values and make sure they are in the meaningful ranges
       responses <- round(responses, digits = 0)
       responses[responses > 60] <- 60
       if (flag == 6){
         responses[responses < -60] <- -60
       } else {
         responses[responses < 0] <- sprintf("%.2f", 0)
       }
     } else if (input$models1 == "Random Forest Regression"){
       df_predictor <- data.frame(hrsd_total = predictor_data)
       responses <- predict(model_rf, newdata = df_predictor)
       # round the predicted values and make sure they are in the meaningful ranges
       responses <- round(responses, digits = 0)
       if (flag == 6){
         responses[responses < -60] <- -60
       } else {
         responses[responses < 0] <- sprintf("%.2f", 0)
       }
       responses[responses > 60] <- 60
     } else if (input$models1 == "SVR"){
       df_predictor <- data.frame(hrsd_total = predictor_data)
       responses <- predict(model_svm, newdata = df_predictor)
       # round the predicted values and make sure they are in the meaningful ranges
       responses <- round(responses, digits = 0)
       responses[responses > 60] <- 60
       if (flag == 6){
         responses[responses < -60] <- -60
       } else {
         responses[responses < 0] <- sprintf("%.2f", 0)
       }
     } ###################### MADRS to HAMD #############################
    } else if (input$predictor1 == "MADRS to HRSD") {
      if (input$models1 == "Pharmacotherapy Equipercentile Model") {
        responses <- sapply(predictor_data, function(value) {
          if (flag == 6){
            if(value < -8){
              return(-5)
            }else if (value > 37){
              return(27)
            }else{
              match_value <- match(value, conversion_table$MADRS)
              return(conversion_table$HAMD[match_value])
            }
          } else{
            if (value > 0 & value < 3){ # the original table omitted MADRS = 0-2
              return(2)
            } else if (value > 53){
              return(40)
            } else{
              match_value <- match(value, conversion_table$MADRS)
              return(conversion_table$HAMD[match_value])
            }
          }
        })
      } else if (input$models1 == "rTMS Equipercentile Model") {
        # Placeholder for Percentile model
        responses <- equate(predictor_data, y = equating_result)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 52] <- 52
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      } else if (input$models1 == "Linear Regression") {
        df_predictor <- data.frame(madrs_total = predictor_data)
        responses <- predict(model_lm, newdata = df_predictor)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 52] <- 52
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      } else if (input$models1 == "Random Forest Regression"){
        df_predictor <- data.frame(madrs_total = predictor_data)
        responses <- predict(model_rf, newdata = df_predictor)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 52] <- 52
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      } else if (input$models1 == "SVR"){
        df_predictor <- data.frame(hrsd_total = predictor_data)
        responses <- predict(model_svm, newdata = df_predictor)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 52] <- 52
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      }
    }
    df$predictions <- responses
    return(df)
  })
  
  # Reactive expression to run the selected model for item scores
  model2 <- reactive({
    req(input$runModel2)
    df <- dataset2()
    predictor_data <- df
    
    # load the model according to selection
    flag <- input$model_type2
    if (input$predictor2 == "HRSD to MADRS") {
      load(file.path("data", paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
    } else if (input$predictor2 == "MADRS to HRSD") {
      load(file.path("data", paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
    }
    
    if (input$predictor2 == "HRSD to MADRS") {
      names(predictor_data) <- c("hrsd1s", "hrsd2s", "hrsd3s", "hrsd4s", "hrsd5s", "hrsd6s", "hrsd7s",
                                 "hrsd8s", "hrsd9s", "hrsd10s", "hrsd11s", "hrsd12s", "hrsd13s", "hrsd14s",
                                 "hrsd15s", "hrsd16s", "hrsd17s")
      if (input$models2 == "Linear Regression") {
        responses <- predict(model_lm_item, newdata = predictor_data)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 60] <- 60
        if (flag == 6){
          responses[responses < -60] <- -60
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      } else if (input$models2 == "Random Forest Regression") {
        responses <- predict(model_rf_item, newdata = predictor_data)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 60] <- 60
        if (flag == 6){
          responses[responses < -60] <- -60
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      } else if (input$models2 == "SVR") {
        responses <- predict(model_svm_item, newdata = predictor_data)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 60] <- 60
        if (flag == 6){
          responses[responses < -60] <- -60
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      }
    } else if (input$predictor2 == "MADRS to HRSD") {
      names(predictor_data) <- c("madrs1", "madrs2", "madrs3", "madrs4", "madrs5",
                                 "madrs6", "madrs7", "madrs8", "madrs9", "madrs10")
      if (input$models2 == "Linear Regression") {
        responses <- predict(model_lm_item, newdata = predictor_data)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 52] <- 52
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      } else if (input$models2 == "Random Forest Regression") {
        responses <- predict(model_rf_item, newdata = predictor_data)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
        responses[responses > 52] <- 52
      } else if (input$models2 == "SVR") {
        responses <- predict(model_svm_item, newdata = predictor_data)
        # round the predicted values and make sure they are in the meaningful ranges
        responses <- round(responses, digits = 0)
        responses[responses > 52] <- 52
        if (flag == 6){
          responses[responses < -52] <- -52
        } else {
          responses[responses < 0] <- sprintf("%.2f", 0)
        }
      }
    }
    
    df$predictions <- responses
    return(df)
  })
  
  # Display the model predictions
  output$predictions <- renderTable({
    if (!is.null(input$file1)) {
      model1()
    } else if (!is.null(input$file2)) {
      model2()
    }
  })
  
  # Download handler for the predictions for sum scores
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("predictions", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model1(), file, row.names = FALSE)
    }
  )
  
  # Download handler for the predictions for item scores
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("predictions", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model2(), file, row.names = FALSE)
    }
  )
  
  # Download handler for the combined sample CSV files as a ZIP archive
  output$downloadSample1 <- downloadHandler(
    filename = function() {
      "sample_csv_files.zip"
    },
    content = function(file) {
      # Create a temporary directory to store the sample files
      tmpdir <- tempdir()
      # Define the paths to the sample files
      sample_HAMD_score <- file.path("data", "example_HRSD_sum.csv")
      sample_MADRS_score <- file.path("data", "example_MADRS_sum.csv")
      # Copy the sample files to the temporary directory
      file.copy(sample_HAMD_score, tmpdir)
      file.copy(sample_MADRS_score, tmpdir)
      # Create a vector of the file paths in the temporary directory
      files_to_zip <- c(file.path(tmpdir, "example_HRSD_sum.csv"),
                        file.path(tmpdir, "example_MADRS_sum.csv"))
      # Create the zip file
      zip::zipr(zipfile = file, files = files_to_zip, root = tmpdir)
    },
    contentType = "application/zip"
  )
  
  # Download handler for the combined sample CSV files as a ZIP archive
  output$downloadSample2 <- downloadHandler(
    filename = function() {
      "sample_csv_files.zip"
    },
    content = function(file) {
      # Create a temporary directory to store the sample files
      tmpdir <- tempdir()
      # Define the paths to the sample files
      sample_HAMD_score <- file.path("data", "example_HRSD_item.csv")
      sample_MADRS_score <- file.path("data", "example_MADRS_item.csv")
      # Copy the sample files to the temporary directory
      file.copy(sample_HAMD_score, tmpdir)
      file.copy(sample_MADRS_score, tmpdir)
      # Create a vector of the file paths in the temporary directory
      files_to_zip <- c(file.path(tmpdir, "example_HRSD_item.csv"),
                        file.path(tmpdir, "example_MADRS_item.csv"))
      # Create the zip file
      zip::zipr(zipfile = file, files = files_to_zip, root = tmpdir)
    },
    contentType = "application/zip"
  )
}

# Run the application
shinyApp(ui = ui, server = server)