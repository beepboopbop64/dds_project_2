library(shiny)
library(class)
library(tidyverse)
library(smotefamily)


ui <- fluidPage(
  titlePanel("Attrition KNN Predictor"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File for Training Data",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      actionButton("train", "Train KNN Model"),
      sliderInput("job_type_risk", "Job Risk Type:", min = 0, max = 15, value = 15),
      sliderInput("survey_results", "Survey Results:", min = 1, max =15, value = 15),
      sliderInput("worker_type", "Worker Type:", min = 1, max = 15, value = 15),
      sliderInput("career_type", "Career Type:", min = 1, max = 15, value = 15)
    ),
    mainPanel(
      h4("Predicted Attrition:"),
      verbatimTextOutput("model"),
      tableOutput("data")
    )
  )
)

server <- function(input, output) {
  # use the input$file1 object to access the uploaded file
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    #############
    # Load Data #
    #############
    
    # Read in data set
    attrition_raw_data = df
    
    # drop columns with only one value as they are not adding any value
    attrition_clean_data <- attrition_raw_data[vapply(attrition_raw_data, function(x) length(unique(x)) > 1, logical(1L))]
    
    
    #######################
    # Feature Engineering #
    #######################
    
    # Create a data set with some feature engineering based on what we have observed.
    attrition_feature_enginering <- attrition_clean_data %>%
      # Job Type Risk
      mutate(job_type_risk = case_when(JobRole %in% 
                                         c("Healthcare Representative",
                                           "Laboratory Technician",
                                           "Research Scientist",
                                           "Sales Executive",
                                           "Sales Representative") ~ "High Risk",
                                       TRUE ~ "Low Risk")
      ) %>%
      mutate_at(vars(job_type_risk), as.factor) %>%
      # Survey Results
      mutate(survey_results = 
               JobInvolvement + JobSatisfaction + WorkLifeBalance) %>%
      # Worker Type
      mutate(worker_type = case_when(OverTime=='Yes' ~ 1,
                                     OverTime=='No' ~ 5) + 
               case_when(MaritalStatus=='Married' ~ 5,
                         MaritalStatus=='Divorced' ~ 3,
                         MaritalStatus=='Single' ~ 1) +
               JobLevel) %>%
      # Career Type
      # Create YearsPerCompany
      mutate(TotalWorkingYears = if_else(TotalWorkingYears=='0', 1,
                                         as.double(TotalWorkingYears))) %>%
      mutate(NumCompaniesWorked = NumCompaniesWorked + 1) %>% # assumption
      mutate(YearsPerCompany = as.integer(TotalWorkingYears / NumCompaniesWorked)) %>%
      # Create career_type
      mutate(career_type = case_when(YearsPerCompany==0 ~ 1,
                                     YearsPerCompany==1 ~ 2,
                                     YearsPerCompany==2 ~ 3,
                                     YearsPerCompany==3 ~ 4,
                                     TRUE ~ 5) +
               case_when(YearsInCurrentRole==0 ~ 1,
                         YearsInCurrentRole==2 ~ 2,
                         YearsInCurrentRole==7 ~ 3,
                         YearsInCurrentRole %in% c(1, 3, 4) ~ 4,
                         TRUE ~ 5) +
               case_when(YearsSinceLastPromotion==0 ~ 1,
                         YearsSinceLastPromotion==1 ~ 2,
                         YearsSinceLastPromotion==2 ~ 3,
                         YearsSinceLastPromotion==7 ~ 4,
                         TRUE ~ 5)
      ) %>%
      
      select(Attrition, job_type_risk, survey_results, worker_type, career_type) 
    
    # Create factor variables
    attrition_factors_data_with_enginering <- attrition_feature_enginering
    attrition_factors_data_with_enginering[sapply(attrition_factors_data_with_enginering, is.character)] <- 
      lapply(attrition_clean_data[sapply(attrition_clean_data, is.character)], as.factor)
    
    data_for_model <- attrition_factors_data_with_enginering
    
  })
  
  # Outputs data on screen in a table for validation
  output$data <- renderTable({
    data()
  })
  
  # Creates a KNN model to predict attrition based on findings in project as well as whatever data is uploaded
  knn_model <- reactive({
    req(input$train)
    df <- data()
    df <- df %>%
      mutate(job_type_risk = case_when(job_type_risk=="High Risk" ~ 0,
                                       job_type_risk=="Low Risk" ~ 15))
    trainIndices = sample(1:dim(df)[1], round(.7 * dim(df)[1]))
    train = df[trainIndices, ]
    test = data.frame(input$job_type_risk, input$survey_results, 
                      input$worker_type, input$career_type)
    knn(train[,2:5], 
        test, 
        train$Attrition, 
        k = 3, 
        prob = TRUE
    )
  })
  
  # Will output the predicted results
  output$model <- renderPrint({
    if (!is.null(input$train)) {
      knn_model()
    }
  })
}

shinyApp(ui, server)
