library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(caTools)
library(Metrics)

# Load your dataset (replace with the actual file path)
crime_data <- read_excel("rproject.xlsx")  # Update with actual file path

# Ensure necessary columns are appropriately formatted
crime_data$Date <- as.Date(crime_data$Year, format = "%Y")
crime_data$Subgroup <- as.factor(crime_data$Subgroup)

# Train-test split for ML model
data <- read_excel("rproject.xlsx")  # Replace with actual file path

# Convert necessary columns
data$Year <- as.factor(data$Year)
data$Subgroup <- as.factor(data$Subgroup)

set.seed(123)
split <- sample.split(data$Rape_Cases_Reported, SplitRatio = 0.7)
trainingset <- subset(data, split == TRUE)
testset <- subset(data, split == FALSE)

# Train the linear regression model
lm_model <- lm(Rape_Cases_Reported ~ Year + Subgroup, data = trainingset)

# Predictions for training and test sets
train_predictions <- predict(lm_model, newdata = trainingset)
test_predictions <- predict(lm_model, newdata = testset)

# Calculate accuracy percentage
calculate_accuracy <- function(actual, predicted) {
  # Convert predictions to same scale as actual values
  predicted_adjusted <- pmax(0, predicted)  # Ensure no negative predictions
  
  # Calculate percentage error
  error_percentage <- abs(actual - predicted_adjusted) / actual
  
  # Calculate accuracy percentage (100 - mean percentage error)
  accuracy <- 100 * (1 - mean(error_percentage, na.rm = TRUE))
  
  return(round(accuracy, 1))
}

# Calculate accuracy for both training and test sets
train_accuracy <- calculate_accuracy(trainingset$Rape_Cases_Reported, train_predictions)
test_accuracy <- calculate_accuracy(testset$Rape_Cases_Reported, test_predictions)

# Define UI with improved layout and styling
ui <- fluidPage(
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #2C3E50;
        padding: 15px;
        margin-bottom: 20px;
      }
      .navbar h1 {
        color: white;
        margin: 0;
        padding: 10px;
        text-align: center;
      }
      .nav-buttons {
        text-align: center;
        margin-bottom: 20px;
      }
      .action-button {
        background-color: #3498DB;
        color: white;
        border: none;
        padding: 10px 20px;
        margin: 5px;
        border-radius: 5px;
        transition: background-color 0.3s;
      }
      .action-button:hover {
        background-color: #2980B9;
      }
      .filter-panel {
        background-color: #F8F9FA;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .filter-label {
        font-weight: bold;
        margin-top: 15px;
        color: #2C3E50;
      }
      .graph-container {
        background-color: white;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .main-content {
        padding: 20px;
      }
    "))
  ),
  
  # Navbar with title
  div(class = "navbar",
      h1("Rajasthan Crime Rate Dashboard")
  ),
  
  # Navigation buttons
  div(class = "nav-buttons",
      actionButton("go_dashboard", "Dashboard", class = "action-button"),
      actionButton("go_mlmodel", "ML Model", class = "action-button")
  ),
  
  # Main content
  div(class = "main-content",
      uiOutput("pageContent")
  )
)

# Define server logic
server <- function(input, output, session) {
  current_page <- reactiveVal("dashboard")
  
  # Reactive filtered data based on input
  filtered_data <- reactive({
    req(input$apply_filters)
    crime_data %>%
      filter(Subgroup == input$crime_type,
             as.numeric(Year) >= input$year_range[1],
             as.numeric(Year) <= input$year_range[2])
  })
  
  output$pageContent <- renderUI({
    if (current_page() == "dashboard") {
      fluidRow(
        # Graphs on the left (9 columns)
        column(9,
               fluidRow(
                 column(6, div(class = "graph-container", plotOutput("pieChart", height = "300px"))),
                 column(6, div(class = "graph-container", plotOutput("lineChart", height = "300px")))
               ),
               fluidRow(
                 column(6, div(class = "graph-container", plotOutput("histogram", height = "300px"))),
                 column(6, div(class = "graph-container", plotOutput("scatterPlot", height = "300px")))
               ),
               fluidRow(
                 column(6, div(class = "graph-container", plotOutput("barChart", height = "300px"))),
                 column(6, div(class = "graph-container", plotOutput("heatmap", height = "300px")))
               )
        ),
        
        # Filters on the right (3 columns)
        column(3,
               div(class = "filter-panel",
                   tags$label(class = "filter-label", "Select Subgroup"),
                   selectInput("crime_type", NULL,
                               choices = unique(crime_data$Subgroup),
                               selected = unique(crime_data$Subgroup)[1],
                               width = "100%"),
                   
                   tags$label(class = "filter-label", "Filter by Year"),
                   sliderInput("year_range", NULL,
                               min = min(as.numeric(crime_data$Year), na.rm = TRUE),
                               max = max(as.numeric(crime_data$Year), na.rm = TRUE),
                               value = c(min(as.numeric(crime_data$Year), na.rm = TRUE),
                                         max(as.numeric(crime_data$Year), na.rm = TRUE)),
                               width = "100%"),
                   
                   br(),
                   actionButton("apply_filters", "Apply Filters",
                                class = "action-button",
                                style = "width: 100%")
               )
        )
      )
    } else if (current_page() == "mlmodel") {
      fluidRow(
        column(12,
               div(class = "graph-container",
                   h3("Machine Learning Model Performance", 
                      style = "color: #2C3E50; margin-bottom: 20px; text-align: center;"),
                   
                   # Accuracy cards
                   fluidRow(
                     column(6,
                            div(style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; text-align: center; margin: 10px;",
                                h2(style = "color: #2980B9; margin: 0;", paste0(train_accuracy, "%")),
                                h4(style = "color: #7F8C8D; margin-top: 10px;", "Training Accuracy")
                            )
                     ),
                     column(6,
                            div(style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; text-align: center; margin: 10px;",
                                h2(style = "color: #27AE60; margin: 0;", paste0(test_accuracy, "%")),
                                h4(style = "color: #7F8C8D; margin-top: 10px;", "Testing Accuracy")
                            )
                     )
                   ),
                   
                   br(),
                   h4("Model Predictions vs Actual Values", 
                      style = "color: #2C3E50; margin: 20px 0; text-align: center;"),
                   fluidRow(
                     column(6, plotOutput("trainPlot", height = "400px")),
                     column(6, plotOutput("testPlot", height = "400px"))
                   )
               )
        )
      )
    }
  })
  
  # Navigation event handlers
  observeEvent(input$go_dashboard, {
    current_page("dashboard")
  })
  
  observeEvent(input$go_mlmodel, {
    current_page("mlmodel")
  })
  
  # Plot renderings
  # 1. Pie Chart
  output$pieChart <- renderPlot({
    pie_data <- crime_data %>%
      group_by(Subgroup) %>%
      summarise(Count = n())
    
    ggplot(pie_data, aes(x = "", y = Count, fill = Subgroup)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Subgroup Distribution") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
  })
  
  # 2. Line Chart
  output$lineChart <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Rape_Cases_Reported, group = Subgroup, color = Subgroup)) +
      geom_line() +
      labs(title = "Rape Cases Over Time", x = "Year", y = "Rape Cases Reported") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
  })
  
  # 3. Histogram
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = Rape_Cases_Reported)) +
      geom_histogram(binwidth = 10, fill = "#3498DB", color = "white") +
      labs(title = "Rape Cases Distribution", x = "Rape Cases Reported", y = "Frequency") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
  })
  
  # 4. Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Rape_Cases_Reported, color = Subgroup)) +
      geom_point(size = 3) +
      labs(title = "Rape Cases vs Year", x = "Year", y = "Rape Cases Reported") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
  })
  
  # 5. Bar Chart
  output$barChart <- renderPlot({
    bar_data <- crime_data %>%
      group_by(Subgroup) %>%
      summarise(Count = n(), .groups = 'drop')
    
    ggplot(bar_data, aes(x = Subgroup, y = Count, fill = Subgroup)) +
      geom_bar(stat = "identity") +
      labs(title = "Crime Cases by Subgroup", x = "Subgroup", y = "Count") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 6. Heatmap
  output$heatmap <- renderPlot({
    heat_data <- crime_data %>%
      group_by(Year, Subgroup) %>%
      summarise(Count = n(), .groups = 'drop')
    
    ggplot(heat_data, aes(x = Year, y = Subgroup, fill = Count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#3498DB") +
      labs(title = "Heatmap of Crime Cases by Year and Subgroup") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 7. Training Plot
  output$trainPlot <- renderPlot({
    ggplot() +
      geom_point(aes(x = trainingset$Rape_Cases_Reported, y = train_predictions),
                 color = '#3498DB', alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, color = '#E74C3C', size = 1) +
      ggtitle("Training Set: Actual vs Predicted") +
      xlab("Actual Rape Cases Reported") +
      ylab("Predicted Rape Cases Reported") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
  })
  
  # 8. Test Plot
  output$testPlot <- renderPlot({
    ggplot() +
      geom_point(aes(x = testset$Rape_Cases_Reported, y = test_predictions),
                 color = '#2ECC71', alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, color = '#E74C3C', size = 1) +
      ggtitle("Test Set: Actual vs Predicted") +
      xlab("Actual Rape Cases Reported") +
      ylab("Predicted Rape Cases Reported") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)