# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)  # For interactive tables
library(shinythemes)  # For better UI

# Load dataset
df <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)

# Selecting only relevant columns
df <- df %>% select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, 
                    Platform, WebFramework, Industry, AISearch, AITool, Employment)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Apply a clean UI theme
  
  titlePanel("Eduvos Graduate Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tech_category", "Select Category:", 
                  choices = c("Programming Languages" = "ProgLang", 
                              "Databases" = "Databases", 
                              "Web Frameworks" = "WebFramework")),
      
      selectInput("campus_filter", "Select Campus:", 
                  choices = unique(df$Campus), 
                  selected = unique(df$Campus)[1]),
      
      selectInput("employment_filter", "Filter by Employment:", 
                  choices = c("All", unique(df$Employment)), 
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("techPlot", height = "600px")),
        tabPanel("Pie Chart", plotOutput("techPie", height = "600px")),
        tabPanel("Data Table", DT::dataTableOutput("summaryTable"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive function to filter data based on campus and employment
  filtered_data <- reactive({
    req(input$tech_category, input$campus_filter)  # Ensure inputs are available
    data <- df %>% filter(Campus == input$campus_filter)
    
    if (input$employment_filter != "All") {
      data <- data %>% filter(Employment == input$employment_filter)
    }
    
    data %>% select(all_of(input$tech_category))
  })
  
  # Bar Chart Visualization
  output$techPlot <- renderPlot({
    req(filtered_data())
    
    tech_count <- filtered_data() %>%
      mutate(value = strsplit(as.character(get(input$tech_category)), ";")) %>% 
      unnest(value) %>%
      group_by(value) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    ggplot(tech_count, aes(x = reorder(value, count), y = count)) +
      geom_col(fill = "darkgreen") +  
      coord_flip() +  
      labs(title = paste("Usage of", input$tech_category, "at", input$campus_filter),
           x = "Technology", y = "Count") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  })
  
  # Pie Chart Visualization
  output$techPie <- renderPlot({
    req(filtered_data())
    
    tech_count <- filtered_data() %>%
      mutate(value = strsplit(as.character(get(input$tech_category)), ";")) %>%
      unnest(value) %>%
      group_by(value) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    ggplot(tech_count, aes(x = "", y = count, fill = value)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = paste("Distribution of", input$tech_category)) +
      theme_minimal()
  })
  
  # Summary Table
  output$summaryTable <- DT::renderDataTable({
    req(filtered_data())  
    
    tech_count <- filtered_data() %>%
      mutate(value = strsplit(as.character(get(input$tech_category)), ";")) %>% 
      unnest(value) %>%
      group_by(value) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
    
    tech_count  
  }, options = list(pageLength = 10))  # Paginate table for better readability
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
