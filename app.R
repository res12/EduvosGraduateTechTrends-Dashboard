library(shiny)
library(ggplot2)
library(dplyr)

# Load dataset
df <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)

# Selecting only relevant columns
df <- df %>% select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, 
                    Platform, WebFramework, Industry, AISearch, AITool, Employment)

# UI
ui <- fluidPage(
  titlePanel("Eduvos Graduate Survey Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tech_category", "Select Category:", 
                  choices = c("Programming Languages" = "ProgLang", 
                              "Databases" = "Databases", 
                              "Web Frameworks" = "WebFramework")),
      selectInput("campus_filter", "Select Campus:", 
                  choices = unique(df$Campus), 
                  selected = unique(df$Campus)[1])
    ),
    mainPanel(
      plotOutput("techPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    df %>%
      filter(Campus == input$campus_filter) %>%
      select(all_of(input$tech_category))
  })
  
  output$techPlot <- renderPlot({
    req(filtered_data())  # Ensure data is available
    
    tech_count <- filtered_data() %>%
      mutate(value = strsplit(as.character(get(input$tech_category)), ",")) %>%
      unnest(value) %>%
      group_by(value) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    ggplot(tech_count, aes(x = reorder(value, -count), y = count, fill = value)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Usage of", input$tech_category, "at", input$campus_filter),
           x = "Technology", y = "Count") +
      theme_minimal()
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      group_by(get(input$tech_category)) %>%
      summarise(Count = n())
  })
}

# Run App
shinyApp(ui = ui, server = server)
