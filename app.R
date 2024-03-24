# Load necessary libraries
library(shiny) # to build an interactive web dashboard application directly from R
library(readxl) # to read Excel file into R
library(dplyr) # for data manipulation
library(ggplot2) # for creating data visualizations
library(here) # to get data from current directory


# Load data
health_data <- read_excel(here::here("Health Sciences’ Middle Level Trainees from KMTC, 2018 - 2022.xlsx"), skip = 4)


#DATA CLEANING

# Set proper column names
colnames(health_data) <- c("Level of Training", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22*")

# Remove the first row (which was used as column names)
health_data <- health_data[-1, ]

# Remove the last 7 rows
health_data <- head(health_data, -7)

# Remove SubTotal and Total rows
health_data <- health_data[!grepl("Sub Total|Total", health_data$`Level of Training`), ]



# THE SHINY APP

# Define UI for the dashboard
ui <- fluidPage(
  titlePanel("Kenya Health Sciences Middle Level Trainees (2017/18 - 2021/22)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("level", "Select Level of Training:",
                  choices = c("Certificate", "Diploma", "Higher Diploma")),
      
      selectInput("year", "Select Year:",
                  choices = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22*"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", plotOutput("summary_plot", height = "500px")),  # Adjust height as needed
        tabPanel("Enrollment Per Year", dataTableOutput("year_table")),
        tabPanel("Enrollment for all years", dataTableOutput("details_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$summary_plot <- renderPlot({
    level_data <- switch(input$level,
                         "Certificate" = health_data[1:7, ],
                         "Diploma" = health_data[9:29, ],
                         "Higher Diploma" = health_data[31:46, ])
    year_col <- match(input$year, colnames(level_data))
    
    # Sort the data in descending order
    level_data <- level_data %>%
      arrange(desc(.[, year_col]))
    
    # Create a data frame for plotting
    data_df <- data.frame(
      Level_of_Training = level_data$`Level of Training`,
      Trainees = as.numeric(unlist(level_data[, year_col]))
    )
    
    # Create a bar plot
    ggplot(data_df, aes(x = reorder(Level_of_Training, -Trainees), y = Trainees, fill = Level_of_Training)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Health Sciences Middle Level Trainees, ", input$year),
           x = "Level of Training", y = "Number of Trainees") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold", color = "black"),
            legend.position = "bottom",
            legend.text = element_text(size = 12, color = "black")) +  
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))  # Wrap x-axis labels for better readability
  })
  
  output$year_table <- renderDataTable({
    level_data <- switch(input$level,
                         "Certificate" = health_data[1:7, ],
                         "Diploma" = health_data[9:29, ],
                         "Higher Diploma" = health_data[31:46, ])
    
    year_col <- match(input$year, colnames(level_data))
    level_data <- level_data[, c(1, year_col)]
    
    # Remove row names
    rownames(level_data) <- NULL
    
    level_data
  })
  
  output$details_table <- renderDataTable({
    level_data <- switch(input$level,
                         "Certificate" = health_data[1:7, ],
                         "Diploma" = health_data[9:29, ],
                         "Higher Diploma" = health_data[31:46, ])
    
    level_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
