# ============================================================
# ST 558 Project 2
# Melbourne Housing Shiny Application
# ============================================================

library(shiny)
library(tidyverse)

# ------------------------------------------------------------
# Load and clean data
# ------------------------------------------------------------

housing <- read_csv(
  "data/Melbourne_housing_FULL.csv",
  na = c("", "NA", "#N/A"),
  show_col_types = FALSE
)

housing <- housing |>
  mutate(
    Type = recode(
      Type,
      "h" = "House",
      "u" = "Unit",
      "t" = "Townhouse"
    )
  )

# Numeric variables that users can filter
numeric_choices <- c(
  "Price",
  "Rooms",
  "Distance",
  "Bedroom2",
  "Bathroom",
  "Car",
  "Landsize"
)

# ------------------------------------------------------------
# User Interface
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    
    # --------------------------------------------------------
    # Sidebar
    # --------------------------------------------------------
    
    sidebarPanel(
      
      h3("Filter Properties"),
      
      p(
        "Choose property characteristics below, then click
        Apply Filters to update the app."
      ),
      
      hr(),
      
      h4("Property Type"),
      
      checkboxGroupInput(
        inputId = "type_filter",
        label = NULL,
        choices = sort(unique(housing$Type)),
        selected = sort(unique(housing$Type))
      ),
      
      h4("Region"),
      
      checkboxGroupInput(
        inputId = "region_filter",
        label = NULL,
        choices = sort(unique(na.omit(housing$Regionname))),
        selected = sort(unique(na.omit(housing$Regionname)))
      ),
      
      hr(),
      
      h4("Numeric Filter 1"),
      
      selectInput(
        inputId = "numeric_var1",
        label = "Choose a numeric variable:",
        choices = numeric_choices,
        selected = "Price"
      ),
      
      uiOutput("numeric_slider1"),
      
      hr(),
      
      h4("Numeric Filter 2"),
      
      selectInput(
        inputId = "numeric_var2",
        label = "Choose another numeric variable:",
        choices = numeric_choices,
        selected = "Rooms"
      ),
      
      uiOutput("numeric_slider2"),
      
      hr(),
      
      actionButton(
        inputId = "apply_filters",
        label = "Apply Filters",
        class = "btn-primary"
      )
      
    ),
    
    # --------------------------------------------------------
    # Main Panel
    # --------------------------------------------------------
    
    mainPanel(
      
      tabsetPanel(
        
        id = "main_tabs",
        selected = "About",
        
        # ----------------------------------------------------
        # About Tab
        # ----------------------------------------------------
        
        tabPanel(
          title = "About",
          value = "About",
          
          h2("About the Melbourne Housing Explorer"),
          
          p(
            "This application allows users to explore housing
            data from Melbourne, Australia."
          ),
          
          p(
            "Use the sidebar to subset the data by property type,
            region, and two numeric variables."
          ),
          
          h3("Dataset"),
          
          p(
            "The Melbourne Housing dataset contains residential
            property information including price, rooms,
            property type, region, distance from Melbourne CBD,
            and other housing characteristics."
          ),
          
          h3("How to Use the App"),
          
          tags$ul(
            tags$li(
              "Choose one or more property types."
            ),
            tags$li(
              "Choose one or more Melbourne regions."
            ),
            tags$li(
              "Select two numeric variables and choose their ranges."
            ),
            tags$li(
              "Click Apply Filters to update the data used throughout the app."
            )
          ),
          
          p(
            "The Data Download tab will display the filtered data
            and provide a download option."
          ),
          
          p(
            "The Data Exploration tab will provide categorical
            summaries, numerical summaries, and visualizations."
          )
        ),
        
        # ----------------------------------------------------
        # Data Download Tab
        # ----------------------------------------------------
        
        tabPanel(
          title = "Data Download",
          value = "Data Download",
          
          h2("Data Download"),
          
          p(
            "The filtered Melbourne housing data will be
            displayed here."
          )
        ),
        
        # ----------------------------------------------------
        # Data Exploration Tab
        # ----------------------------------------------------
        
        tabPanel(
          title = "Data Exploration",
          value = "Data Exploration",
          
          h2("Data Exploration"),
          
          p(
            "The summaries and plots in this section will use
            the filtered dataset."
          )
        )
        
      )
      
    )
    
  )
  
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Dynamic numeric slider 1
  # ----------------------------------------------------------
  
  output$numeric_slider1 <- renderUI({
    
    req(input$numeric_var1)
    
    values <- housing[[input$numeric_var1]]
    values <- values[!is.na(values)]
    
    sliderInput(
      inputId = "numeric_range1",
      label = paste(
        "Select",
        input$numeric_var1,
        "range:"
      ),
      min = min(values),
      max = max(values),
      value = c(
        min(values),
        max(values)
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # Dynamic numeric slider 2
  # ----------------------------------------------------------
  
  output$numeric_slider2 <- renderUI({
    
    req(input$numeric_var2)
    
    values <- housing[[input$numeric_var2]]
    values <- values[!is.na(values)]
    
    sliderInput(
      inputId = "numeric_range2",
      label = paste(
        "Select",
        input$numeric_var2,
        "range:"
      ),
      min = min(values),
      max = max(values),
      value = c(
        min(values),
        max(values)
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # Filtered data
  #
  # eventReactive means the data only updates when the
  # Apply Filters button is clicked.
  # ----------------------------------------------------------
  
  filtered_data <- eventReactive(
    input$apply_filters,
    {
      
      req(
        input$type_filter,
        input$region_filter,
        input$numeric_var1,
        input$numeric_var2,
        input$numeric_range1,
        input$numeric_range2
      )
      
      data <- housing |>
        filter(
          Type %in% input$type_filter,
          Regionname %in% input$region_filter
        )
      
      # First numeric filter
      data <- data |>
        filter(
          !is.na(.data[[input$numeric_var1]]),
          .data[[input$numeric_var1]] >= input$numeric_range1[1],
          .data[[input$numeric_var1]] <= input$numeric_range1[2]
        )
      
      # Second numeric filter
      data <- data |>
        filter(
          !is.na(.data[[input$numeric_var2]]),
          .data[[input$numeric_var2]] >= input$numeric_range2[1],
          .data[[input$numeric_var2]] <= input$numeric_range2[2]
        )
      
      data
      
    },
    ignoreNULL = FALSE
  )
  
}

# ------------------------------------------------------------
# Run Application
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)