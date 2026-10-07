# ============================================================
# ST 558 Project 2
# Melbourne Housing Shiny Application
# ============================================================

library(shiny)
library(tidyverse)
library(DT)

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
            "This application allows users to explore residential
            property data from Melbourne, Australia."
          ),
          
          p(
            "Users can filter the dataset by property type,
            geographic region, and selected numeric variables."
          ),
          
          h3("About the Data"),
          
          p(
            "The dataset contains information on Melbourne
            residential properties including property prices,
            number of rooms, property type, region, distance from
            Melbourne's central business district, land size,
            building area, and other housing characteristics."
          ),
          
          p(
            "For more information about the Melbourne Housing dataset, visit:"
          ),
          
          tags$a(
            href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
            "Melbourne Housing Market Dataset",
            target = "_blank"
          ),
          
          br(),
          br(),
          
          tags$img(
            src = "https://images.unsplash.com/photo-1518005020951-eccb494ad742",
            width = "100%",
            style = "max-width: 600px;"
          ),
          
          h3("How to Use the App"),
          
          tags$ul(
            tags$li(
              "Use the sidebar to select one or more property types."
            ),
            tags$li(
              "Choose one or more Melbourne regions."
            ),
            tags$li(
              "Choose two numeric variables and select their ranges."
            ),
            tags$li(
              "Click Apply Filters to update the dataset."
            ),
            tags$li(
              "Use the Data Download tab to view or download the filtered data."
            ),
            tags$li(
              "Use the Data Exploration tab to create summaries and plots."
            )
          )
        ),
        
        # ----------------------------------------------------
        # Data Download Tab
        # ----------------------------------------------------
        
        tabPanel(
          title = "Data Download",
          value = "Data Download",
          
          h2("Filtered Housing Data"),
          
          p(
            "The table below displays the data based on the most
            recently applied sidebar filters."
          ),
          
          br(),
          
          downloadButton(
            outputId = "download_data",
            label = "Download Filtered Data"
          ),
          
          br(),
          br(),
          
          DT::dataTableOutput(
            outputId = "housing_table"
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
            "Interactive numerical summaries, categorical summaries,
            and visualizations will be available in this section."
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
  # Reactive filtered dataset
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
      
      data <- data |>
        filter(
          !is.na(.data[[input$numeric_var1]]),
          .data[[input$numeric_var1]] >= input$numeric_range1[1],
          .data[[input$numeric_var1]] <= input$numeric_range1[2]
        )
      
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
  
  # ----------------------------------------------------------
  # Data table
  # ----------------------------------------------------------
  
  output$housing_table <- DT::renderDataTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No properties match the selected filters."
      )
    )
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  # ----------------------------------------------------------
  # Download filtered data
  # ----------------------------------------------------------
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "melbourne_housing_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write_csv(
        filtered_data(),
        file
      )
      
    }
    
  )
  
}

# ------------------------------------------------------------
# Run Application
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)