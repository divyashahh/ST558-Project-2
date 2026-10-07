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
        "Use the controls in this sidebar to filter the
        Melbourne housing data."
      ),
      
      hr(),
      
      h4("Property Filters"),
      
      p(
        "Interactive filtering controls will be added here."
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
            "Users will be able to filter properties and
            investigate housing characteristics, prices,
            property types, and geographic regions."
          ),
          
          h3("Dataset"),
          
          p(
            "The Melbourne Housing dataset contains information
            about residential properties, including property
            prices, number of rooms, property type, location,
            distance from the Melbourne central business
            district, and other housing characteristics."
          ),
          
          h3("How to Use the App"),
          
          p(
            "Use the sidebar to select the properties you are
            interested in exploring."
          ),
          
          p(
            "The Data Download tab will allow users to view and
            download the filtered dataset."
          ),
          
          p(
            "The Data Exploration tab will provide numerical
            summaries, categorical summaries, and interactive
            visualizations."
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
          ),
          
          p(
            "Users will also be able to download the filtered
            data as a CSV file."
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
            "Use this section to explore numerical and
            categorical summaries of the Melbourne housing
            data."
          ),
          
          h3("Categorical Summaries"),
          
          p(
            "One-way and two-way contingency tables will be
            available here."
          ),
          
          h3("Numerical Summaries"),
          
          p(
            "Users will be able to compare numerical variables
            across categorical groups."
          ),
          
          h3("Visualizations"),
          
          p(
            "Interactive plots will be available here."
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
  
  # Reactive filtering and application outputs
  # will be added in later steps.
  
}

# ------------------------------------------------------------
# Run Application
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)