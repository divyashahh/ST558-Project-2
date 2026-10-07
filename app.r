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

# Variables available for filtering/exploration

numeric_choices <- c(
  "Price",
  "Rooms",
  "Distance",
  "Bedroom2",
  "Bathroom",
  "Car",
  "Landsize",
  "BuildingArea",
  "YearBuilt",
  "Propertycount"
)

categorical_choices <- c(
  "Type",
  "Method",
  "Regionname",
  "CouncilArea",
  "Suburb"
)

# ------------------------------------------------------------
# User Interface
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    
    # ========================================================
    # Sidebar
    # ========================================================
    
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
    
    # ========================================================
    # Main Panel
    # ========================================================
    
    mainPanel(
      
      tabsetPanel(
        
        id = "main_tabs",
        selected = "About",
        
        # ----------------------------------------------------
        # About
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
            "Users can filter properties by type, region, and
            selected numeric characteristics."
          ),
          
          h3("About the Data"),
          
          p(
            "The dataset contains information including property
            price, number of rooms, property type, geographic
            region, distance from Melbourne CBD, land size,
            building area, and other property characteristics."
          ),
          
          p("More information about the dataset:"),
          
          tags$a(
            href =
              "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
            "Melbourne Housing Market Dataset",
            target = "_blank"
          ),
          
          h3("How to Use the App"),
          
          tags$ul(
            tags$li(
              "Use the sidebar to choose property types and regions."
            ),
            tags$li(
              "Choose two numerical variables and their desired ranges."
            ),
            tags$li(
              "Click Apply Filters to update the app."
            ),
            tags$li(
              "Use Data Download to view and save the filtered data."
            ),
            tags$li(
              "Use Data Exploration to create tables, summaries, and graphs."
            )
          )
        ),
        
        # ----------------------------------------------------
        # Data Download
        # ----------------------------------------------------
        
        tabPanel(
          title = "Data Download",
          value = "Data Download",
          
          h2("Filtered Housing Data"),
          
          p(
            "The table below displays the data from the most
            recently applied filters."
          ),
          
          downloadButton(
            outputId = "download_data",
            label = "Download Filtered Data"
          ),
          
          br(),
          br(),
          
          DT::dataTableOutput("housing_table")
        ),
        
        # ----------------------------------------------------
        # Data Exploration
        # ----------------------------------------------------
        
        tabPanel(
          title = "Data Exploration",
          value = "Data Exploration",
          
          h2("Explore the Data"),
          
          p(
            "Use the options below to create summaries and
            visualizations using the filtered housing data."
          ),
          
          tabsetPanel(
            
            # ==================================================
            # Categorical summaries
            # ==================================================
            
            tabPanel(
              "Categorical Summaries",
              
              br(),
              
              h3("One-Way Contingency Table"),
              
              selectInput(
                inputId = "cat_var1",
                label = "Choose a categorical variable:",
                choices = categorical_choices,
                selected = "Type"
              ),
              
              tableOutput("one_way_table"),
              
              hr(),
              
              h3("Two-Way Contingency Table"),
              
              selectInput(
                inputId = "cat_var2",
                label = "Choose the first categorical variable:",
                choices = categorical_choices,
                selected = "Type"
              ),
              
              selectInput(
                inputId = "cat_var3",
                label = "Choose the second categorical variable:",
                choices = categorical_choices,
                selected = "Regionname"
              ),
              
              tableOutput("two_way_table")
            ),
            
            # ==================================================
            # Numerical summaries
            # ==================================================
            
            tabPanel(
              "Numerical Summaries",
              
              br(),
              
              h3("Numerical Summary by Category"),
              
              selectInput(
                inputId = "summary_numeric",
                label = "Choose a numeric variable:",
                choices = numeric_choices,
                selected = "Price"
              ),
              
              selectInput(
                inputId = "summary_group",
                label = "Summarize across levels of:",
                choices = categorical_choices,
                selected = "Type"
              ),
              
              DT::dataTableOutput("numeric_summary")
            ),
            
            # ==================================================
            # Visualizations
            # ==================================================
            
            tabPanel(
              "Visualizations",
              
              br(),
              
              selectInput(
                inputId = "plot_type",
                label = "Choose a plot type:",
                choices = c(
                  "Bar Chart",
                  "Box Plot",
                  "Scatter Plot",
                  "Faceted Scatter Plot",
                  "Heatmap"
                ),
                selected = "Bar Chart"
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Bar Chart'",
                
                selectInput(
                  inputId = "bar_x",
                  label = "Categorical variable:",
                  choices = categorical_choices,
                  selected = "Type"
                ),
                
                selectInput(
                  inputId = "bar_fill",
                  label = "Color/group by:",
                  choices = categorical_choices,
                  selected = "Regionname"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Box Plot'",
                
                selectInput(
                  inputId = "box_x",
                  label = "Categorical variable:",
                  choices = categorical_choices,
                  selected = "Type"
                ),
                
                selectInput(
                  inputId = "box_y",
                  label = "Numeric variable:",
                  choices = numeric_choices,
                  selected = "Price"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Scatter Plot'",
                
                selectInput(
                  inputId = "scatter_x",
                  label = "X variable:",
                  choices = numeric_choices,
                  selected = "Distance"
                ),
                
                selectInput(
                  inputId = "scatter_y",
                  label = "Y variable:",
                  choices = numeric_choices,
                  selected = "Price"
                ),
                
                selectInput(
                  inputId = "scatter_color",
                  label = "Color by:",
                  choices = categorical_choices,
                  selected = "Type"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Faceted Scatter Plot'",
                
                selectInput(
                  inputId = "facet_x",
                  label = "X variable:",
                  choices = numeric_choices,
                  selected = "Rooms"
                ),
                
                selectInput(
                  inputId = "facet_y",
                  label = "Y variable:",
                  choices = numeric_choices,
                  selected = "Price"
                ),
                
                selectInput(
                  inputId = "facet_var",
                  label = "Facet by:",
                  choices = categorical_choices,
                  selected = "Regionname"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Heatmap'",
                
                selectInput(
                  inputId = "heat_x",
                  label = "First categorical variable:",
                  choices = categorical_choices,
                  selected = "Type"
                ),
                
                selectInput(
                  inputId = "heat_y",
                  label = "Second categorical variable:",
                  choices = categorical_choices,
                  selected = "Regionname"
                ),
                
                selectInput(
                  inputId = "heat_numeric",
                  label = "Numeric variable to summarize:",
                  choices = numeric_choices,
                  selected = "Price"
                )
              ),
              
              br(),
              
              plotOutput(
                outputId = "exploration_plot",
                height = "600px"
              )
            )
            
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
  
  # ==========================================================
  # Dynamic sidebar sliders
  # ==========================================================
  
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
  
  # ==========================================================
  # Reactive filtered data
  # ==========================================================
  
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
          .data[[input$numeric_var1]] >=
            input$numeric_range1[1],
          .data[[input$numeric_var1]] <=
            input$numeric_range1[2]
        )
      
      data <- data |>
        filter(
          !is.na(.data[[input$numeric_var2]]),
          .data[[input$numeric_var2]] >=
            input$numeric_range2[1],
          .data[[input$numeric_var2]] <=
            input$numeric_range2[2]
        )
      
      data
    },
    ignoreNULL = FALSE
  )
  
  # ==========================================================
  # Data Download tab
  # ==========================================================
  
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
  
  # ==========================================================
  # One-way contingency table
  # ==========================================================
  
  output$one_way_table <- renderTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations available."
      )
    )
    
    table(
      data[[input$cat_var1]],
      useNA = "ifany"
    )
  })
  
  # ==========================================================
  # Two-way contingency table
  # ==========================================================
  
  output$two_way_table <- renderTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations available."
      )
    )
    
    table(
      data[[input$cat_var2]],
      data[[input$cat_var3]],
      useNA = "ifany"
    )
  })
  
  # ==========================================================
  # Numerical summary
  # ==========================================================
  
  output$numeric_summary <- DT::renderDataTable({
    
    data <- filtered_data()
    
    summary_data <- data |>
      filter(
        !is.na(.data[[input$summary_numeric]]),
        !is.na(.data[[input$summary_group]])
      ) |>
      group_by(
        .data[[input$summary_group]]
      ) |>
      summarise(
        Count = n(),
        Mean =
          mean(.data[[input$summary_numeric]]),
        Median =
          median(.data[[input$summary_numeric]]),
        SD =
          sd(.data[[input$summary_numeric]]),
        Minimum =
          min(.data[[input$summary_numeric]]),
        Maximum =
          max(.data[[input$summary_numeric]]),
        .groups = "drop"
      )
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 10
      ),
      rownames = FALSE
    )
  })
  
  # ==========================================================
  # Exploration plot
  # ==========================================================
  
  output$exploration_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No properties match the current filters."
      )
    )
    
    # --------------------------------------------------------
    # Bar chart
    # --------------------------------------------------------
    
    if (input$plot_type == "Bar Chart") {
      
      plot_data <- data |>
        filter(
          !is.na(.data[[input$bar_x]]),
          !is.na(.data[[input$bar_fill]])
        )
      
      ggplot(
        plot_data,
        aes(
          x = .data[[input$bar_x]],
          fill = .data[[input$bar_fill]]
        )
      ) +
        geom_bar(
          position = "dodge"
        ) +
        labs(
          title = paste(
            input$bar_x,
            "by",
            input$bar_fill
          ),
          x = input$bar_x,
          y = "Count",
          fill = input$bar_fill
        ) +
        theme_minimal() +
        theme(
          axis.text.x =
            element_text(
              angle = 45,
              hjust = 1
            )
        )
    }
    
    # --------------------------------------------------------
    # Box plot
    # --------------------------------------------------------
    
    else if (input$plot_type == "Box Plot") {
      
      plot_data <- data |>
        filter(
          !is.na(.data[[input$box_x]]),
          !is.na(.data[[input$box_y]])
        )
      
      ggplot(
        plot_data,
        aes(
          x = .data[[input$box_x]],
          y = .data[[input$box_y]],
          fill = .data[[input$box_x]]
        )
      ) +
        geom_boxplot(
          show.legend = FALSE
        ) +
        labs(
          title = paste(
            input$box_y,
            "by",
            input$box_x
          ),
          x = input$box_x,
          y = input$box_y
        ) +
        theme_minimal() +
        theme(
          axis.text.x =
            element_text(
              angle = 45,
              hjust = 1
            )
        )
    }
    
    # --------------------------------------------------------
    # Scatter plot
    # --------------------------------------------------------
    
    else if (input$plot_type == "Scatter Plot") {
      
      plot_data <- data |>
        filter(
          !is.na(.data[[input$scatter_x]]),
          !is.na(.data[[input$scatter_y]]),
          !is.na(.data[[input$scatter_color]])
        )
      
      ggplot(
        plot_data,
        aes(
          x = .data[[input$scatter_x]],
          y = .data[[input$scatter_y]],
          color = .data[[input$scatter_color]]
        )
      ) +
        geom_point(
          alpha = 0.35
        ) +
        labs(
          title = paste(
            input$scatter_y,
            "vs.",
            input$scatter_x
          ),
          x = input$scatter_x,
          y = input$scatter_y,
          color = input$scatter_color
        ) +
        theme_minimal()
    }
    
    # --------------------------------------------------------
    # Faceted scatter plot
    # --------------------------------------------------------
    
    else if (
      input$plot_type ==
      "Faceted Scatter Plot"
    ) {
      
      plot_data <- data |>
        filter(
          !is.na(.data[[input$facet_x]]),
          !is.na(.data[[input$facet_y]]),
          !is.na(.data[[input$facet_var]])
        )
      
      ggplot(
        plot_data,
        aes(
          x = .data[[input$facet_x]],
          y = .data[[input$facet_y]]
        )
      ) +
        geom_point(
          alpha = 0.30
        ) +
        facet_wrap(
          vars(
            .data[[input$facet_var]]
          )
        ) +
        labs(
          title = paste(
            input$facet_y,
            "vs.",
            input$facet_x,
            "by",
            input$facet_var
          ),
          x = input$facet_x,
          y = input$facet_y
        ) +
        theme_minimal()
    }
    
    # --------------------------------------------------------
    # Heatmap
    # --------------------------------------------------------
    
    else if (input$plot_type == "Heatmap") {
      
      plot_data <- data |>
        filter(
          !is.na(.data[[input$heat_x]]),
          !is.na(.data[[input$heat_y]]),
          !is.na(.data[[input$heat_numeric]])
        ) |>
        group_by(
          .data[[input$heat_x]],
          .data[[input$heat_y]]
        ) |>
        summarise(
          value =
            median(
              .data[[input$heat_numeric]]
            ),
          .groups = "drop"
        )
      
      ggplot(
        plot_data,
        aes(
          x = .data[[input$heat_x]],
          y = .data[[input$heat_y]],
          fill = value
        )
      ) +
        geom_tile() +
        scale_fill_viridis_c() +
        labs(
          title = paste(
            "Median",
            input$heat_numeric,
            "by",
            input$heat_x,
            "and",
            input$heat_y
          ),
          x = input$heat_x,
          y = input$heat_y,
          fill = paste(
            "Median",
            input$heat_numeric
          )
        ) +
        theme_minimal() +
        theme(
          axis.text.x =
            element_text(
              angle = 45,
              hjust = 1
            )
        )
    }
  })
}

# ------------------------------------------------------------
# Run Application
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)