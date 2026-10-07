# ============================================================
# ST 558 Project 2
# Melbourne Housing Shiny Application
# ============================================================

library(shiny)
library(tidyverse)
library(DT)
library(shinycssloaders)

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
# UI
# ------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #f7f7f7;
        }

        .well {
          background-color: white;
        }

        h2, h3 {
          margin-top: 20px;
        }

        .app-image {
          max-width: 650px;
          width: 100%;
          margin-top: 15px;
          margin-bottom: 20px;
          border-radius: 6px;
        }

        .filter-note {
          color: #555555;
          font-size: 13px;
        }
      ")
    )
  ),
  
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
      
      p(
        class = "filter-note",
        "Changing a control will not update the results until
        Apply Filters is pressed."
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
      ),
      
      br(),
      br(),
      
      textOutput("row_count")
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
            "This application allows users to interactively
            explore residential property data from Melbourne,
            Australia."
          ),
          
          tags$img(
            src = "melbourne.jpg",
            class = "app-image",
            alt = "Melbourne housing"
          ),
          
          h3("About the Data"),
          
          p(
            "The dataset contains information about residential
            properties including price, number of rooms,
            property type, geographic region, distance from the
            Melbourne CBD, land size, building area, and other
            property characteristics."
          ),
          
          tags$a(
            href =
              "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
            "View the Melbourne Housing Market data source",
            target = "_blank"
          ),
          
          h3("Using the Sidebar"),
          
          tags$ul(
            tags$li(
              "Select one or more property types."
            ),
            tags$li(
              "Select one or more Melbourne regions."
            ),
            tags$li(
              "Choose two numeric variables and their ranges."
            ),
            tags$li(
              "Press Apply Filters to apply all sidebar selections."
            )
          ),
          
          h3("App Tabs"),
          
          tags$ul(
            tags$li(
              strong("About: "),
              "Provides information about the app and dataset."
            ),
            tags$li(
              strong("Data Download: "),
              "Displays the filtered dataset and allows it to be
              downloaded as a CSV file."
            ),
            tags$li(
              strong("Data Exploration: "),
              "Creates categorical summaries, numerical summaries,
              and visualizations from the filtered data."
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
            "The table reflects the most recently applied
            sidebar filters."
          ),
          
          downloadButton(
            outputId = "download_data",
            label = "Download Filtered Data"
          ),
          
          br(),
          br(),
          
          shinycssloaders::withSpinner(
            DT::dataTableOutput("housing_table")
          )
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
            visualizations from the filtered housing data."
          ),
          
          tabsetPanel(
            
            # ==================================================
            # Categorical
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
                label = "First categorical variable:",
                choices = categorical_choices,
                selected = "Type"
              ),
              
              selectInput(
                inputId = "cat_var3",
                label = "Second categorical variable:",
                choices = categorical_choices,
                selected = "Regionname"
              ),
              
              tableOutput("two_way_table")
            ),
            
            # ==================================================
            # Numerical
            # ==================================================
            
            tabPanel(
              "Numerical Summaries",
              
              br(),
              
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
              
              shinycssloaders::withSpinner(
                DT::dataTableOutput("numeric_summary")
              )
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
                  "bar_x",
                  "Categorical variable:",
                  choices = categorical_choices,
                  selected = "Type"
                ),
                
                selectInput(
                  "bar_fill",
                  "Color/group by:",
                  choices = categorical_choices,
                  selected = "Regionname"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Box Plot'",
                
                selectInput(
                  "box_x",
                  "Categorical variable:",
                  choices = categorical_choices,
                  selected = "Type"
                ),
                
                selectInput(
                  "box_y",
                  "Numeric variable:",
                  choices = numeric_choices,
                  selected = "Price"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Scatter Plot'",
                
                selectInput(
                  "scatter_x",
                  "X variable:",
                  choices = numeric_choices,
                  selected = "Distance"
                ),
                
                selectInput(
                  "scatter_y",
                  "Y variable:",
                  choices = numeric_choices,
                  selected = "Price"
                ),
                
                selectInput(
                  "scatter_color",
                  "Color by:",
                  choices = categorical_choices,
                  selected = "Type"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Faceted Scatter Plot'",
                
                selectInput(
                  "facet_x",
                  "X variable:",
                  choices = numeric_choices,
                  selected = "Rooms"
                ),
                
                selectInput(
                  "facet_y",
                  "Y variable:",
                  choices = numeric_choices,
                  selected = "Price"
                ),
                
                selectInput(
                  "facet_var",
                  "Facet by:",
                  choices = categorical_choices,
                  selected = "Regionname"
                )
              ),
              
              conditionalPanel(
                condition =
                  "input.plot_type == 'Heatmap'",
                
                selectInput(
                  "heat_x",
                  "First categorical variable:",
                  choices = categorical_choices,
                  selected = "Type"
                ),
                
                selectInput(
                  "heat_y",
                  "Second categorical variable:",
                  choices = categorical_choices,
                  selected = "Regionname"
                ),
                
                selectInput(
                  "heat_numeric",
                  "Numeric variable to summarize:",
                  choices = numeric_choices,
                  selected = "Price"
                )
              ),
              
              shinycssloaders::withSpinner(
                plotOutput(
                  "exploration_plot",
                  height = "600px"
                )
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
  # Dynamic Sliders
  # ==========================================================
  
  output$numeric_slider1 <- renderUI({
    
    req(input$numeric_var1)
    
    values <- housing[[input$numeric_var1]]
    values <- values[!is.na(values)]
    
    sliderInput(
      "numeric_range1",
      paste("Select", input$numeric_var1, "range:"),
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
      "numeric_range2",
      paste("Select", input$numeric_var2, "range:"),
      min = min(values),
      max = max(values),
      value = c(
        min(values),
        max(values)
      )
    )
  })
  
  # ==========================================================
  # Filter Data
  # ==========================================================
  
  filtered_data <- eventReactive(
    input$apply_filters,
    {
      
      validate(
        need(
          length(input$type_filter) > 0,
          "Please select at least one property type."
        ),
        need(
          length(input$region_filter) > 0,
          "Please select at least one region."
        )
      )
      
      req(
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
  # Row count
  # ==========================================================
  
  output$row_count <- renderText({
    
    data <- filtered_data()
    
    paste(
      format(nrow(data), big.mark = ","),
      "properties currently selected"
    )
  })
  
  # ==========================================================
  # Data Table
  # ==========================================================
  
  output$housing_table <- DT::renderDataTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No properties match the current filters."
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
  
  # ==========================================================
  # Download
  # ==========================================================
  
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
  # One-Way Table
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
  # Two-Way Table
  # ==========================================================
  
  output$two_way_table <- renderTable({
    
    data <- filtered_data()
    
    validate(
      need(
        input$cat_var2 != input$cat_var3,
        "Choose two different categorical variables."
      )
    )
    
    table(
      data[[input$cat_var2]],
      data[[input$cat_var3]],
      useNA = "ifany"
    )
  })
  
  # ==========================================================
  # Numeric Summary
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
    
    validate(
      need(
        nrow(summary_data) > 0,
        "No complete observations are available for this summary."
      )
    )
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ==========================================================
  # Plot
  # ==========================================================
  
  output$exploration_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No properties match the current filters."
      )
    )
    
    if (input$plot_type == "Bar Chart") {
      
      validate(
        need(
          input$bar_x != input$bar_fill,
          "Choose different variables for the x-axis and grouping."
        )
      )
      
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
    
    else if (input$plot_type == "Box Plot") {
      
      plot_data <- data |>
        filter(
          !is.na(.data[[input$box_x]]),
          !is.na(.data[[input$box_y]])
        )
      
      validate(
        need(
          nrow(plot_data) > 0,
          "No complete observations are available for this plot."
        )
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
    
    else if (input$plot_type == "Scatter Plot") {
      
      validate(
        need(
          input$scatter_x != input$scatter_y,
          "Choose different x and y variables."
        )
      )
      
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
    
    else if (
      input$plot_type ==
      "Faceted Scatter Plot"
    ) {
      
      validate(
        need(
          input$facet_x != input$facet_y,
          "Choose different x and y variables."
        )
      )
      
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
          alpha = 0.3
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
    
    else if (input$plot_type == "Heatmap") {
      
      validate(
        need(
          input$heat_x != input$heat_y,
          "Choose two different categorical variables."
        )
      )
      
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
      
      validate(
        need(
          nrow(plot_data) > 0,
          "No complete observations are available for this heatmap."
        )
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
# Run app
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)