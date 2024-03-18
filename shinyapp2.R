# Title: Retirement contribution calculator
# Description: This calculator allows users to determine 
# how much money they could save over time by making recurring payments 
# from each paycheck into their employer retirement plan (or another similar retirement account). 
# Details: Users are able to choose the amount of salary,
#rate of growth, contribution percentage, number of periods,
# years invested, annual rate of return, and show target amount.
# Author: Sitie Cai
# Date: 11/1/23


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(bslib)     # for creating nice shiny dashboards
library(tidyverse) # for data manipulation and graphics
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)





# =======================================================
# Define UI for application
# =======================================================
ui <- page_sidebar(
  title = "Retirement contribution calculator",
  fillable = FALSE,
  
  # -------------------------------------------------------
  # Bootstrap theme
  # (feel free to comment-out the theme commands)
  # -------------------------------------------------------
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Inter")
  ),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebar = sidebar(
    title = "Inputs",
    numericInput(inputId = "salary",
                 label = "Annual Salary:",
                 value = 80000),
    sliderInput(inputId = "rate",
                label = "Rate of Growth:",
                min = 0, 
                max = 1, 
                value = 0.02),
    sliderInput(inputId = "contri",
                label = "Contribution Percentage:",
                min = 0, 
                max = 1, 
                value = 0.15),
    radioButtons(inputId = "periods",
                 label = "Number of Periods",
                 choices = list("Annually: k = 1",
                                "Semi-annual: k = 2",
                                "Quarterly: k = 4",
                                "Bimonthly: k = 6",
                                "Monthly: k = 12",
                                "Weekly: k = 52"),
                 selected = "Monthly: k = 12"),
    numericInput(inputId = "years",
                 label = "Years Invested:",
                 value = 5),
    sliderInput(inputId = "ror",
                label = "Annual Rate of Return:",
                min = 0, 
                max = 1, 
                value = 0.08),
    numericInput(inputId = "target",
                 label = "Target Amount:",
                 value = 35000),
    checkboxInput(inputId = "show",
                  label = "Show Target",
                  value = FALSE)
  ),  # closes sidebar (of inputs)
  
  # -------------------------------------------------------
  # Main content area with outputs: plots and table
  # Note: outputs are wrapped inside cards() 
  # -------------------------------------------------------
  card(
    card_header("Balance timeline"),
    height = 500,
    plotlyOutput(outputId = "plot1")
  ),
  card(
    card_header("Balance decomposition"),
    plotlyOutput(outputId = "plot2")
  ),
  card(
    card_header("Data Table output"),
    height = 600,
    style = "resize:vertical;",
    card_body(
      min_height = 500,
      dataTableOutput(outputId = "table")
    )
  )
  
) # closes page_sidebar (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive Balance table
  # (adapt code to get the appropriate Balance data table)
  # ------------------------------------------------------------
  tbl = reactive({
    if(input$periods == "Annually: k = 1"){
      periods = 1
    }
    if(input$periods == "Semi-annual: k = 2"){
      periods = 2
    }
    if(input$periods == "Quarterly: k = 4"){
      periods = 4
    }
    if(input$periods == "Bimonthly: k = 6"){
      periods = 6
    }
    if(input$periods == "Monthly: k = 12"){
      periods = 12
    }
    if(input$periods == "Weekly: k = 52"){
      periods = 52
    }
    periods
    
    salary = input$salary * (1 + input$rate)^(1:input$years - 1)
    contribution = (input$contri*salary)/periods
    total_periods = periods*input$years
    
    balance = rep(0, total_periods)
    balance[1] = contribution[1]
    
    for(var in 2:total_periods){
      balance[var] = balance[var - 1]*(1+input$ror/periods)+ contribution[ceiling(var/periods)]
    }
    
    own = cumsum(salary * input$contri)
    
    
    balance_tbl = data.frame(
      year = rep(1:input$years, each = periods),
      salary = rep(salary, each = periods), 
      contribution = rep(contribution, each = periods),
      balance = balance,
      own = rep(own, each = periods)) %>%
      group_by(year) %>%
      summarize(salary = max(salary),
                contribution = max(contribution) * periods,
                balance = max(balance),
                own = max(own),
                growth = max(balance - own),
                own_pct= max(own/balance)*100,
                growth_pct = max(growth/balance)*100,
                hit_target = ifelse(balance >= input$target, "yes", "no"))
    balance_tbl
  })
  
  # ------------------------------------------------------------
  # Plot of balance timeline
  # (adapt code to make a timeline according to your analysis)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    p <- ggplot(data = tbl(), aes(x = year, y = balance)) +
      geom_line() +
      geom_point() +
      geom_area(alpha = 0.3) +
      scale_y_continuous(label = scales::comma) +
      theme_gray()
    if(input$show){
      p <-  p + geom_hline(data = tbl(), aes(yintercept = input$target), 
                           color = "orange")
    }
    p
  })
  
  
  # ------------------------------------------------------------
  # Plot of balance decomposition
  # (adapt code to make a graphic according to your analysis)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    new_tbl = tbl() %>%
      pivot_longer(cols = own:growth, 
                   names_to = "type")
    
    q = ggplot(data = new_tbl, aes( x = year, y = value)) +
      geom_col(data = new_tbl, aes( fill = type))+
      scale_y_continuous( label = scales::comma)
    q
  })
  
  
  # ------------------------------------------------------------
  # Table with Retirement Balance data
  # (adapt code to display appropriate table)
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    # to limit the number of decimal digits in the rendered table
    # you can convert it into a "DataTable" object, and then use
    # formatRound() to limit the displayed decimals.
    tbl() |>
      datatable() |> # convert into "DataTable" object
      formatRound(columns = 2:8, 
                  digits = 2) # round to 2-digits
  })
  
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
