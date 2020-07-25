# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
pkgs <- c('XML', 'RCurl', 'lubridate', 'TSA', 'fUnitRoots', 'lmtest', 'FitAR', 'forecast', 'ggplot2', 'rugarch', 'knitr', 'dplyr')
invisible(lapply(pkgs, require, character.only = T))

# Load data
#### LINUX 
url <- paste0("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130428&end=", gsub("-", "", Sys.Date()))
#livedata <- getURL(url)
#bc <- readHTMLTable(livedata, which = 1, stringsAsFactors = FALSE, colClasses = numeric())
bc$Date <- mdy(bc$Date)
colnames(bc) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Market Cap")
bc <- arrange(bc, Date)
bc[, names(bc) %in% c("Volume", "Market Cap")] <- sapply(bc[, names(bc) %in% c("Volume", "Market Cap")], function(x) gsub(",", "", x))
bc[, !names(bc) %in% "Date"] <- sapply(bc[, !names(bc) %in% "Date"], as.numeric)
bc_ts <- ts(bc$Close, start = c(2013, as.numeric(format(as.Date("2013-04-28"), "%j"))), frequency = 365.25)
bc_train <- bc_ts[1:(length(bc_ts) - 10)]
bc_test <- bc_ts[(length(bc_ts) - 9): length(bc_ts)]

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Google Trend Index"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
                                   min = "2007-01-01", max = "2017-07-31")
                    
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    bc %>%
      filter(
        time > as.POSIXct(input$date[1]) & time < as.POSIXct(input$date[2])
        )
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)