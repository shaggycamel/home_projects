# Select date range to be plotted
dateRangeInput("date", strong("Date range"), 
               start = "2007-01-01", end = "2017-07-31",
               min = "2007-01-01", max = "2017-07-31")

  
  mainPanel(
    plotOutput(outputId = "lineplot", height = "300px"),
    textOutput(outputId = "desc"),
    tags$a(href = "https://www.google.com/finance/domestic_trends", 
           "Source: Google Domestic Trends", target = "_blank")
  )

  
  output$lineplot <- renderPlot({
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Date", ylab = "Trend index")
  })
