# Load necessary libraries
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Sizeha", "Lake Size (ha):", value = 45000, min = 1, step = 1),
      numericInput("MaxDepth", "Max. Depth \n (where deepest stratum area \n is greater  than 5% of lake):", value = 38, min = 1, step = 1),
      actionButton("update", "Update")
    ),
    
    mainPanel(
      plotOutput("allocationPlot"),
      tableOutput("filteredTable"), 
      p("Note: "), 
      p("NA typically fished as two gang straps."), 
      p("ON typically fished as single gang")
      
      
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive values for inputs
  data <- reactive({
    Sizeha <- input$Sizeha
    MaxDepth <- input$MaxDepth
    
    # largemesh polynomial
    x <- 0:50000
    y <- -0.00000003 * x^2 + 0.003 * x + 22.989
    modlm <- lm(y ~ poly(x, 2))
    
    # smallmesh polynomial
    x2 <- 1:50000
    y2 <- -0.00000001 * x2^2 + 0.0014 * x2 + 16.774
    modsm <- lm(y2 ~ poly(x2, 2))
    
    
    # Create individual data frames
    s3.6 <- data.frame(depth = c(6, 6), value = c(0.51, 0.49), strata = c("1-3", "3-6"), Scaling = 0.5)
    s6.12 <- data.frame(depth = c(12, 12, 12), value = c(0.37, 0.29, 0.34), strata = c("1-3", "3-6", "6-12"), Scaling = 0.61)
    s12.20 <- data.frame(depth = c(20, 20, 20, 20), value = c(0.34, 0.25, 0.30, 0.11), strata = c("1-3", "3-6", "6-12", "12-20"), Scaling = 0.63)
    s20.35 <- data.frame(depth = c(35, 35, 35, 35, 35), value = c(0.28, 0.23, 0.26, 0.13, 0.10), strata = c("1-3", "3-6", "6-12", "12-20", "20-35"), Scaling = 0.68)
    s35.50 <- data.frame(depth = c(50, 50, 50, 50, 50, 50), value = c(0.25, 0.19, 0.23, 0.14, 0.11, 0.09), strata = c("1-3", "3-6", "6-12", "12-20", "20-35", "35-50"), Scaling = 0.78)
    s50.75 <- data.frame(depth = c(75, 75, 75, 75, 75, 75, 75), value = c(0.22, 0.17, 0.20, 0.14, 0.12, 0.10, 0.06), strata = c("1-3", "3-6", "6-12", "12-20", "20-35", "35-50", "50-75"), Scaling = 0.88)
    s200 <- data.frame(depth = c(999, 999, 999, 999, 999, 999, 999, 999), value = c(0.19, 0.14, 0.18, 0.14, 0.12, 0.11, 0.07, 0.05), strata = c("1-3", "3-6", "6-12", "12-20", "20-35", "35-50", "50-75", "75+"), Scaling = 1)
    
    # Combine into a single data frame
    logicdf <- rbind(s3.6, s6.12, s12.20, s20.35, s35.50, s50.75, s200)
    
    
    # Filter data based on maximum depth using base R
    logicdffilter <- logicdf[logicdf$depth >= MaxDepth, ]
    depth_needed <- unique(logicdffilter$depth)[1]
    
    # Filter and calculate sets using base R
    filtered_data <- logicdf[logicdf$depth == depth_needed, ]
    Ptotal<-unique(filtered_data$Scaling)
    
    
    # Predict number of nets
    SMallocation <- ifelse(Sizeha >= 50000, 50, predict(modsm, data.frame(x2 = Sizeha))*Ptotal)
    LMallocation <- ifelse(Sizeha >= 50000, 100, predict(modlm, data.frame(x = Sizeha))*Ptotal)
    
    filtered_data$NA.n <- as.integer(ceiling(filtered_data$value * LMallocation))
    filtered_data$ON.n <- as.integer(ceiling(filtered_data$value * SMallocation))
    
    # Compute totals 
    totals <- data.frame(
    strata = "Totals",  
    NA.n  = sum(filtered_data$NA.n),  
    ON.n = sum(filtered_data$ON.n), 
    depth=NA, 
    value=NA, 
    Scaling=NA
    )
    
    filtered_data <- rbind(filtered_data, totals)
    
    list(
      x = x, y = y, x2 = x2, y2 = y2,
      Sizeha = Sizeha, SMallocation = SMallocation, LMallocation = LMallocation,
      filtered_data = filtered_data
    )
  })
  
  # Render plot
  # output$allocationPlot <- renderPlot({
  #   d <- data()
  #   plot(d$x, d$y, type = "l", col = "black", lwd = 2,
  #        xlab = "Surface area (ha)", ylab = "Nets (n)",
  #        main = "Net Allocations Cycle 4")
  #   lines(d$x2, d$y2, col = "grey60")
  #   points(d$Sizeha, d$SMallocation, col = "grey60", pch = 19, cex = 1.5)
  #   points(d$Sizeha, d$LMallocation, col = "black", pch = 19, cex = 1.5)
  #   legend("topleft", legend = c("NA large mesh", "ON small mesh"),
  #          col = c("black", "grey60"), lty = c(1, 1), lwd = 2)
  # })
  # 
  # Render filtered table
  output$filteredTable <- renderTable({
   data()$filtered_data[,c(3,5:6)]
  })

  
}  
# Run the app
shinyApp(ui = ui, server = server)
