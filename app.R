
ui <- navbarPage(
  
  title = "BasicML",
  theme = shinytheme("slate"),
  
  tabPanel(
    title = "Info",
    icon = icon("info")
    
  ),
  
  navbarMenu(
    title = "Clustering",

    tabPanel(
      title = "K-Means",
      icon = icon("gauge"),
      
      tags$h3("2-Dimensional K-Means Clustering"),
      
      includeCSS("www/dropdown.css"),
      
      sidebarLayout(
        
        sidebarPanel(
          width = 3,
          
          tags$h4( "Controls" ),
          
          selectInput('vars', 'Select Variables', var_names, multiple = TRUE, selected = var_names[1] ) ,
          #uiOutput( "update_dropdown" ),
          #selectInput('ycol', 'Y Variable', names(var_lookup), selected = names(var_lookup)[2] ),
          numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
          actionButton( "run", "Run")
          
          ),

        mainPanel(

          fluidRow(
            column(
              width = 7,
              plotOutput('plot1')
            )
            
          )
        )
      
      )
    ),
    
    tabPanel(
      title = "Something Else",
      icon = icon("gauge")
    )
  )
)

server <- function(input, output, session) {
  
  # output$update_dropdown <- renderUI({
  #   
  #   new_names <- reactive({
  #     names(var_lookup)[names(var_lookup) != input$xcol]
  #   })
  #   
  #   selectInput('ycol', 'Y Variable', new_names(), selected = new_names()[2], selectize = FALSE)
  #   
  # })
  # 
  
  # Combine the selected variables into a new data frame
  selectedData <- eventReactive( input$run, {
    
    data %>% select( input$vars )
    
    #iris[, c(var_lookup[[input$xcol]], var_lookup[[input$ycol]]) ]
  })
  
  selectedClusters <- eventReactive(input$run, { input$clusters   })
    
  clusters <- eventReactive( input$run, {
    kmeans(selectedData(), selectedClusters() )
  })
  
  
  
  output$plot1 <- renderPlot({
    
    ggplot() + 
      geom_point( aes( x = selectedData()[,1], 
                       y = selectedData()[,2], 
                       col = as.factor(clusters()$cluster)), size = 3, alpha = .25) +
      geom_point( aes(x = clusters()$centers[,1], y = clusters()$centers[,2], 
                      col = as.factor(1:selectedClusters()) ), size = 5) +
      #labs( x = input$xcol, y = input$ycol) +
      theme_bw() + 
      theme( legend.position = "none",
             plot.background = element_rect(fill = "#1c1e22", color = "#0c0d0e"),
             panel.border = element_rect( color = "#0c0d0e"),
             panel.grid = element_line( color = "#333333"),
             panel.background = element_rect( fill = "#1c1e22"), 
             axis.title.x = element_text( color = "#bec5cb", size = 18),
             axis.title.y = element_text( color = "#bec5cb", size = 18),
             axis.ticks = element_line( color = "#bec5cb"),
             axis.text = element_text(color = "#bec5cb", size = 14) )
    
  })
  
}

shinyApp(ui, server)
