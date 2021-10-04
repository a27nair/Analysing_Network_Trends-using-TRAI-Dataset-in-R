library(shiny)
library(shinydashboard)
 library(shiny)
library(DT)
ui <-dashboardPage(skin="green",
                   
dashboardHeader(title = "Analysis App",
                dropdownMenu(type="message",
                     messageItem(from="Artile",message="hi")
                 )
 ),
  
  dashboardSidebar(
    sidebarSearchForm("SearchText","buttonSearch","search"),
    
    sidebarMenu(
      menuItem("dashboard",tabName = "dashboard",icon =icon("dashboard")),
      menuItem("Iris", tabName = "iris", icon =icon("tree")),
      menuItem("Cars",  tabName= "cars", icon=icon("car")),
      
      
      
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              sliderInput("bins","Months",1,12,1)
              
              ),
      tabItem("iris",
              box(plotOutput("correlation_plot"),width=8),
              box(
                selectInput("features","Features:",
                            c("Sepal.Width","Petal.Length",
                             "Petal.Width")),width = 4
              )       
              
       ),
      tabItem("cars",
              fluidPage(
                h1("Cars"),
                dataTableOutput("carstable")
              )
              
      )
              
    )
    
   # mainPanel()
  )
  
)
 
 
 server <- function(input, output) {
   output$correlation_plot<-renderPlot({
     plot(iris$Sepal.Length,iris[[input$features]],
          xlab = "Sepal length",ylab = "Feature")
   })
   output$carstable<-renderDataTable(mtcars)
 }
 
 shinyApp(ui,server)
