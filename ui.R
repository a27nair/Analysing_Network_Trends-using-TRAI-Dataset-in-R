library(shiny)
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(forcats)
library(leaflet)
#first trial
library(readr)
dataset22<- read.csv("FinalDataset20.csv") 
Mymapdata<-read.csv("FinalMap20.csv")


ui <-dashboardPage(
                   
                   dashboardHeader(title = "ANALYSIS OF TRENDS IN NETWORK",
                                   dropdownMenu(type="message",
                                                messageItem(from="Artile",message="hi")
                                   )
                   ),
                  
                   
                   
                   dashboardSidebar(
                    # sidebarSearchForm("SearchText","buttonSearch","search"),
                     
                     sidebarMenu(
                       menuItem("page",tabName = "homepage",icon =icon("homepage")),
                       menuItem("2020",  tabName= "2020", icon=icon("car")),
                       menuItem("Visual", tabName = "visual", icon =icon("tree")),
                       menuItem("correlation",  tabName= "correlation", icon=icon("car")),
                       menuItem('Map',tabName = 'map',icon=icon("image")
                                ),
                       
                      
                       #sliderInput("bins","Months",1,12,1),
                       radioButtons("ft1",label="Service Provider",
                                    choices = list("JIO"=1,"AIRTEL"=2, "IDEA"=3,"VODAFONE"=4,"CELLON"=5))
                       
                       
                       
                     )
                     
                   ),
                   dashboardBody(
                     tabItems(
                       
                       tabItem("homepage",
                                img(src = 'page.jpg', height = '500px', width = '1000px')
                              
                    
                               
                               
                               
                                   
                       ),
                       tabItem("2020",
                               fluidPage(
                                 fluidRow(
                                   valueBoxOutput("plan")
                                 ),
                                
                                 sidebarLayout(
                                   sidebarPanel(
                                       selectInput(inputId = "factor" ,
                                                   label = "choose feature for analysis", 
                                                  c("Service.Provider","Technology","Test_Type"),selected =NULL),width = 4
                                      
                                  
                                       
                                 
                                
                                   ),
                                 mainPanel(
                                   tabsetPanel(type = "tab",
                                               tabPanel("Data",dataTableOutput("df")),
                                               tabPanel("Summary",verbatimTextOutput("summ")),
                                               #if input$factor=5{
                                               
                                               
                                               tabPanel("Plot",plotOutput("plot")))
                                                     
                                 )
                                 )
                                 )
                               ),
                       tabItem("visual",
                               fluidPage(
            
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("ft",label ="choose feature",
                                                  choices = list("Service.Provider"=1,"Techology"=2,"Month"=3,"State_Wise"=4),selected = 1),
                                     
                                     br(),
                                     
                                     radioButtons("multiple","Multi variable analysis",
                                                        choices = list("Service.Provider"=1,
                                                          "Technology"=2,
                                                          "Test_Type"=3,
                                                           "Month"=4
                                                          ),selected = 1

                                     )
                                                  
                                     
                                   ),
                                     
                                   
                                   mainPanel(
                                     tabsetPanel(type = "tab",
                                                 #tabPanel("correction",plotOutput("cor")),
                                                 #tabPanel("Summary",verbatimTextOutput("summ")),
                                                 #if input$factor=5{
                                                 tabPanel("Visualization",plotOutput("feactures"))),
                                     tabsetPanel(type="tab",
                                                 tabPanel("Analysis",plotOutput("mul"))
                                                 )
                                     
                                   )
                                  )
                                 )
                               ),
                       
                      
                       
                               
                      
                       tabItem("correlation",
                               
                               # tabBox(
                               #   tabPanel(title="Jan_Delhi",status = "success",solidHeader = T,plotOutput("Jan_Delhi")),
                               #   tabPanel(title="Feb_Delhi",status='success',solidHeader = T,plotOutput("Feb_Delhi")),
                               #   tabPanel(title="Mar_Delhi",status='success',solidHeader = T,plotOutput("Mar_Delhi")),
                               #   tabPanel(title="AP_Delhi",status='success',solidHeader = T,plotOutput("AP_Delhi"))
                               #   #tabPanel(title="May_Delhi",status='success',solidHeader = T,plotOutput("May_Delhi"))
                               #   
                               #   
                               #   
                               #   
                               #   ),
                             plotOutput("correlation_plot",height=600)
                               
                        
                       ),
                       
                       tabItem("map",
                               
                               leafletOutput("geo",height=600),
                               
                               leafletOutput("signal"),
                               leafletOutput("dataspeed")
                               
                               )
                             
                       
                       
                              
                       
                       
                       
                     )
                   ) 
                               
                               
                       
                       
                       
                       
                     
                    
                   
                   
)