library(dplyr)
library(ggplot2)
library(plotrix)
library(leaflet)
#library(ggmap)
#library(ggalt)
library(mapproj)
library(corrplot)
library("PerformanceAnalytics")
library(xts)
library(sp)
library(readr)

FinalDataset20<- read.csv("FinalDataset20.csv") 
FinalMap20<-read.csv("FinalMap2020.csv")
server <- function(input, output) {
   
   output$plot<-renderPlot({
      if (input$factor=="Service.Provider"){
         test3=FinalDataset20%>%
         group_by(Service.Provider)
         count=count(test3)
         x<-count$n
         lbl<-count$Service.Provider
         percentage<-round(x/sum(x)*100)
         labels_new<-paste(lbl,percentage)
         labels_new
         final_labels<-paste(labels_new,'%',sep = "")
         final_labels
         pie3D(x,labels = final_labels,explode = 0.2,main='total user percentage per serviceprovider',labelcex = 0.8,shade = 0.5,radius = 0.5)
         
      }
      else if (input$factor=="Technology")
      {
         test7=FinalDataset20%>%
         group_by(Technology)
         count=count(test7)
         x<-count$n
         lbl<-count$Technology
         percentage<-round(x/sum(x)*100)
         labels_new<-paste(lbl,percentage)
         labels_new
         final_labels<-paste(labels_new,'%',sep = "")
         pie3D(x,labels = final_labels,explode = 0.2,main='total user percentage per technology',labelcex = 0.8,shade = 0.5,radius = 0.5)
         
         
      }
      else if(input$factor=="Test_Type")
      {
         test7=FinalDataset20%>%
         group_by(Test_Type)
         count=count(test7)
         x<-count$n
         lbl<-count$Test_Type
         percentage<-round(x/sum(x)*100)
         labels_new<-paste(lbl,percentage)
         labels_new
         final_labels<-paste(labels_new,'%',sep = "")
         pie3D(x,labels = final_labels,explode = 0.2,main='total user percentage per Test_Type',labelcex = 0.8,shade = 0.5,radius = 0.5)
         
      }
      
    
      
      
      
      
      })
   output$summ<-renderPrint({
      summary(FinalDataset20)
   })
   
   output$df<-renderDataTable({
      FinalDataset20})
   #multi variable analysis
             output$feactures<-renderPlot({ 
               
                  if (input$ft==1)
                 {
                    #2.Service Provider with the general Poor Signal Strength in the country
                    test1=FinalDataset20%>%
                     group_by(Service.Provider)%>%
                    summarise(avg=mean(Signal_strength))%>%
                    arrange(desc(avg))
                    ggplot(data=test1,aes(x=Service.Provider,y=avg,group=1))+ 
                    theme_bw()+
                    geom_line()+
                   labs(y="Signal_stregth",
                   title="Signalstrenghth per Serviceproviders")+
                   geom_line(color="red")+
                   geom_point()
         
                  }
                else if(input$ft==2)
                {
                      test2=FinalDataset20%>%
                      group_by(Technology)%>%
                      summarise(avg=mean(Signal_strength))%>%
                      arrange(desc(avg))
                     ggplot(data=test2,aes(x=Technology,y=avg,group=1))+ 
                      theme_bw()+
                      geom_line()+
                      labs(y="Signal_stregth",
                     title="Signalstrenghth per Technology")+
                      geom_line(color="red")+
                      geom_point()
                }
                
                else if(input$ft==3)
                {
                   test3=FinalDataset20%>%
                   group_by(Month)%>%
                   summarise(avg=mean(Signal_strength))%>%
                   arrange(desc(avg))
                   ggplot(data=test3,aes(x=Month,y=avg,group=1))+ 
                   theme_bw()+
                   geom_line()+
                   labs(y="Signal_stregth",
                   title="Signalstrenghth varies in month_Wise")+
                   geom_line(color="red")+
                   geom_point()
                }
                else if(input$ft==4)
                {
                   test4=FinalDataset20%>%
                      group_by(State_Wise)%>%
                      summarise(avg=mean(Signal_strength))%>%
                      arrange(desc(avg))
                      month<-c("M.P","Orrisa","Delhi","Kolkata","Mah","UP East","Mumbai","UP west","Raj","A.P","Kar","Punjab","Harn","Bihar","Guja","chennia","WB","JK","HP","Assam","T.N","North East","kerala")
                      ggplot(data=test4,aes(month,y=avg, group=1))+ 
                      theme_bw()+
                      geom_line()+
                      labs(y="Signal_stregth",
                      title="Signalstrenghth varies in  State_Wise")+
                      geom_line(color="red")+
                      geom_point()  
                   
                }
                #all the plot
                else if(input$ft==2 && input$ft1==7)
                {
                   
                      test5=FinalDataset20%>%
                      group_by(Service.Provider,Technology,Test_Type)%>%
                      summarise(avg=mean(Data.Speed.Mbps.))%>%
                      arrange(desc(avg))
                      ggplot(test5,aes(x=Technology,y=avg,fill=Test_Type))+ 
                      geom_bar(stat="identity", position="dodge")
                }
                
                   
                  })
             output$mul<-renderPlot({
                if (input$multiple==1)
                {
                      test2=FinalDataset20%>%
                      group_by(Service.Provider,Technology,Test_Type)%>%
                      summarise(avg_dataspeed=mean(Data.Speed.Mbps.))%>%
                      arrange(desc(avg_dataspeed))
                      ggplot(test2,aes(x=Service.Provider,y=avg_dataspeed,fill=Technology))+ 
                      geom_bar(stat="identity", position="dodge")
                }
                else if(input$multiple==2)
                {
                      test2=FinalDataset20%>%
                      group_by(Service.Provider,Technology,Test_Type)%>%
                      summarise(avg_dataspeed=mean(Data.Speed.Mbps.))%>%
                      arrange(desc(avg_dataspeed))
                      ggplot(test2,aes(x=Technology,y=avg_dataspeed,fill=Service.Provider))+ 
                      geom_bar(stat="identity", position="dodge")
                }
               else if(input$multiple==3)
               {
                  test2=FinalDataset20%>%
                     group_by(Service.Provider,Technology,Test_Type)%>%
                     summarise(avg_dataspeed=mean(Data.Speed.Mbps.))%>%
                     arrange(desc(avg_dataspeed))
                     ggplot(test2,aes(x=Technology,y=avg_dataspeed,fill=Test_Type))+ 
                     geom_bar(stat="identity", position="dodge")
               }
                
                else if(input$multiple==4)
                {
                   test3=FinalDataset20%>%
                      group_by(Month,Service.Provider)%>%
                      summarise(avg=mean(Data.Speed.Mbps.))%>%
                      arrange(desc(avg))
                      ggplot(data=test3,aes(x=Month,y=avg,col=Service.Provider,shape==Service.Provider,group=1))+ 
                      theme_bw()+
                      geom_line()+
                      labs(y="Data.Speed.Mbps",
                     title="Data.Speed.Mbps varies in month_Wise")+
                      geom_line(color="red")+                      
                         geom_point()
               
                   
                }
                
             })
             #correlation
             output$correlation_plot<-renderPlot({
                
                 df=FinalDataset20%>%
                   group_by(Service.Provider)
                   my_data <- FinalMap20[, c(4,5)]
                  chart.Correlation(my_data, histogram=TRUE, pch=10)


             })

   
  
      
         
     
   
   
   
   #code for map
             g1=FinalDataset20%>%
               group_by(State_Wise,lattitude,longitude)%>%
               summarise(avg_dataspeed=mean(Data.Speed.Mbps.),avg_signalStrengthl=mean(Signal_strength))
output$geo<-renderLeaflet({   #Stamen.Toner
      
      leaflet()%>%
      addProviderTiles("OpenTopoMap")%>%
      addMarkers(data=FinalMap20,lat=~lattitude,lng=~longitude,popup=~paste("<h3 style='color:blue'>Details</h3>",
                                                                            "<b>State: </b>",State_Wise,"<br>",
                                                                           "<b>Avg_dataspeed:</b>",avg_dataspeed,"<br>",
                                                                           "<b>Avg_signalStrength:</b>",avg_signalStrengthl,"<br>",
                                                                              sep = "" )) 
        })
 output$signal<-renderLeaflet({
    FinalMap20$signalStrength_level = cut(FinalMap20$avg_signalStrengthl, 
                                          breaks = c(-110,-100,-85,-71,-60), right=FALSE,
                                          labels = c("Nosignal[-110-(-100))", "Poor[-100-(-85))", "Good[-85-(-71))",
                                                     "Excellent[-71-(-60))"
                                          ))
    # Define a color pallete corresponding to the magnitude ranges
    pal = colorFactor(palette = c("yellow", "red", "black"), domain=FinalMap20$signalStrength_level)
    
    # Create the map object & add circle marker
    leaflet(data=FinalMap20) %>% 
       addProviderTiles("Stamen.Toner") %>% 
       addCircleMarkers(lng = ~ longitude, 
                        lat= ~ lattitude, 
                        color = ~ pal(signalStrength_level), #use the pallete
                        label = paste("Level=", FinalMap20$avg_signalStrength, "Type=",
                                      FinalMap20$signalStrength_level,
                                      "stae=",FinalMap20$State_Wise),
                        clusterOptions = markerClusterOptions(freezeAtZoom=4))
    
     })
 
 # data speed
 output$dataspeed<-renderLeaflet({
    FinalMap20$dataspeed_level = cut(FinalMap20$avg_dataspeed, 
                                          breaks = c(0.47,5.74,10.44,19.62), right=FALSE,
                                          labels = c("Poor[0.47-5.74)", "Moderate[5.74-10.44)", "Good[10.44-19.7)"
                                                     
                                          ))
    # Define a color pallete corresponding to the magnitude ranges
    pal = colorFactor(palette = c("blue", "red", "black"), domain=FinalMap20$dataspeed_level)
    
    # Create the map object & add circle marker
    leaflet(data=FinalMap20) %>% 
       addProviderTiles("Stamen.Toner") %>% 
       addCircleMarkers(lng = ~ longitude, 
                        lat= ~ lattitude, 
                        color = ~ pal(dataspeed_level), #use the pallete
                        label = paste("Level=", FinalMap20$avg_dataspeed, "Type=", FinalMap20$dataspeed_level,FinalMap20$State_Wise),
                        
                        clusterOptions = markerClusterOptions(freezeAtZoom=5)
       )
                 
    
 })
 
 
  
   #infor box for 2020page
   output$plan<-renderInfoBox({
      sp=FinalDataset20%>%
         group_by(Service.Provider)%>%
         summarise(avg=mean(Data.Speed.Mbps.))
      
         
      if (input$ft1==1)
      {
         
         valueBox(table(FinalDataset20$Service.Provider)[4],"JIO Avg_Dpeed:11.7Mbps",color="navy")
         
      }
      else if(input$ft1==2){
         valueBox(table(FinalDataset20$Service.Provider)[1],"AIRTEL Avg_Dspeed:5.55Mbps",color="navy",width = 4)
      }
      else if(input$ft1==3)
      {
         valueBox(table(FinalDataset20$Service.Provider)[3],"IDEA Avg_Dspeed:6.34Mbps",color="navy")
      }
      else if(input$ft1==4)
      {
         valueBox(table(FinalDataset20$Service.Provider)[5],"VADOFONE Avg_Dspeed:6.76Mbps",color="navy")
      }
      else if(input$ft1==5)
      {
         valueBox(table(FinalDataset20$Service.Provider)[2],"CELLONE Avg_Dspeed:2.21Mbps",color="navy")
      }
      
      

   })
   
   
}