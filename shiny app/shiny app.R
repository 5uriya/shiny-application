





library(leaflet)
library(shiny)
library(jsonlite)
library(httr)

ui <- fluidPage(
  titlePanel("Google Distance Matrix API"),
  
  sidebarLayout(
    sidebarPanel(
      h3(textInput(inputId = "Start",label = "Enter the starting point")),
      h3(textInput(inputId = "End",label = "Enter the destination")),
      actionButton(inputId = "Find",label = "Find distance"),
      h3(textOutput(outputId = "Distance")),
      h3(textOutput(outputId = "Time")),
      h3(textOutput(outputId = "Origin")),
      h3(textOutput(outputId = "Destination"))
      
      
    ),
    mainPanel( width = 8,
               
               leafletOutput(outputId = "map")
    ),
    position = c("left","right"),
    fluid = TRUE
    
  ))



server <- function(input,output){
  observeEvent(input$Find,{
    
    library(httr)
    library(jsonlite)

    origin<-input$Start
    destination<-input$End
    destination<-gsub(pattern = " ",replacement = "",destination)
    origin<-gsub(pattern = " ",replacement = "",origin)
    url1<-"https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins="
    url1<-paste(url1,origin,"&destinations=",destination,"&key=AIzaSyB47VpiErStMPVbFHQxueDYN0-hnDu4l-U",sep = "",collapse = NULL)
    sample1<-fromJSON(url1,flatten = TRUE)
    url2<-paste("https://maps.googleapis.com/maps/api/geocode/json?address=",origin,",+CA&key=AIzaSyD7TlapnNso6QUYkh0c17n97qDgEvJBERk",sep = "",collapse = NULL)
    sample2<-fromJSON(url2,flatten = TRUE)
    url3<-paste("https://maps.googleapis.com/maps/api/geocode/json?address=",destination,",+CA&key=AIzaSyD7TlapnNso6QUYkh0c17n97qDgEvJBERk",sep = "",collapse = NULL)
    sample3<-fromJSON(url3,flatten = TRUE)
    
    
    lt1 <- as.vector(sample2$results[6])
    names(lt1) <- NULL
    ltt1<- as.numeric(lt1)
    ln1 <- as.vector(sample2$results[7])
    names(ln1) <- NULL
    lnn1 <- as.numeric(ln1)
    
    lt2 <- as.vector(sample3$results[6])
    names(lt2) <- NULL
    ltt2<- as.numeric(lt2)
    ln2 <- as.vector(sample3$results[7])
    names(ln2) <- NULL
    lnn2 <- as.numeric(ln2)
    
    
    
    
    
    des.out<-sample1$destination_addresses
    ori.out<-sample1$origin_addresses
    num.out<-unlist(sample1[3])
    names(num.out)<- NULL
    output$Distance<- renderText(paste("Distance = ",num.out[2],sep = "",collapse = NULL))
    output$Time<-renderText(num.out[4])
    output$Origin<-renderText(ori.out) 
    output$Destination<-renderText(des.out)
    
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        
        addAwesomeMarkers(lng= c(lnn1,lnn2), lat= c(ltt1,ltt2))
      
    })
    
    
  })
  
}

shinyApp(ui=ui , server = server)