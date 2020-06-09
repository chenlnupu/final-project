library(shiny)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(leaflet)
library(leaflet.extras)
library(rvest)


list1=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W",
        "X","Y","Z")
list2=c()
list3=c()

ui <- fluidPage(
  titlePanel("Search cocktail"),
  sidebarPanel(
    actionButton(inputId = "button",label = "Random Search"),
    selectInput(inputId = "Initial",label = "Search by Initial",choices = c("-",list1)),
    selectInput(inputId = "cock","Choose a cocktail",c("-")),
    selectInput(inputId = "barname","Choose a bar in Bay area",c("-")),
    p(style = "font-family:Impact", "Link:"),
    p(style = "font-family:Impact",
      "See populor cocktail:",
      a("Top 50 cocktail",
        href = "https://vinepair.com/articles/50-most-popular-cocktails-world-2017/")),
    
    p(style = "font-family:Impact",
      "See cocktail video:",
      a("Cocktail in 10 mins",
        href = "https://www.youtube.com/watch?v=IRkM8jsG-hY"))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Random search",
               textOutput("name_of_random"),
               uiOutput("image1"),
               textOutput("instruction1"),
               tableOutput("ingredient1")),
      tabPanel("Search by name", 
               uiOutput("image"),
               textOutput("instruction"),
               tableOutput("ingredient")),
      tabPanel("Map", 
               tableOutput("table"),
               leafletOutput("mymap"))
    )
  )
  
  
)

server <- function(input, output, session) {
  
  data1 <- eventReactive(input$button, {
    fromJSON("https://www.thecocktaildb.com/api/json/v1/1/random.php",flatten=TRUE)
    #data$drinks$strDrinkThumb
  })
  
  
  observe({
    name_by_ini<-fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/search.php?f={f}",
                                   f=input$Initial),flatten=TRUE)
    x<-name_by_ini$drinks$strDrink
    
    updateSelectInput(session, "cock", choices =c("-",x))
    
  })
  
  observe({
    html <- read_html("https://www.diffordsguide.com/pubs-and-bars/search?city=30&offset=0&limit=48")
    name_of_bar<-html%>%html_nodes("a.cell.small-6.medium-3.large-2.box")%>%
      html_nodes("h3")%>%
      html_text()
  
    updateSelectInput(session, "barname", choices =c("-",name_of_bar))
    
  })
  
  
  
  output$image<- renderUI({
    if(input$cock!="-"){
      
      endpoint<-str_glue("https://www.thecocktaildb.com/api/json/v1/1/search.php")
      r<-GET(endpoint,query=list(s=input$cock))
      json<-content(r,as = "text",encoding='UTF-8')
      temp<-fromJSON(json,flatten = TRUE)
      
      #temp<-fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/search.php?s={s}",s=input$cock))
      image_temp<-temp$drinks$strDrinkThumb[1]
      tags$img(src=image_temp, width =300, height = 300)
    }
    else{
      image_temp<-"https://cf.ltkcdn.net/cocktails/images/std/251453-675x450-non-alcoholic-cocktails.jpg" 
      tags$img(src=image_temp, width = 500, height = 300)
    }
  })
  
  output$name_of_random <- renderText({
    
    if(input$button){
      paste("Name of cocktail",data1()$drinks$strDrink,sep=": ")
    }
    
  })
  
  output$image1<- renderUI({
    if(input$button){
      image_temp<-data1()$drinks$strDrinkThumb
      tags$img(src=image_temp, width = 300, height = 300)
    }
    else{
      image_temp<-"https://cdn.diffords.com/contrib/encyclopedia/2018/12/5c0fc5824b9d2.jpg" 
      tags$img(src=image_temp, width = 500, height = 500)
    }
  })
  
  output$instruction <- renderText({
    if(input$cock!="-"){
      endpoint<-str_glue("https://www.thecocktaildb.com/api/json/v1/1/search.php")
      r<-GET(endpoint,query=list(s=input$cock))
      json<-content(r,as = "text",encoding='UTF-8')
      temp_text<-fromJSON(json,flatten = TRUE)
      
      #temp_text$drinks$strInstructions[1]
      paste("Instruction",temp_text$drinks$strInstructions[1],sep=": ")
    }
    
  })
  
  output$instruction1 <- renderText({
    if(input$button){
      #data1()$drinks$strInstructions[1]
      paste("Instruction",data1()$drinks$strInstructions[1],sep=": ")
    }
    
  })
  
  
  
  output$ingredient <- renderTable({
    if(input$cock!="-"){
      
      endpoint<-str_glue("https://www.thecocktaildb.com/api/json/v1/1/search.php")
      r<-GET(endpoint,query=list(s=input$cock))
      json<-content(r,as = "text",encoding='UTF-8')
      temp_text<-fromJSON(json,flatten = TRUE)
      
      for(i in 22:36){
        if(!is.na(temp_text$drinks[1,i])){
          #list2<-append(list2,as.numeric(i-21))
          list3<-append(list3,temp_text$drinks[1,i])
          
        }
      }
      
      table<-tibble(ingredient = list3)
    }
    
  })
  
  output$ingredient1 <- renderTable({
    if(input$button){
      for(i in 22:36){
        if(!is.na(data1()$drinks[1,i])){
          #list2<-append(list2,as.numeric(i-21))
          list3<-append(list3,data1()$drinks[1,i])
          
        }
      }
      
      table<-tibble(ingredient = list3)
    }
  })
  
  output$mymap <- renderLeaflet({
    
    r <- GET(
      "https://maps.googleapis.com/maps/api/place/nearbysearch/json",
      query = list(
        key = "AIzaSyDUPoYKs4ufp8n1dWgSGunD_WVwJolkCMw",
        location = "37.7749,-122.419",
        radius = 10000,
        types = "food",
        name = input$barname
      )
    )
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    bar<-fromJSON(json, flatten = TRUE)
    
    map_object = leaflet()
    map_base = addProviderTiles(map_object, providers$OpenStreetMap)
    
    cocktailglassIcon <- makeIcon(
      iconUrl = "https://img.icons8.com/color/72/cocktail.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 0, iconAnchorY = 94
    )
    
    leaflet(data = map_base) %>% addTiles() %>%
      addMarkers(lng = ifelse(input$barname!="-",bar$results$geometry.location.lng[1],-122.419),
                 #ifelse(HVData$nh3>5, "black", "red")
                 lat = ifelse(input$barname!="-",bar$results$geometry.location.lat[1],37.7749), 
                 icon = cocktailglassIcon,
                 label = paste(bar$results$name[1],"rating:",bar$results$rating[1]))
  })
  
}        


# Run the application 
shinyApp(ui = ui, server = server)

