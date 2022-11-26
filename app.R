library(shiny)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(png)

df =read.csv(file="pokemon.csv")
pokemon_list=df$Name

ui <-fluidPage(
              setBackgroundImage(src = "https://assets.pokemon.com//assets/cms2/img/misc/virtual-backgrounds/masters/city.jpg", 
                            shinydashboard = FALSE),
  
              titlePanel(h1("Pokemon searcher", align = "center")),
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(column(width = 3),imageOutput(outputId = "sprite")),
                   
                   textOutput(outputId = "height"),
                   selectInput(
                     "pokemon_choice", "What's your favourite pokemon?", pokemon_list,
                     multiple = FALSE
                   ),
                   
                 ),
               mainPanel(
                 plotOutput(outputId = "stats")
                 
                 
               ))
               )

server <- function(input,output){

  output$sprite = renderImage({
    request_url = paste("https://pokeapi.co/api/v2/pokemon/",
                        tolower(input$pokemon_choice),sep = "")
    res = GET(request_url)
    data= fromJSON(rawToChar(res$content))
    download.file(data$sprites$front_default,'sprite.png', mode = 'wb')
    list(src = "sprite.png",
         contentType = 'image/png',
         weight=400,
         height=300,
         alt = "This is alternate text")
    }, deleteFile = FALSE)
  
  
  output$stats = renderPlot({
    request_url = paste("https://pokeapi.co/api/v2/pokemon/",
                        tolower(input$pokemon_choice),sep = "")
    res = GET(request_url)
    data= fromJSON(rawToChar(res$content))
    pokemon_name = input$pokemon_choice
    title_name=paste("Stats of",pokemon_name) 
    
    barplot(data$stats$base_stat, main=title_name,
            xlab="Pokemon Statistics",legend=TRUE,horiz = FALSE,
            names.arg=data$stats$stat$name,col="#005bd0")
  })
  output$height = renderText({
    request_url = paste("https://pokeapi.co/api/v2/pokemon/",
                        tolower(input$pokemon_choice),sep = "")
    res = GET(request_url)
    data= fromJSON(rawToChar(res$content))
    
    info = c("Height :",data$height,"\n","Weight: ",data$weight)
    
    paste(info,collapse = " ")})
  
}

shinyApp(ui=ui,server=server)