library(shiny)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(png)
library(ggplot2)
library

df =read.csv(file="pokemon.csv")
pokemon_list=df$Name
df = df[,2:10]

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
                 tabsetPanel(
                   tabPanel("Plot",plotOutput(outputId = "stats")),
                   tabPanel("Dataset",dataTableOutput("dataset"),
                            tags$head(tags$style("#dataset table {background-color: white; }", media="screen", type="text/css"))
                   )
                 )
                 
                 
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
    
    print(data$stats$base_stats)
    
    ggplot2::ggplot() + ggplot2::geom_bar(mapping = aes(x=data$stats$stat$name,
                                                        y=data$stats$base_stat),
                                          stat = "identity",color="blue",
                                          fill="#005bd0"
                                          )+
      ggplot2::ggtitle(title_name)+
      ggplot2::xlab("Pokemon Statistics")+
      ggplot2::ylab("Score")

  })
  output$height = renderText({
    request_url = paste("https://pokeapi.co/api/v2/pokemon/",
                        tolower(input$pokemon_choice),sep = "")
    res = GET(request_url)
    data= fromJSON(rawToChar(res$content))
    
    info = c("Height :",data$height,"\n","Weight: ",data$weight)
    
    paste(info,collapse = " ")})
  
    

  output$dataset <- renderDataTable({
    request_url = paste("https://pokeapi.co/api/v2/pokemon/",
                        tolower(input$pokemon_choice),sep = "")
    res = GET(request_url)
    data= fromJSON(rawToChar(res$content))
    
    bg_color <- c("#FFD700", "#00FF00")
    table = data.frame(df)
    
    return (table)
    })
  
  
}

shinyApp(ui=ui,server=server)