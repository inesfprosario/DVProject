library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(dplyr)
library(highcharter)
library(shinyWidgets)

# >>>>>> Ficheiro necessários

# Tab Geral,  Tab Abstencao e Tab Nulos e Brancos
RDetalhes <- read.csv("data/ResultadosDetalhados.csv", sep = ",", header = TRUE)
AbsBrNul <- read.csv("data/AbstencaoBrancosNulos.csv", sep = ",", header = TRUE)

# Tab Resultados Por Distrito
MaisVotadosDistrito <- read.csv("data/MaisVotadosDistrito.csv", sep = ",", header = TRUE)
map <- readOGR("data/pt_distritos_4326.shp", layer = "pt_distritos_4326") 
AbsBrNul.Distrito <- read.csv("data/AbstencaoBrancosNulosDistrito.csv", sep = ",", header = TRUE)

# Tab Resultados Por Concelho
MaisVotadosConcelho <- read.csv("data/MaisVotadosConcelho.csv", sep = ",", header = TRUE)
mapconcelhos <- readOGR("data/pt_concelhos_4326.shp", layer = "pt_concelhos_4326")
AbsBrNul.Concelho <- read.csv("data/AbstencaoBrancosNulosConcelho.csv", sep = ",", header = TRUE)

# Tab Mandatos
mandatos.original <- read.csv('data/Mandatos.csv')

# >>>>>> UI

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".leaflet-container { background: transparent; }" ))),
    
    titlePanel(p('Eleições legislativas em Portugal', style = "color:#000")),
    
    tabsetPanel(
      
      tabPanel("Geral",
               sidebarLayout(
                 sidebarPanel(
                   div("Resultados globais", style = "font-weight:bold; font-size:16px" ),
                   p('Apresentam-se os resultados globais, no Território Nacional e Estrangeiro, das Eleições Legislativas em Portugal, desde 1991 a 2015.'),
                   p('Destacam-se o PS e PSD, que conquistam ao longo dos anos o maior número de votos, liderando alternadamente os resultados das legislativas.'),
                   
                   br(),
                   
                   selectInput(inputId = "anoativo", label = "Seleccione o ano",
                               choices = c(2015,2011,2009,2005,2002,1999,1995,1991)),
                   br(),
                   
                   htmlOutput("results"),
                  
                   a (href="https://www.pordata.pt/", "Fonte: Pordata")),
                 
                 mainPanel(highchartOutput(outputId ='barchartgeral', height = "800"))
               )),
      
      tabPanel("Resultados por distrito",
               sidebarLayout(
                 sidebarPanel(
                   div("Resultados no território continental, por distrito", style = "font-weight:bold; font-size:16px" ),
                   
                   p('A análise, por distrito, dos resultados obtidos no território continental realça as diferenças nas preferências políticas entre o Norte e o Sul do país.'),
                   
                   br(),
                   
                   selectInput(inputId = "anoativo2", label = "Seleccione o ano",
                               choices = c(2015,2011,2009,2005,2002,1999,1995,1991)),
                   
                   selectInput(inputId = "variavel", label = "Apresentar por",
                               choices = c("Partido mais votado", "Abstenção",  "Votos Brancos", "Votos Nulos")),
                 
                 a (href="https://www.pordata.pt/", "Fonte: Pordata")),
                 
                 mainPanel(leafletOutput(outputId = "map", height = "800", width= "500" ))
               )),

      tabPanel("Resultados por concelho",
               sidebarLayout(
                 sidebarPanel(
                   div("Resultados no território continental, por concelho", style = "font-weight:bold; font-size:16px" ),
                   
                   p('À semelhança do que acontece na distribuição de votos por distrito, também por concelho destacam-se como maiores forças politicas o Partido Social Democrata (PSD) e o Partido Socialista (PS). 
                     '),
                   
                   br(),
                   
                   selectInput(inputId = "anoativo4", label = "Seleccione o ano",
                               choices = c(2015,2011,2009,2005,2002,1999,1995,1991)),
                   
                   selectInput(inputId = "variavel2", label = "Apresentar por",
                               choices = c("Partido mais votado", "Abstenção",  "Votos Brancos", "Votos Nulos")),
                   
                   a (href="https://www.pordata.pt/", "Fonte: Pordata")),
                 
                 mainPanel(leafletOutput(outputId = "map2", height = "800", width= "500" ))
               )),     
    
      tabPanel("Mandatos",
               sidebarLayout(
                 sidebarPanel(
                   div("Repartição dos mandatos", style = "font-weight:bold; font-size:16px" ),
                   
                   p('A conversão dos votos em mandatos faz-se de acordo com o sistema de representação proporcional e o método da média mais alta de Hondt.'),
                   p('Em consequência, a maioria dos assentos da Assembleia da República têm as cores do PS e PSD.'),
                   
                   br(),
                   
                   selectInput(inputId = "anoativo3", label = "Seleccione o ano",  
                               choices = c(2015,2011,2009,2005,2002,1999,1995,1991)),
                   
                   a (href="https://www.eleicoes.mai.gov.pt/", "Fonte: SGMAI")),
                 
                 mainPanel(highchartOutput(outputId = "mandatospiechart", height = "800", width = "800"))
               )),   
      
      
      tabPanel("Abstenção", 
               sidebarLayout(
                 sidebarPanel( 
                   div("Evolução da abstenção", style = "font-weight:bold; font-size:16px" ),
                   
                   p('A abstenção é um dos maiores problemas, que tem vindo a ser analisado, nas várias eleições realizadas em Portugal. Vários factores, como o descontentamento social ou económico, têm sido apontados como principais causas para o constante decréscimo na participação ativa no exercício do direito de voto.', align="justify"),
                 
                 a (href="https://www.pordata.pt/", "Fonte: Pordata")),
                 
                 mainPanel(highchartOutput('columnchartabstencao'))
              )),
      
      tabPanel("Votos brancos e nulos", 
               sidebarLayout(
                 sidebarPanel( 
                   div("Evolucao dos votos brancos e nulos", style = "font-weight:bold; font-size:16px" ),
                   
                   p('A par de um aumento da taxa de abstenção, tem-se também assistido ao aumento do peso de votos brancos, que atingiram em 2011 a maior percentagem desde as primeiras Eleições legislativas, em 1975.', align="justify"),
                   
                   a (href="https://www.pordata.pt/", "Fonte: Pordata")),
                 
                 mainPanel(highchartOutput('linechartbrnul'))
               ))

  ))

# >>>>>> Server

server <- function(input, output){
  
  output$barchartgeral <- renderHighchart({
    
    f.RDetalhes <- RDetalhes[(RDetalhes$Ano == input$anoativo) & (RDetalhes$CodNomeDistrito == "PORTUGAL"), ]
    
    max_freq <- 50 #max(df$frequency)

    datalabels <- "{point.PVotos}% | {point.Votos:,.0f} votos"
    
    highchart() %>%
      hc_title(text="<strong>Percentagem do total de votos por partido</strong>", style=list(fontSize="16px")) %>%
      
      hc_add_series(pointWidth=20, f.RDetalhes, "bar", 
                    hcaes(y = PVotos), name = 'Percentagem do total de votos', 
                    dataLabels = list(align = "left", enabled = TRUE, format = datalabels, allowOverlap = FALSE, inside = FALSE)) %>%
      
      hc_legend(enabled = FALSE) %>%
      
      hc_yAxis(
        title = list(text = 'Percentage of AirBnB records'),
        labels = list(format = '{value}%'),
        max = max_freq,
        visible = FALSE
        
      ) %>%
      
      hc_colors("#c0d73c") %>%
      
      hc_xAxis(categories = f.RDetalhes$Sigla) %>%
      hc_tooltip(enabled = FALSE)
    
  })
 
   
  output$map <- renderLeaflet({
    
    
    if (input$variavel == "Partido mais votado") {
      
      datafiltered <- MaisVotadosDistrito[which(MaisVotadosDistrito$Ano == input$anoativo2), ]
      orderdistricts <- match(map@data$Distrito, datafiltered$CodNomeDistrito)
      map@data <- datafiltered[orderdistricts, ]
      
      factpal <- colorFactor(c("#016699", "#F26524", "#FFC000", "#F9A8A8"), unique(map$Sigla))
      
      labels <- sprintf("<strong>%s</strong><br>
                        %s: %g%s<br>
                        %s: %g%s<br>
                        %s: %g%s<br>
                        %s: %g%s<br>
                        Outros: %g%s<br>", map$NomeDistrito, map$Sigla, map$PVotos, "%", map$Sigla_2, map$PVotos_2, "%", map$Sigla_3, map$PVotos_3, "%", map$Sigla_4, map$PVotos_4, "%", map$PVotos_outros, "%") %>% lapply(htmltools::HTML)
      
      l <- leaflet(map, options = leafletOptions(zoomControl = FALSE,
                                                 minZoom = 7, maxZoom = 7,
                                                 dragging = FALSE)) %>%
        
        setView(-8.2, 39, zoom = 7) %>% 
        addPolygons(
          fillColor = ~factpal(Sigla),
          color = "white",
          dashArray = "1",
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          label = labels,
          labelOptions = labelOptions(
            opacity = 0.8,
            direction = "auto"),
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)
        ) %>%
        
        leaflet::addLegend(pal = factpal, values = ~Sigla, opacity = 1, title = "Legenda", position = "bottomright")
      
    }
    
    else {

      datafiltered <- AbsBrNul.Distrito[which(AbsBrNul.Distrito$Ano == input$anoativo2), ]
      orderdistricts <- match(map@data$Distrito, datafiltered$CodNomeDistrito)
      map@data <- datafiltered[orderdistricts, ]
      
      if (input$variavel == "Abstenção") { 

        bins <- c(25, 30, 35, 45, 50, 55)
        
        pal <- colorBin("Greys", domain = AbsBrNul.Distrito$TaxaAbstencao, bins = bins)
        
        labels <- sprintf("<strong>%s</strong><br>%g%s", map$NomeDistrito, map$TaxaAbstencao, "%") %>% lapply(htmltools::HTML)
        
        l <- leaflet(map, options = leafletOptions(zoomControl = FALSE,
                                                   minZoom = 7, maxZoom = 7,
                                                   dragging = FALSE)) %>%
          
          setView(-8.2, 39, zoom = 7) %>% 
          addPolygons(
            fillColor = ~pal(TaxaAbstencao),
            color = "white",
            dashArray = "1",
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            label = labels,
            labelOptions = labelOptions(
              opacity = 0.8,
              direction = "auto"),
            highlight = highlightOptions(
              weight = 3,
              color = "#666",
              dashArray = "",
              fillOpacity = 1,
              bringToFront = TRUE)
          ) %>%
          

          leaflet::addLegend(pal = pal, values = ~map$TaxaAbstencao, opacity = 1, title = "Legenda", labFormat = labelFormat(suffix = "%", between = "% - "), position = "bottomright")
        
      }
      
      else {
        if (input$variavel == "Votos Brancos"){

          bins <- c(0.5, 1.3, 2.1, 2.9, 3.7, 4.5)
          
          pal <- colorBin("Greys", domain = AbsBrNul.Distrito$Brancos, bins = bins)
          
          labels <- sprintf("<strong>%s</strong><br>%g%s", map$NomeDistrito, map$Brancos, "%") %>% lapply(htmltools::HTML)
          
          l <- leaflet(map, options = leafletOptions(zoomControl = FALSE,
                                                     minZoom = 7, maxZoom = 7,
                                                     dragging = FALSE)) %>%
            
            setView(-8.2, 39, zoom = 7) %>%
            addPolygons(
              fillColor = ~pal(map$Brancos),
              color = "white",
              dashArray = "1",
              weight = 2,
              opacity = 1,
              fillOpacity = 1,
              label = labels,
              labelOptions = labelOptions(
                opacity = 0.8,
                direction = "auto"),
              highlight = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)
            ) %>%
            
            leaflet::addLegend(pal = pal, values = ~map$Brancos, opacity = 1, title = "Legenda", labFormat = labelFormat(suffix = "%", between = "% - "), position = "bottomright")}
        else {
          
          bins <- c(0.8, 1.2, 1.6, 2.0, 2.4, 2.8)
          
          pal <- colorBin("Greys", domain = AbsBrNul.Distrito$Nulos, bins = bins)
          
          labels <- sprintf("<strong>%s</strong><br>%g%s", map$NomeDistrito, map$Nulos, "%") %>% lapply(htmltools::HTML)
          
          l <- leaflet(map, options = leafletOptions(zoomControl = FALSE,
                                                     minZoom = 7, maxZoom = 7,
                                                     dragging = FALSE)) %>%
            
            setView(-8.2, 39, zoom = 7) %>%
            addPolygons(
              fillColor = ~pal(map$Nulos),
              color = "white",
              dashArray = "1",
              weight = 2,
              opacity = 1,
              fillOpacity = 1,
              label = labels,
              labelOptions = labelOptions(
                opacity = 0.8,
                direction = "auto"),
              highlight = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)
            ) %>%
            
            leaflet::addLegend(pal = pal, values = ~map$Nulos, opacity = 1, title = "Legenda", labFormat = labelFormat(suffix = "%", between = "% - "), position = "bottomright" )
          
        }}}})
  
  output$map2 <- renderLeaflet({
    
    if (input$variavel2 == "Partido mais votado") {
      
      datafiltered2 <- MaisVotadosConcelho[which(MaisVotadosConcelho$Ano == input$anoativo4) , ]
      mapconcelhos@data <- datafiltered2
      
      factpal <- colorFactor(c("#5b656b","#016699", "#F26524","#FFC000", "#F9A8A8"), unique(MaisVotadosConcelho$Sigla))

      labels <- sprintf("<strong>%s</strong><br>
                        %s: %g%s<br>
                        %s: %g%s<br>
                        %s: %g%s<br>
                        %s: %g%s<br>
                        Outros: %g%s<br>", mapconcelhos$NomeConcelho, mapconcelhos$Sigla, mapconcelhos$PVotos, "%", mapconcelhos$Sigla_2, mapconcelhos$PVotos_2, "%", mapconcelhos$Sigla_3, mapconcelhos$PVotos_3, "%", mapconcelhos$Sigla_4, mapconcelhos$PVotos_4, "%", mapconcelhos$PVotos_outros, "%") %>% lapply(htmltools::HTML)

               
      l <- leaflet(mapconcelhos, options = leafletOptions(zoomControl = FALSE,
                                                          minZoom = 7, maxZoom = 7,
                                                          dragging = FALSE)) %>%
        
        setView(-8.2, 39, zoom = 7) %>% 
        addPolygons(
          fillColor = ~factpal(Sigla),
          color = "white",
          dashArray = "1",
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          label = labels,
          labelOptions = labelOptions(
            opacity = 0.8,
            direction = "auto"),
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 1,
            bringToFront = TRUE)
        ) %>%
        
        leaflet::addLegend(pal = factpal, values = ~Sigla, opacity = 1, title = "Legenda", position = "bottomright")
      
    }
    
    else {
      
      datafiltered2 <- AbsBrNul.Concelho[which(AbsBrNul.Concelho$Ano == input$anoativo4), ]
      mapconcelhos@data <- datafiltered2
      
      if (input$variavel2 == "Abstenção") { 
        
        bins <- c(25, 30, 35, 45, 50, 55)
        
        pal <- colorBin("Greys", domain = AbsBrNul.Concelho$TaxaAbstencao, bins = bins)
        
        labels <- sprintf("<strong>%s</strong><br>%g%s", mapconcelhos$NomeConcelho, mapconcelhos$TaxaAbstencao, "%") %>% lapply(htmltools::HTML)
        
        l <- leaflet(mapconcelhos, options = leafletOptions(zoomControl = FALSE,
                                                            minZoom = 7, maxZoom = 7,
                                                            dragging = FALSE)) %>%
          
          setView(-8.2, 39, zoom = 7) %>% 
          addPolygons(
            fillColor = ~pal(mapconcelhos$TaxaAbstencao),
            color = "white",
            dashArray = "1",
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            label = labels,
            labelOptions = labelOptions(
              opacity = 0.8,
              direction = "auto"),
            highlight = highlightOptions(
              weight = 3,
              color = "#666",
              dashArray = "",
              fillOpacity = 1,
              bringToFront = TRUE)
          ) %>%
          
          leaflet::addLegend(pal = pal, values = ~mapconcelhos$TaxaAbstencao, opacity = 1, title = "Legenda", labFormat = labelFormat(suffix = "%", between = "% - "), position = "bottomright") 
        
      }
      
      else {
        if (input$variavel2 == "Votos Brancos"){
          
          bins <- c(0.5, 1.3, 2.1, 2.9, 3.7, 4.5)
          
          pal <- colorBin("Greys", domain = AbsBrNul.Concelho$Brancos, bins = bins)
          
          labels <- sprintf("<strong>%s</strong><br>%g%s", mapconcelhos$NomeConcelho, mapconcelhos$Brancos, "%") %>% lapply(htmltools::HTML)
          
          l <- leaflet(mapconcelhos, options = leafletOptions(zoomControl = FALSE,
                                                              minZoom = 7, maxZoom = 7,
                                                              dragging = FALSE)) %>%
            
            setView(-8.2, 39, zoom = 7) %>%
            addPolygons(
              fillColor = ~pal(mapconcelhos$Brancos),
              color = "white",
              dashArray = "1",
              weight = 2,
              opacity = 1,
              fillOpacity = 1,
              label = labels,
              labelOptions = labelOptions(
                opacity = 0.8,
                direction = "auto"),
              highlight = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)
            ) %>%
            
            leaflet::addLegend(pal = pal, values = ~mapconcelhos$Brancos, opacity = 1, title = "Legenda", labFormat = labelFormat(suffix = "%", between = "% - "), position = "bottomright")}
        else {
          
          bins <- c(0.8, 1.2, 1.6, 2.0, 2.4, 2.8)
          
          pal <- colorBin("Greys", domain = AbsBrNul.Concelho$Nulos, bins = bins)
          
          labels <- sprintf("<strong>%s</strong><br>%g%s", mapconcelhos$NomeConcelho, mapconcelhos$Nulos, "%") %>% lapply(htmltools::HTML)
          
          l <- leaflet(mapconcelhos, options = leafletOptions(zoomControl = FALSE,
                                                              minZoom = 7, maxZoom = 7,
                                                              dragging = FALSE)) %>%
            
            setView(-8.2, 39, zoom = 7) %>%
            addPolygons(
              fillColor = ~pal(mapconcelhos$Nulos),
              color = "white",
              dashArray = "1",
              weight = 2,
              opacity = 1,
              fillOpacity = 1,
              label = labels,
              labelOptions = labelOptions(
                opacity = 0.8,
                direction = "auto"),
              highlight = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)
            ) %>%
            
            leaflet::addLegend(pal = pal, values = ~mapconcelhos$Nulos, opacity = 1, title = "Legenda", labFormat = labelFormat(suffix = "%", between = "% - "), position = "bottomright" )
          
        }}}})
  
  output$columnchartabstencao <- renderHighchart({

    x <- c("Taxa de Abstenção")
    
    y <- sprintf("{point.%s}: .2f", 
                 c("TaxaAbstencao"))
    
    tltip <- "<strong><i>{point.Ano}</i></strong><br>
                  <strong>Taxa de Abstenção</strong>: {point.TaxaAbstencao}%"

    highchart() %>%
      hc_title(text="<strong>Taxa de Abstenção</strong>", style=list(fontSize="16px")) %>%
      
      hc_add_series(AbsBrNul, "column", 
                    hcaes(y = TaxaAbstencao), name = 'Taxa de Abstenção') %>%
      hc_yAxis(
        title = list(text = 'Taxa de Abstenção'),
        labels = list(format = '{value}%')
      ) %>%
      
      hc_colors("#c0d73c") %>%
      
      hc_legend(enabled=FALSE) %>%
      
      hc_xAxis(categories = AbsBrNul$Ano) %>%
      hc_tooltip(useHTML = TRUE, headerFormat = '', pointFormat = tltip)
    
  })

  output$mandatospiechart <- renderHighchart({
    mandatos <- mandatos.original[(mandatos.original$Ano == input$anoativo3), ]
    
    highchart() %>%
      hc_title(text="<strong>Distribuição dos mandatos</strong>",
               style=list(fontSize="16px")) %>%
      
      hc_chart(type="pie") %>%
      
      hc_tooltip(enabled = TRUE) %>%
      
      hc_add_series_labels_values(labels=mandatos$Sigla, 
                                  value=mandatos$Mandatos,
                                  name="Total", 
                                  showInLegend=FALSE,
                                  startAngle=-90, endAngle=90,
                                  innerSize=400,
                                  colors = mandatos$Cor)

  })  
  
  output$linechartbrnul <- renderHighchart({
    
    
    highchart()%>%
      hc_title(text="<strong>Distribuição de votos</strong>", style=list(fontSize="16px")) %>%
      
      hc_add_series(AbsBrNul,'line', 
                    hcaes(y=Nulos), name= 'Percentagem de Votos Nulos') %>%
      hc_add_series(AbsBrNul,'line',
                    hcaes(y=Brancos), name= 'Percentagem de Votos em Branco') %>%
      hc_yAxis(
        title=list(text= 'Percentagem de votos'),
        labels=list(format='{value}%')
      ) %>%
      
      hc_colors(c("#c0d73c", "#5b656b")) %>%
    
      hc_xAxis(categories=AbsBrNul$Ano) 
    
  })
      
  output$results = renderText({
    paste("Votos válidos: ", "<br>", "<b>", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "Validos"],"% </b> |", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "VValidos"],"votos","<br><br>",
          "Em branco: ", "<br>", "<b>", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "Brancos"],"% </b> |", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "VBrancos"],"votos","<br><br>",
          "Nulos: ", "<br>", "<b>", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "Nulos"],"% </b> |", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "VNulos"],"votos","<br><br>",
          #"<br>","Total de inscritos: ", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "Inscritos"],"<br><br>",
          "Abstenção: ", "<br>", "<b>", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "TaxaAbstencao"],"% </b> |", AbsBrNul[(AbsBrNul$Ano == input$anoativo), "VAbstencao"],"inscritos","<br><br>")

  })
  
}


# shinyApp()
shinyApp(ui = ui, server = server)