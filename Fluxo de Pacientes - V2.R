library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(maps)
library(geosphere)
library(ggplot2)
library(rworldmap)
library(plyr)
library(data.table)
library(ggthemes)
library(readxl)
library(plyr)
library(rgdal)
library(RColorBrewer)
library(rgdal)
library(maptools)
library(ggrepel)

# Load data
tabela <- read.csv2("https://raw.githubusercontent.com/pablodiasvieira/saudero/master/SIHSUS-2017.csv", header=T, sep=";")
RegSau <- read.csv2("https://raw.githubusercontent.com/pablodiasvieira/saudero/4f364256398a8d892ee5fee690de121db1c04906/Regioes-Saude-RO.csv", header = T, sep = ";", encoding = "UTF-8")
cidades <- read.csv2("https://raw.githubusercontent.com/pablodiasvieira/saudero/master/MuncipiosCodLatLong.csv", header=T,dec=",", sep=";", encoding = "UTF-8")
cidades[,"CodDATASUS"] <- as.integer(regmatches(cidades$"Código.IBGE",
                                                regexpr("......", cidades$"Código.IBGE")))
## DADOS GEOGRAFICOS 
RO <- readOGR("C:/Users/02752345259/Downloads/IBGE/RO/RO", "11MUE250GC_SIR")
RO$CD_GEOCMU <- substr(RO$CD_GEOCMU, 1, 6) ## Ajuste no código dos municípios 

## FILTRO NOS DADOS
# Variávies selecionadas
tabela1<- tabela[,c("MUNIC_RES","MUNIC_MOV","MORTE")]

# Agregando o numero de atendimentos por estabelecimento de saude
QUAT <- ddply(tabela1, c("MUNIC_RES","MUNIC_MOV"), function(x) count(x$MUNIC_MOV))

# Cruzando os códigos dos atendimentos com do IBGE 
trend_data <- left_join(QUAT, cidades[,c(2,4,6,7,8)], by=c("MUNIC_RES"="CodDATASUS") )
trend_data <- left_join(trend_data , cidades[,c(2,4,6,7,8)], by=c("MUNIC_MOV"="CodDATASUS") )
trend_data$id <-as.character(c(1:nrow(trend_data)))

# Pegar somente RONDONIA
trend_data = filter(trend_data, (UF.x == "RO"& UF.y == "RO"))
trend_data = filter(trend_data, (Nome.do.Município.x != Nome.do.Município.y ))

theme_osa = function(base_size=9, base_family="")
{
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.title.x=element_text(vjust=0),
          axis.title.y=element_text(angle=90, vjust=0.3),
          axis.text=element_text(),
          axis.ticks=element_line(colour="black", size=0.25),
          legend.background=element_rect(fill=NA, colour=NA),
          legend.direction="vertical",
          legend.key=element_rect(fill=NA, colour="white"),
          legend.text=element_text(),
          legend.title=element_text(face="bold", hjust=0),
          panel.border=element_rect(fill=NA, colour="black"),
          panel.grid.major=element_line(colour="grey92", size=0.3, linetype=1),
          panel.grid.minor=element_blank(),
          plot.title=element_text(vjust=1),
          strip.background=element_rect(fill="grey90", colour="black", size=0.3),
          strip.text=element_text()
    )
}
# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Fluxo de Pacientes no Estado de RO em 2017"),
                tagList(
                  shinythemes::themeSelector()),
                sidebarLayout(
                  sidebarPanel(
                    # Selecionar o Município
                    selectInput(inputId = "Nom", label = strong("Municipio"),
                                choices = unique(filter(trend_data, UF.x=="RO" )$Nome.do.Município.x),
                                selected = "Porto Velho"),
                    checkboxInput(inputId = "Reg", label = strong("Pintar Regiões de Saúde"), value = TRUE),
                    conditionalPanel(condition = "input.Reg == true",
                                     selectInput(inputId = "Paleta", label = "Paleta de Cores:",
                                                 selected = "Dark2", choices = c("YIGn","Oranges","GnBu","Set1","Dark2","Spectral","PuOr"))),
                    
                    selectInput(inputId = "tema", label = strong("Tema"),
                              choices = c("Solarized","Bw","Void", "Osa"),
                                selected = "Solarized"),
                    
                    # Selecionar a curvatura da seta
                    sliderInput(inputId = "curva", label = strong("Curvatura da Seta:"), 
                                value = -0.3, min = -1,max = 1, step = 0.1,
                                animate = animationOptions(interval = 100)),
                    # Selecionar o tamanho dos nomes
                    checkboxInput(inputId = "munic", label = strong("Nome dos Municipios"), value = TRUE),
                    conditionalPanel(condition = "input.munic == true",
                                     sliderInput(inputId = "l", label = "Letra:",
                                                 min = 0, max = 8, value = 3.2, step = 0.1,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Tamanho do nome dos Municipios"))),
                # O que vai mostrar
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Mapa", plotOutput(outputId = "meugrafico", height = "500px")), #Grafico
                      tabPanel("Dados Filtrados", tableOutput("a")),
                      tabPanel("Cores", tableOutput("c"))))))

# Define server function
server <- function(input, output) {
    # Transformaçoes do Banco de Dados
    cores <- reactive({c("magenta","darkgreen","blue","black")})
    selected_trends <- reactive({
      dados.teste <- filter(trend_data, Nome.do.Município.x != input$Nom & Nome.do.Município.y == input$Nom)
        icone <- data.frame(axi = 0,cor = 0)
        core <- as.character(cores())
        for(i in seq(1,nrow(dados.teste))){
          teste <- dados.teste$freq[i]
          ifelse(teste <= as.numeric(summary(dados.teste$freq)[2]),                                                icone[i,] <-  c(round(as.numeric(summary(dados.teste$freq)[2]), 0), core[1]), 
                 ifelse(teste >  as.numeric(summary(dados.teste$freq)[2]) & teste <= as.numeric(summary(dados.teste$freq)[3]),  icone[i,] <-  c(round(as.numeric(summary(dados.teste$freq)[3]), 0), core[2]), 
                        ifelse(teste >  as.numeric(summary(dados.teste$freq)[3]) & teste <= as.numeric(summary(dados.teste$freq)[5]),  icone[i,] <-  c(round(as.numeric(summary(dados.teste$freq)[5]), 0), core[3]), 
                               icone[i,] <-  c(round(as.numeric(summary(dados.teste$freq)[6]), 0), core[4]  ))))}
          dados.teste$axi <- as.numeric(icone$axi)
          dados.teste$cor<- icone$cor
  return(dados.teste)})
    # Parte estética para legenda: esquema de cores  
    legenda.size <- reactive({cores <- cores()
      legenda <- data.frame(AXI = as.numeric(rownames(as.data.frame(summary(as.factor(selected_trends()$axi))))),
                             N   = as.data.frame(summary(as.factor(selected_trends()$axi)))[,1],
                             COR = cores)
      return(legenda)})
  # Criar um Vetor Geografico do estado de RO
    ro.muni <- reactive ({
      ro.municipios <- merge(fortify(merge(RO, filter(cidades, (UF == "RO")), by.x='CD_GEOCMU', by.y='CodDATASUS'), region = "CD_GEOCMU"),
                             merge(RO, filter(cidades, (UF == "RO")), by.x='CD_GEOCMU', by.y='CodDATASUS')@data, by.x = "id", by.y = "CD_GEOCMU")
      ro.municipio <- merge(ro.municipios, RegSau, by.x="Nome.do.Município", by.y="Municipio")
      return(ro.municipio)
      })
    # Saídas
      # Tabela dos dados
    output$a <- renderTable({SELE <- selected_trends()[,c(1,2,4,5,7,8)]
    names(SELE) <-c("MUNIC_RES","MUNIC_MOV","Movimentações", "Municipio" , "Latitude" ,"Longitude")
    SELE})
      # Tabela das cores
    output$c <- renderTable({legenda <- legenda.size()[,1:3]
    names(legenda)<- c("Intervalo Quartílico", "Qtd Municipios", "Cor")
    legenda})
      # Função para dara saída o Mapa de Flucox
    output$meugrafico <- renderPlot({
    ##mapa
     ggplot() + 
     {if (input$Reg){geom_polygon(data= ro.muni(), aes(long,lat, group=group, fill=rsaude), color = "grey65")}else{
       geom_polygon(data= ro.muni(), aes(long,lat, group=group),fill="#f9f9f9", color = "grey65")}}+
       {if(input$curva==F){geom_segment(data = selected_trends(), aes(x = Longitude.x, y = Latitude.x, 
                                                                      xend = Longitude.y, yend = Latitude.y, color = as.factor(axi), size = as.integer(axi)),
                                        position = "identity", lineend = "butt", arrow=arrow(), alpha = c(0.35))}
         else{geom_curve(data = selected_trends(), aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, 
                                                       yend = Latitude.y, color=as.factor(axi), size = as.integer(axi)),
                         curvature = input$curva, arrow = arrow(length = unit(0.01, "npc")), alpha = c(0.35))}} +
        {if(input$curva==F){geom_segment(data = selected_trends(), aes(x = Longitude.x, y = Latitude.x, 
                            xend = Longitude.y, yend = Latitude.y, color = as.factor(axi), size = as.integer(axi)),
                            position = "identity", lineend = "butt", arrow=arrow(), alpha = c(0.35))}
          else{geom_curve(data = selected_trends(), aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, 
                            yend = Latitude.y, color=as.factor(axi), size = as.integer(axi)),
                          curvature = input$curva, arrow = arrow(length = unit(0.01, "npc")), alpha = c(0.35))}} +  
           coord_equal()+
           {if(input$Reg){scale_fill_brewer(palette=(input$Paleta))}}+
        scale_color_manual(name="Número de \nAtendimentos", values = (cores()), labels = legenda.size()$AXI)+
        scale_size_continuous(name="Número de \nAtendimentos", breaks = legenda.size()$AXI, guide=guide_legend(override.aes = list(color=(cores()))))+
        {if(input$tema == "Solarized"){theme_solarized(light=FALSE)}else{if(input$tema=="Bw"){theme_bw()} else{
        if (input$tema == "Void"){theme_void()}else{theme_osa()}}}}+
        guides(color=F)+
        theme(legend.title = element_text(size = 10, color = "white", face = "bold"),
              legend.justification = c(1, 0), legend.position = c(0.98, 0.6),
              legend.background = element_rect(size=0.5, linetype="solid", color ="white", fill = 'gray50'),
              legend.key = element_blank(), legend.text = element_text(face = "bold",color = "white",size=12)) +
        labs(x="Longitude",y="Latitude")+
        if(input$munic){geom_text_repel(aes(x = Longitude.x, y = Latitude.x, group=NULL,
                        label=Nome.do.Município.x, color = as.character(axi)), size = input$l , 
                        data=selected_trends())}})
}
# Criar o objeto Shiny
shinyApp(ui = ui, server = server)


