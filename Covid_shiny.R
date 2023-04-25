library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
# #Obtain the world map --------------------------------------------
world1 <- map_data("world")

## Get Covid Data------------------------------------
covid <- read.csv("data/WHO-COVID-19-global-data.csv")

## Create selection values --------------
cases <- select(covid, New_cases:Cumulative_deaths)


covid_min_date <- min(covid$Date_reported)
covid_max_date <- max(covid$Date_reported)




ui <- dashboardPage(
  
  dashboardHeader(title="Covid Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World analysis",
               tabName="map"),
      menuItem("Per region analysis",
               tabName="region")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              selectInput("case_death",
                          "Case/Death distribution",
                          choices=colnames(cases)),
              dateInput("minimum_date", "Choose a Date:", value = "2020-04-29"),
              ## TO DO: CHANGE TITLE TO BE BOLD AND CENTERED USING CSS/ANY OTHER TOOL
              textOutput("title1"),
              plotlyOutput("wmap"),
              ## TO DO: CHANGE TITLE TO BE BOLD AND CENTERED USING CSS/ANY OTHER TOOL
              textOutput("title2"),
              ## TO DO: CHANGE GRAPH TO BE ARRANGED IN DESCENDING ORDER
              plotlyOutput("reg1")),
      
    tabItem(tabName = "region",
            selectInput("case_death1",
                        "Case/Death distribution",
                        choices=colnames(cases)),
            fluidRow(
              box( 
                title = "EMRO",
                plotlyOutput("reg2")),
              
              box(
                title = "AFRO",
                plotlyOutput("reg3")
              )),
            fluidRow(
                
                box(
                  title = "EURO",
                  plotlyOutput("reg4")),
                
                box(
                  title = "SEARO",
                  plotlyOutput("reg5")
                )),
            fluidRow(
              
                  box( 
                    title = "AMRO",
                    plotlyOutput("reg6")),
                  
                  box(
                    title = "WPRO",
                    plotlyOutput("reg7")
                  ))
              
            ## TO DO: ADD MORE SECTIONS TO ALSO ANALYZE VACCINATION TRENDS AND MAYBE INTRODUCE ANIMATION OF A CERTAIN PERIOD
            
           )
  )))
     


server <- function(input,output) {
  
  # World map of cases/deaths 
  output$title1 <- renderText({
    paste("World map of ", input$case_death, "as at ", input$minimum_date, ".")
  })
  output$wmap <- renderPlotly ({
    data1 <-select(covid, region=Country, Date_reported, input$case_death)
    data2 <- data1 %>% filter(Date_reported==input$minimum_date)
    mapdata <- left_join(world1, data2, by="region")
    p<-mapdata%>% ggplot(aes(x = long, y = lat, text=paste(
                                                           "Country: ", region, "<br>",
                                                           input$case_death, get(input$case_death), "<br>"
                                                           ))) + geom_polygon(aes(group=group, fill=get(input$case_death)))+ labs(fill=input$case_death)+ coord_fixed() 
    ggplotly(p, tooltip=c("text"))
  }) 
  
  output$title2 <- renderText({
    paste("WHO Region total ", input$case_death, "as at", input$minimum_date, "." )
  })
  # Regional bar chart of total cases/deaths
  output$reg1 <- renderPlotly({
    res<- covid %>% filter(Date_reported == input$minimum_date) %>% group_by(WHO_region) %>%
      summarise(total = sum(get(input$case_death)))
    g<-res%>%ggplot(aes(y=total,x=WHO_region, fill=WHO_region,text=paste(
      "Date Reported: ", input$minimum_date, "<br>",
      input$case_death, total, "<br>"
    )))+geom_col()+ labs(y=input$case_death)+coord_flip()
    ggplotly(g, tootltip=c("text"))
  })
    ## TO DO: USE FUNCTIONS TO RECREATE THE REPEATED GRAPHS
  ## TO DO: FIX BINS OF HISTOGRAMS TO BE MAYBE WEEKLY OR MONTHLY
  output$reg2 <- renderPlotly({
    g2<-covid%>% filter(WHO_region == 'EMRO')%>% ggplot(aes(y=get(input$case_death1), x=Date_reported, fill=WHO_region))+geom_col()+ labs(y=input$case_death1)
    ggplotly(g2)
  })
  
  output$reg3 <- renderPlotly({
    g3<-covid%>% filter(WHO_region == 'AFRO')%>% ggplot(aes(y=get(input$case_death1), x=Date_reported, fill=WHO_region))+geom_col()+ labs(y=input$case_death1)
    ggplotly(g3)
  })
  output$reg4 <- renderPlotly({
    g4<-covid%>% filter(WHO_region == 'EURO')%>% ggplot(aes(y=get(input$case_death1), x=Date_reported, fill=WHO_region))+geom_col()+ labs(y=input$case_death1)
    ggplotly(g4)
  })
  output$reg5 <- renderPlotly({
    g5<-covid%>% filter(WHO_region == 'SEARO')%>% ggplot(aes(y=get(input$case_death1), x=Date_reported, fill=WHO_region))+geom_col()+ labs(y=input$case_death1)
    ggplotly(g5)
  })
  output$reg6 <- renderPlotly({
    g6<-covid%>% filter(WHO_region == 'AMRO')%>% ggplot(aes(y=get(input$case_death1), x=Date_reported, fill=WHO_region))+geom_col()+ labs(y=input$case_death1)
    ggplotly(g6)
  })
  output$reg7 <- renderPlotly({
    g7<-covid%>% filter(WHO_region == 'WPRO')%>% ggplot(aes(y=get(input$case_death1), x=Date_reported, fill=WHO_region))+geom_col()+ labs(y=input$case_death1)
    ggplotly(g7)
  })
    
 
    }
    
 


#RENDER APP
shinyApp(ui, server)
