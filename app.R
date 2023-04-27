##-----------------Library imports---------------------------------------------------##
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(glue)
## Also include this else dashboard deployment fails
library(maps)


##-------------------Obtain the world map --------------------------------------------##
world1 <- map_data("world")

##-------------------------Get Covid Data----------------------------------------------##
covid <- read.csv("data/WHO-COVID-19-global-data.csv")

covid$Month <- format(as.Date(covid$Date_reported), "%m")
covid$Year <- format(as.Date(covid$Date_reported), "%Y")
covid$Day <- format(as.Date(covid$Date_reported), "%d")
covid$weekday <- wday(covid$Date_reported, label=TRUE, abbr=FALSE)
##-------------------------------Create selection values --------------------------------##
cases <- select(covid, New_cases:Cumulative_deaths)


covid_min_date <- min(covid$Date_reported)
covid_max_date <- max(covid$Date_reported)

### TODO  - DESIGN CONSIDERATIONS 
### Add logic to make the map more zoomed in using coord_fixed ratio
### Fix plotly hover info to only work with custom text
### Change font sizes of titles and ticks. Also look into themes
### Figure out how to get data directly from website
### Bring logic for different colors if cases/deaths/vaccinations
### Add vaccination data and add map visualization and color logic for that as well
### Look into proper mapping using leaflet/folium/choropleth

### TODO - DATA MANIPULATION CONSIDERATIONS
### Add logic to update the regions in descending order and rename regions to official names.
### Filter data to focus on vaccination ratio/infection ratio

##-----------------------------UI Section-------------------------------------------------------------------------## 
ui <- dashboardPage(
  # Header
  dashboardHeader(title="Covid Dashboard"),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("World analysis",
               tabName="map"),
      menuItem("Per region analysis",
               tabName="region")
      ## TODO add vaccination section either as a section or as part of the world and region analysis
    )
  ),
  ## App content
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              ### Probably reduce size or use other widgets
              fluidRow(
                box(selectInput("case_death",
                                "Case/Death distribution",
                                choices=colnames(cases))),
                box(dateInput("minimum_date", "Choose a Date:", value = "2020-04-29"))
              ),
              plotlyOutput("wmap"),
              br(),
              br(),
              plotlyOutput("reg1")),
     # Region analysis. TODO: Add logic to sort graphs by region with most cases/deaths and show names and totals for each region 
    tabItem(tabName = "region",
            selectInput("case_death1",
                        "Case/Death distribution",
                        choices=colnames(cases)),
            
            selectInput("year", "Select Year", choices=unique(covid$Year)),
            fluidRow(
              box( 
                textOutput("treg2"), 
                plotlyOutput("reg2")),
              
              box(
                textOutput("treg3"),
                plotlyOutput("reg3")
              )),
            fluidRow(
                
                box(
                  textOutput("treg4"),
                  plotlyOutput("reg4")),
                
                box(
                  textOutput("treg5"),
                  plotlyOutput("reg5")
                )),
            fluidRow(
              
                  box( 
                    textOutput("treg6"),
                    plotlyOutput("reg6")),
                  
                  box(
                    textOutput("treg7"),
                    plotlyOutput("reg7")
                  ))
              
            
           )
  )))
     


server <- function(input,output) {
  
  # World map of cases/deaths 
  output$wmap <- renderPlotly ({
    data1 <-select(covid, region=Country, Date_reported, input$case_death)
    data2 <- data1 %>% filter(Date_reported==input$minimum_date)
    mapdata <- left_join(world1, data2, by="region")
    p<-mapdata%>% ggplot(aes(x = long,
                             y = lat, 
                             text=paste("Country: ", region, "<br>",input$case_death, get(input$case_death), "<br>"))) + 
    geom_polygon(aes(group=group,fill=get(input$case_death)))+ 
    labs(title = glue('World map of {input$case_death} as at {input$minimum_date}'), fill=input$case_death) + 
    coord_fixed(ratio=1)+ 
    theme( axis.ticks = element_blank(),# remove the vertical grid lines
           panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_blank(),
           panel.background = element_blank(),
           axis.text.x=element_blank(), #remove x axis labels
           axis.ticks.x=element_blank(), #remove x axis ticks
           axis.text.y=element_blank(),  #remove y axis labels
           axis.ticks.y=element_blank(),  #
           plot.title = element_text(hjust = 0.5))+ 
    theme(text = element_text(family = "Times New Roman"))
    
    ggplotly(p, tooltip=c("text"))
  }) 
  
  
  # Regional bar chart of total cases/deaths
  output$reg1 <- renderPlotly({
    res<- covid %>% filter(Date_reported == input$minimum_date) %>% group_by(WHO_region) %>%
      summarise(total = sum(get(input$case_death)))
    g<-res%>%arrange(desc(total))%>%ggplot(aes(y=total,
                                               x=WHO_region,
                                               fill=WHO_region,
                                               text=paste("Date Reported: ", input$minimum_date, "<br>",input$case_death, total, "<br>")))+
    geom_col()+ 
    labs(title = glue('WHO Region total {input$case_death} as at {input$minimum_date}'),
         y=input$case_death)+coord_flip()+
    xlab("") + 
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          panel.background = element_blank())+
    theme(text = element_text(family = "Times New Roman"))
    ggplotly(g, tootltip=c("text"))
  })
  
  ## Function to create per region bar graphs
  regiongraph <- function(region, years, cases, color){
    graph<-covid%>%subset(Year == years)%>%filter(WHO_region == region)%>%ggplot(aes(y=get(cases), x=Date_reported,text=paste(
      cases, get(cases), "<br>"
    )))+geom_col(fill=color)+labs(y=cases)+
      theme( # remove the vertical grid
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank() )
    ggplotly(graph, tootltip=c("text"))
    
  }
  output$reg2 <- renderPlotly({
    mwaka <<- input$year
    regiongraph('EURO', mwaka, input$case_death1, '#c8d65b')
  })

  output$reg3 <- renderPlotly({
    mwaka <<- input$year
    regiongraph('AFRO', mwaka, input$case_death1, '#0a71d5')
  })
  output$reg4 <- renderPlotly({
    mwaka <<- input$year
    regiongraph('EMRO', mwaka, input$case_death1, '#00ae8f')
  })
  output$reg5 <- renderPlotly({
    mwaka <<- input$year
    regiongraph('SEARO', mwaka, input$case_death1, '#5200ae')
  })
  output$reg6 <- renderPlotly({
    mwaka <<- input$year
    regiongraph('AMRO', mwaka, input$case_death1, '#ffbb30')
  })
  output$reg7 <- renderPlotly({
    mwaka <<- input$year
    regiongraph('WPRO', mwaka, input$case_death1, '#c12592')
  })

 
    }
    
 


#RENDER APP
shinyApp(ui, server)
