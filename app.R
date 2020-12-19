library(shiny)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(treemap)
library(leaflet.minicharts)
library(rsconnect)

#Data Source
#http://www.fao.org/faostat/en/#data/FBS

#Downloaded datasets for this app
data <- read.csv("FAO_Protein_2017.csv",header = T) #For Protein values
data2 <- read.csv("FAO_ProteinPie_2017.csv",header = T) #For Piecharts on map
data3 <- read.csv("FAOSTAT_FoodBalanceProteinNewName1961-2017.csv",header = T) #For line 

foodCols <- names(data2)[6:102]

#Sourcecode/packages
#Displaying relative importance
#https://www.r-graph-gallery.com/treemap.html
#https://www.data-to-viz.com/graph/lollipop.html

#Piechart on map
#https://cran.r-project.org/web/packages/leaflet.minicharts/vignettes/introduction.html

#Map interface
#https://rstudio.github.io/leaflet/basemaps.html

##########################
#UserInterface
ui <- fluidPage(
  
  #App title
  titlePanel("Where do we get protein in our daily diets?"),
  
  #Main panel to display outputs
  mainPanel(
    #Subtitle for the app
    em(h4("Explore source of protein in human diets by selecting the region and the commodity")),
    
    tabsetPanel(type = "tabs",
                
                #Output: Treeplot to show relative importance of protein source
                tabPanel("I. Contribution to total protein intake (Percentage, %)",plotOutput("treeplot")),
                
                #Output: Lollipop chart to display values in order of magnitude
                tabPanel("II. Quantity", plotOutput("lollipop"), 
                         height = "400px",
                         width = "400px"),
                
                #Output: World Map to compare consumption by region
                tabPanel("III. World Map: Commodity by region",leafletOutput("map")),
                
                #Output: World Map to compare consumption by region
                tabPanel("IV. Trend",plotOutput("line")),
                
    #Data Source acknowledgment            
    h5("Data publicly available from FAOSTAT, Food Balances"),
    
    #adding the new div tag for data sources
    tags$div(class="header", checked=NA,
             tags$a(href="http://www.fao.org/faostat/en/#data/FBSH", "Old Food Balance (1961-2013)"),
             tags$a(href="http://www.fao.org/faostat/en/#data/FBS", "New Food Balances (2014-2017)")
    )
    
    ),
  
  #Sidebar layout with inputs 
  absolutePanel(
    h4("Tab I&II - Contribution and Quantity"),

      #Input: Select number of commodity to include
      selectInput("area",label="Select region",choices = data$Area,selected= (data$Area == "World")),
      
      # br() spacing
      br(),
      
      #Input: Select number of commodity to include
      sliderInput(inputId="num",label="The number of commodity to display on Tab I&II",value=10, min=1, max=97),
    helpText("Size of the regtangular indicates relative importance of each commodity."),
    
      # br() spacing
      br(),
    h4("Tab III - Protein supplies by region"),

      #Input: Select commodity to display on the map
      checkboxInput("labels", "Show g of protein supplied per day on the map",value=T),
      selectInput("food", "Select commodity", choices = foodCols, multiple = TRUE, selected = c("Wheat.and.products","Rice.and.products")),
      
      helpText("For optimal display, select up to 5 entries at a time. Size of pie chart indicates total g protein supplied by selected commodities. The default map shows wheat and rice, the top two sources of protein in human diets at a global level."),
      
      # br() spacing
      br(),
      h4("Tab IV - Temporal variation"),
      
      #Input: Select parameters to plot
      sliderInput(inputId="yr",label="Specify year range",min = min(data3$Year),max = max(data3$Year),value=c(1961,2017)),
    
    helpText("Select region and commodity using the steps above."),
      
      fixed = TRUE,
      draggable = TRUE, 
      top = 60, 
      left = "auto", 
      right = 20, 
      bottom = "auto",
      width = 330, 
      height = "auto"
      )
  )
)

#Server
server <-function(input, output) {
  
  #Treeplot
  output$treeplot <- renderPlot({
  data %>%
    filter(Area == input$area) %>%
    arrange(Value) %>%
    mutate(ProteinTotAll=sum(Value)) %>% 
    top_n(input$num, Value) %>% 
    group_by(Item) %>%  
    summarise(ProteinTot=Value/ProteinTotAll*100) %>%
    mutate(across(where(is.numeric), round, 1))%>%
    mutate(treemaplabel=paste(Item, ProteinTot, sep ="\n")) %>%
    treemap(data,
            # data
            index="treemaplabel",
            vSize="ProteinTot",
            type="index",
            
            # Main
            title="",
            palette="Dark2",
            
            # Borders:
            border.col=c("white"),             
            border.lwds=1,                         
            
            # Labels
            fontsize.labels=0.8,
            fontcolor.labels="white",
            fontface.labels=1,            
            bg.labels=c("transparent"),              
            align.labels=c("left", "top"),                                          overlap.labels=0.5,
            inflate.labels=T 
    )
  })
  
  #Lollipop chart
  output$lollipop <- renderPlot({
    data %>%
      filter(Area == input$area) %>%
      arrange(Value) %>%
      top_n(input$num, Value) %>% 
      mutate(Item=factor(Item, Item)) %>%
      ggplot( aes(x=Item, y=Value) ) +
      geom_segment( aes(x=Item ,xend=Item, y=0, yend=Value), color="grey") +
      geom_point(size=5, color="#69b3a2") +
      coord_flip() +
      theme_ipsum(axis_title_size = 20) +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      xlab("") +
      ylab("g/capita/day")
  })
  
  #Map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -0, lat = 20, zoom = 2) %>%
      addMinicharts(
        data2$lng, data2$lat,
        chartdata = data2[,(names(data2) %in% c("Wheat.and.products","Rice.and.products"))],
        type = "pie",
        layerId = data2$Area,
        showLabels = TRUE,
        #time = data2$year,
        width = 60 * sqrt(data2$total) / sqrt(max(data2$total)), transitionTime = 0
      )
  })
  
  #Update piecharts on the map following new inputs
  observe({
    if (length(input$food) == 0) {
      newdata <- 0
    } else {
      newdata <- data2[,input$food]
    }
    maxValue <- max(as.matrix(newdata))
    
    leafletProxy("map") %>%
      updateMinicharts(
        data2$Area, #ID
        chartdata = newdata, #update data
        maxValues = maxValue, #update total g of commodity selected
        type = ifelse(length(input$food) < 2, "polar-area", "pie"), #chart type if no commodity was chosen
        #time = data2$year,
        showLabels = input$labels,
        transitionTime = 0
      )
  })
  
  #Line
  output$line <- renderPlot({
    data3 %>%
      filter(Area == input$area) %>%
      filter(ItemName %in% c(input$food)) %>%
      ggplot(aes(x=Year,y=Value)) +
      geom_point()+
      facet_wrap(~ItemName)+
      geom_smooth(method=lm , color="darkgreen", fill="#69b3a2", se=TRUE)+
      xlim(input$yr[1], input$yr[2]) +
      theme_ipsum() +
      ylab("g protein per day")+
      xlab("Year") 
  })

}

#Launch the app
shinyApp(ui, server)
