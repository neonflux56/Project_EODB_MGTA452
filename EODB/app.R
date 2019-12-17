




require(devtools)

require(tableHTML)
require(shinythemes)
require(shiny)
require(shinydashboard)

require(leaflet)
require(rworldmap)
require(stringr)
require(ggplot2)
require(tidyverse)
require(keras)
require(lime)
require(tidyquant)
require(rsample)
require(recipes)
require(yardstick)
require(corrr)
require(ggforce)
require(scales)



#Fetch Data
Xtrain <- as.data.frame(read.csv("Xtrain.csv"))
Ytrain <- as.data.frame(read.csv("Ytrain.csv"))
Xian <- as.data.frame(read.csv("finaldf.csv"))
latitude <- as.data.frame(read.csv("latitude.csv"))
OutputRank <- as.data.frame(read.csv("finalOutput.csv"))

OutputRank <- OutputRank %>%
    mutate(Rank = seq(1,length(OutputRank$Country),1))
OutputRank <- inner_join(OutputRank,latitude,by = "Country")


header <- dashboardHeader(
    title = span("Start-Up Global",style = "font-size:25px"),
    titleWidth = 300
)

sidebar <- dashboardSidebar(sidebarMenuOutput('menu'), width = 160)


body <- dashboardBody(
    
    tags$style(make_css(list('.box',c('font-size'),c('13px')))),
    
    #shinyDashboardThemes(theme = "grey_dark"),
    
    tabItems(
        # homepage tab content
        tabItem(tabName="hp",
                fluidRow(
                    box(title= span('Project Introduction',style="font-size:20px"),uiOutput("projinfo"),height = 150,width = 12,solidHeader = TRUE,status="primary")),
                fluidRow(
                    box(title = span('Global Overview',style="font-size:20px"),plotOutput("plot1"),height = 480,width = 6,solidHeader = TRUE,background ="black"),
                    box(title=span('Best/Worst Performing Countries',style="font-size:20px"),plotOutput("plot2"),height = 480,solidHeader = TRUE,width = 6,background ="black")),
                fluidRow(
                    box(title = span('Feature Description',style="font-size:20px"), uiOutput("featinfo"),solidHeader = TRUE,height = 480,width = 6,background ="black"),
                    box(title = span('Feature Correlation',style="font-size:20px"),solidHeader = TRUE,plotOutput("plot3"),height = 480,width = 6,background ="black"))),
        
        ## second tab content
        tabItem(tabName="countryselect",
                fluidRow(
                    box(title = span('Select Country',style="font-size:20px") , selectInput("CountrySelected",label = "",choices = Xian$Country), height = 150,solidHeader = TRUE,width = 2,background = "black" ),
                    box(title = span('EODB Rank',style="font-size:20px"),textOutput("txtOutput1"),height = 150,solidHeader = TRUE,width = 2,   tags$head(tags$style("#txtOutput1{color: orange;font-size: 45px;font-style: bold;}")),background = "black" ),
                    box(title = span('Feature Table for 2019',style="font-size:20px"),tableOutput("table1"), height = 150,solidHeader = TRUE,width = 8,tags$head(tags$style('#table1{font-size:9px;}')),background = "black"),
                    fluidRow(
                        box(title = span('Map',style="font-size:20px"),leafletOutput("plot6"), height= 470,solidHeader = TRUE,width = 4,background = "black"),
                        box(title = span('Urban Population Percentage',style="font-size:20px"),plotOutput("plot4"), height= 470,solidHeader = TRUE,width = 4,background = "black" ),
                        box(title = span('GDP Trend',style="font-size:20px"),plotOutput("plot5"), height= 470,solidHeader = TRUE,width = 4,background = "black" ))
                    
                )),
        
        #third tab 
        tabItem(tabName="rankselect",
                fluidRow(
                    box(title = span('Select Rank',style="font-size:20px"), selectInput("RankSelected",label = "",choices = OutputRank$Rank), height = 150,solidHeader = TRUE,width = 2,background = "black"),
                    box(title = span('Country',style="font-size:20px"),textOutput("txtOutput2"),height = 150,solidHeader = TRUE,width = 2,   tags$head(tags$style("#txtOutput2{color: orange;font-size: 30px;font-style: bold;}")),background ="black"),
                    box(title = span('Feature Table for 2019',style="font-size:20px"),tableOutput("table2"), height = 150,solidHeader = TRUE,width = 8,tags$head(tags$style('#table2{font-size:9px;}')),background = "black")),
                fluidRow( 
                    box(title = span('Map',style="font-size:20px"),leafletOutput("plot9"), height= 470,solidHeader = TRUE,width = 4,background = "black"),
                    box(title = span('Urban Population Percentage',style="font-size:20px"),plotOutput("plot7"), height= 470,solidHeader = TRUE,width = 4,background = "black"),
                    box(title = span('GDP Trend',style="font-size:20px"),plotOutput("plot8"), height= 470,solidHeader = TRUE,width = 4,background = "black"))
        ),
        
        #fourth
        tabItem(tabName="moreinfo",
                fluidRow(
                    box(title= span('Contact Info',style="font-size:20px"),uiOutput("moreinfo1"),height = 150,width = 12,solidHeader = TRUE,status="primary"))
                
                )
        
        
        
    ))


ui <- dashboardPage(header,sidebar,body,title = "Start-Up Global")





server <- function(input, output){
    
    output$menu<-renderMenu({sidebarMenu(menuItem(text=span('Home',style="font-size:18px"),tabName = 'hp'),menuItem(text= span('Country Measure',style="font-size:18px"),tabName = "countryselect"),menuItem(text= span('Rank Measure',style="font-size:18px"),tabName = "rankselect"),menuItem(text= span('Contact Info',style="font-size:18px"),tabName = "moreinfo") ) })  
    
    output$moreinfo1 <- renderUI({
        HTML(paste("<p>","To get more details on this project, please visit the following link-","<br>"),
             paste("<a href =","https://github.com/neonflux56/Project_EODB_MGTA452",">","Project on Github","</a>"),
             paste("<p>","For further queries and suggestions, please contact through the below link-","<br>"),
             paste("<a href =","https://ashishgupta.netlify.com/",">","Contact","</a>"))
    })
    
    
    
    
    
    output$projinfo <- renderUI({
        HTML(paste("<p>","The goal of our project is to showcase important socioeconomic factors that makes a country conducive to new business and rank the countries in order of their Ease of Doing Business (EODB) index. This provides a birds-eye view into each country from the perspective of a business owner. To achieve this, we used various indicators from the years 2006 through 2018, and predicted the extent of easiness of doing business in the year 2019 using a deep learning neural network model, obtaining an accuracy of around 85% as a result."))
    })
    
    output$featinfo <- renderUI({
        HTML(paste0("<p>","<b>","1. Log_GDP","</b>", ": Log transformation of GDP per capita in current U.S dollars. GDP per capita is gross domestic product divided by midyear population. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products." ,"<br>"  ),
             paste0("<b>","2. NB ","</b>", ": Multi-Categorical feature representing the number of new limited liability corporations registered in the calendar year. The country is categorized as ‘high’ if the number of new business registered larger than 20000, ‘medium’ if between 1000 and 20000, ‘low’ if smaller than 1000." ,"<br>"  ),
             paste0("<b>","3. Cost ","</b>", ": Multi-Categorical feature representing the cost to register a business normalized by presenting it as a percentage of gross national income (GNI) per capita. The country is categorized as ‘High’ if the cost in percentage larger than 75, ‘Medium High’ if between 25 and 75, ‘medium low’ if between 25 and 10, ‘low’ if smaller than 10." ,"<br>"  ),
             paste0("<b>","4. LowerSecondary ","</b>", ": Ratio of people entering last grade of lower secondary education to the actual age population entering last grade." ,"<br>"  ),
             paste0("<b>","5. Percent_Urban_Population ","</b>", ": Urban population ratio with respect to the whole population." ,"<br>"  ),
             paste0("<b>","6. procedures ","</b>", ": Start-up procedures are those required to start a business, including interactions to obtain necessary permits and licenses and to complete all inscriptions, verifications, and notifications to start operations." ,"<br>"  ),
             paste0("<b>","7.	sexratio ","</b>", ": Male to female ratio." ,"<br>"  ),
             paste0("<b>","8.	time_days  ","</b>", ": Number of days required to start the business" ,"<br>"  )
        )})
    
    
    output$plot1 <- renderPlot({
        ExtractCode <- Xian %>%
            group_by(Country,Code) %>%
            summarise(size = n()) %>%
            select(-size)
        OutputRank <- OutputRank %>%
            rowwise() %>%
            mutate(Country, Code = ExtractCode$Code[which(ExtractCode$Country==Country)])
        mapped_data <- joinCountryData2Map(OutputRank, joinCode = "ISO3", nameJoinColumn = "Code") 
        mapCountryData(mapped_data, nameColumnToPlot = "Rank", colourPalette=c('#1E6C03', '#E5FEDC'), addLegend='TRUE', borderCol = "grey", mapTitle = "Rank - Ease Of Doing Business")
    })
    
    output$plot2 <- renderPlot({
        plot_countries = c('New Zealand', 'Finland', 'Norway', 'Canada', 'Eritrea', 'Central African Republic', 'Iraq', 'Chad')
        bottom_countries = c('Eritrea', 'Equatorial Guinea', 'Central African Republic', 'Iraq', 'Chad')
        index1 <- Xian %>%
            filter(Year != "2019") %>%
            filter(Country %in% plot_countries) %>%
            select(Country, Year, EODB_index)
        Plot2 <- ggplot(data=index1, aes(x=Year, y=EODB_index, color = Country, group = Country)) +geom_line() + geom_point() + labs(x="Year", y = "rank of ease of doing business")
        Plot2
    })
    
    output$plot3 <- renderPlot({
        Y2 <- as.data.frame(Ytrain) %>%
            mutate(easy = ifelse(as.data.frame(Ytrain)$V2 == 1, 1, 0))
        corrr_analysis <- Xtrain %>%
            mutate(EODB_Measure = as.vector(Y2$easy)) %>%
            correlate() %>%
            focus(EODB_Measure) %>%
            rename(feature = rowname) %>%
            arrange(abs(EODB_Measure)) %>%
            mutate(feature = as_factor(feature)) %>%
            arrange(desc(abs(EODB_Measure)))
        corrr_analysis2 <- corrr_analysis %>%
            filter(!startsWith(as.character(feature),"Country"))
        
        corrr_analysis2 %>%ggplot(aes(x = EODB_Measure, y = fct_reorder(feature, desc(EODB_Measure)))) + geom_point() +
            # Positive Correlations - Contribute 
            geom_segment(aes(xend = 0, yend = feature), 
                         color = palette_light()[[2]], 
                         data = corrr_analysis2 %>% filter(EODB_Measure > 0)) +
            geom_point(color = palette_light()[[2]], 
                       data = corrr_analysis2 %>% filter(EODB_Measure > 0)) +
            # Negative Correlations - Prevent 
            geom_segment(aes(xend = 0, yend = feature), 
                         color = palette_light()[[1]], 
                         data = corrr_analysis2 %>% filter(EODB_Measure < 0)) +
            geom_point(color = palette_light()[[1]], 
                       data = corrr_analysis2 %>% filter(EODB_Measure < 0)) +
            # Vertical lines
            geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
            geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
            geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
            # Aesthetics
            theme_tq() +
            labs(title = "Feature correlation in training data",y = "Feature",x = "Correlation Measure")
    })
    
    output$plot4 <- renderPlot({
        Countryset <- input$CountrySelected 
        UP <- Xian %>%
            group_by(Country) %>%
            summarise(Urban_Portion = mean(Percent_Urban_Population)) %>%
            mutate(Rural_Portion = 100 - Urban_Portion)
        UP1 <- UP %>%
            gather("Urban_Portion","Rural_Portion",2:3)  %>%
            arrange(Country) %>%
            filter(Country == Countryset)
        colnames(UP1) <- c("Country","Class","Perc")
        Plot4<- ggplot(UP1, aes(x = 2, y = Perc, fill = Class)) + geom_bar(stat = "identity", color = "white") + coord_polar(theta = "y", start = -pi/2)+ 
            theme_void()+xlim(0.5, 2.5)
        Plot4
    })
    
    output$plot5 <- renderPlot({
        Countryset <- input$CountrySelected
        gdpline <- Xian %>%
            select (Country, Year, Log_GDP) %>%
            mutate (GDP = exp(Log_GDP)) %>%
            filter(Country == Countryset)
        Plot5 <- ggplot(data = gdpline, aes(x = Year, y = GDP, group = 1)) + geom_line() + geom_point()
        Plot5
    })
    
    output$plot6 <- renderLeaflet({
        Countryset <- input$CountrySelected
        leaflet() %>% addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 5)) %>% addMarkers( lat = OutputRank$latitude[which(OutputRank$Country== Countryset)], lng = OutputRank$longitude[which(OutputRank$Country== Countryset)],  popup = Countryset)
    })
    
    output$table1 <- renderTable ({
        Countryset <- input$CountrySelected 
        Table1<- Xian %>%
            filter(Year == '2019') %>%
            filter(Country == Countryset) %>%
            mutate(Log_GDP = format(round(Log_GDP, 2), nsmall = 2)) %>%
            mutate(Lower_Secondary = format(round(LowerSecondary, 2), nsmall = 2)) %>%
            mutate(Urban_Population = format(round(Percent_Urban_Population, 2), nsmall = 2)) %>%
            select(-Country,-index,-Y,-EODB_index,-LowerSecondary,-Percent_Urban_Population)
        Table1
    })
    
    output$txtOutput1 <- renderText({
        Countryset <- input$CountrySelected 
        Text1<- OutputRank %>% filter(Country == Countryset) 
        Text1$Rank
    })
    
    output$txtOutput2 <- renderText({
        Rankset <- input$RankSelected
        Text1<- OutputRank %>% filter(Rank == Rankset) 
        Countryset <- Text1$Country
        Countryset
    })
    
    output$table2 <- renderTable ({
        Rankset <- input$RankSelected
        Text1<- OutputRank %>% filter(Rank == Rankset) 
        Countryset <- Text1$Country
        Table2<- Xian %>%
            filter(Year == '2019') %>%
            filter(Country == Countryset) %>%
            mutate(Log_GDP = format(round(Log_GDP, 2), nsmall = 2)) %>%
            mutate(Lower_Secondary = format(round(LowerSecondary, 2), nsmall = 2)) %>%
            mutate(Urban_Population = format(round(Percent_Urban_Population, 2), nsmall = 2)) %>%
            select(-Country,-index,-Y,-EODB_index,-LowerSecondary,-Percent_Urban_Population)
        Table2
    })
    
    output$plot7 <- renderPlot({
        Rankset <- input$RankSelected
        Text1<- OutputRank %>% filter(Rank == Rankset) 
        Countryset <- Text1$Country 
        UP <- Xian %>%
            group_by(Country) %>%
            summarise(Urban_Portion = mean(Percent_Urban_Population)) %>%
            mutate(Rural_Portion = 100 - Urban_Portion)
        UP1 <- UP %>%
            gather("Urban_Portion","Rural_Portion",2:3)  %>%
            arrange(Country) %>%
            filter(Country == Countryset)
        colnames(UP1) <- c("Country","Class","Perc")
        Plot7<- ggplot(UP1, aes(x = 2, y = Perc, fill = Class)) + geom_bar(stat = "identity", color = "white") + coord_polar(theta = "y", start = -pi/2)+
            theme_void()+xlim(0.5, 2.5)
        Plot7
    })
    
    output$plot8 <- renderPlot({
        Rankset <- input$RankSelected
        Text1<- OutputRank %>% filter(Rank == Rankset) 
        Countryset <- Text1$Country
        gdpline <- Xian %>%
            select (Country, Year, Log_GDP) %>%
            mutate (GDP = exp(Log_GDP)) %>%
            filter(Country == Countryset)
        Plot8 <- ggplot(data = gdpline, aes(x = Year, y = GDP, group = 1)) + geom_line() + geom_point()
        Plot8
    })
    
    output$plot9 <- renderLeaflet({
        Rankset <- input$RankSelected
        Text1<- OutputRank %>% filter(Rank == Rankset) 
        Countryset <- Text1$Country
        leaflet() %>% addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 5)) %>% addMarkers( lat = OutputRank$latitude[which(OutputRank$Country== Countryset)], lng = OutputRank$longitude[which(OutputRank$Country== Countryset)],  popup = Countryset)
    })
    
    
}





shinyApp(ui,server)















