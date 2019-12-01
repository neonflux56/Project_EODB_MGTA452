
    



#pkgs <- c("keras", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr","stringr","rworldmap")
#install.packages(pkgs)
#install.packages("leaflet")
#install.packages("tableHTML")
#install.packages('rsconnect')
#install.packages("shinydashboard")
#install.packages("shinythemes")
require(devtools)
#install_github("nik01010/dashboardthemes")



require(tableHTML)
require(shinythemes)
require(shiny)
require(shinydashboard)
require(devtools)
#require(dashboardthemes)
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
gdp <- read.csv("Data/GDP.csv")
cost <- read.csv("Data/cost.csv")
nb <- read.csv("Data/NB.csv")
LowerS <- read.csv("Data/LowerS.csv")
percent_UP <- read.csv("Data/percent_urban_population.csv")
procedures <- read.csv("Data/procedures.csv")
sexratio <- read.csv("Data/sexratio.csv")
time <- read.csv("Data/time.csv")
countries <- read.delim("Data/countries.txt", header = FALSE, sep = "\n")
EODB_index <- read.csv("Data/EODB-index.csv")
latitude <- as.data.frame(read.csv("Data/latitude.csv"))




# Preprocess Data
cost1 <- cost %>%
    mutate(Cost = cost$Cost...Men....of.income.per.capita. , Country = cost$Economy , Year = cost$DB.Year) %>%
    select( Country, Year, Cost ) %>%
    drop_na() %>%
    filter(!Year %in% c(2019,2020,2005)) %>%
    mutate(Cost, Cost = ifelse(Cost>75,"High",ifelse(Cost>25,"Medium High",ifelse(Cost>10,"Medium Low","Low"))))


gdp1 <- gdp %>%
    select (Country.Code , Country.Name, X2000:X2018) %>%
    gather("Year","GDP",3:21) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    mutate ( Log_GDP = log(GDP)) %>%
    select(Country = Country.Name, Code = Country.Code, Year,Log_GDP) %>%
    filter(!Year %in% c(2000,2001,2002,2003,2004,2005))


nb1 <- nb %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    rowwise() %>%
    mutate(X2017,X2017 = mean(c(X2015,X2016),na.rm = TRUE)) %>%
    mutate(X2018, X2018 = mean(c(X2016,X2017),na.rm = TRUE)) %>%
    gather("Year","NB",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year,NB) #%>%
#mutate(NB, NB = ifelse(NB>20000, "High",ifelse(NB>1000,"Medium","Low"))) 

LowerS1 <- LowerS %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","LowerSecondary",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, LowerSecondary)


percent_UP1 <- percent_UP %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","Percent_Urban_Population",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, Percent_Urban_Population)

procedures1 <- procedures %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","procedures",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, procedures)

sexratio1 <- sexratio %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    rowwise() %>%
    mutate(X2018,X2018 = mean(c(X2015,X2016),na.rm = TRUE)) %>%
    mutate(X2006,X2006 = mean(c(X2008,X2009),na.rm = TRUE)) %>%
    gather("Year","sexratio",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, sexratio)

time1 <- time %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","time_days",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, time_days)

EODB_index1 <- EODB_index %>%
    select (Jurisdiction, X2018:X2006) %>%
    gather("Year","EODB_index",2:14) %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Jurisdiction, Year, EODB_index) %>%
    mutate(Country, Country = str_trim(Country)) %>%
    arrange(desc(Year)) %>%
    arrange(Country) %>%
    rowwise() %>%
    mutate(EODB_index, EODB_index = ifelse(EODB_index!= "N/A", parse_number(EODB_index),NA ))


#Merge
Xian <- cost1
Xian <- merge(Xian,gdp1,by = c("Country","Year"), all.y = TRUE)
Xian <- merge(Xian,procedures1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,time1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,sexratio1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,nb1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,LowerS1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,percent_UP1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,EODB_index1,by = c("Country","Year"),all.x = TRUE)


#REPLACE NA VALUES AND REMOVE OTHER JUNK

Xian1 <- Xian %>%
    filter(Country %in% countries$V1 )  %>%
    arrange(desc(Year)) %>%
    arrange(Country) 

Xian <- Xian1 %>%
    mutate(index = seq(1,length(Xian1$Year),1)) %>%
    filter(index <= 2080 ) %>%
    select(index,Country,Code,Year,everything()) 


### Cost
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(Cost)), bestval = "") %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
#for few costs missing
for (i in 1:length(Xian$Cost)) {
    if (is.na(Xian$Cost[i]) == TRUE) {
        if (Xian$Country[i] == Xian$Country[i-1]){
            Xian$Cost[i] = Xian$Cost[i-1]}
    }
}
#For all costs missing
Xian <- Xian %>%
    mutate(Country, Cost = ifelse(Country == "Liechtenstein","Medium High",Cost ))

### GDP
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(Log_GDP)), bestval = max(Log_GDP, na.rm = TRUE))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(Log_GDP) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( Log_GDP, Log_GDP = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , Log_GDP , Log_GDP) ) ) 



### Procedures
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(procedures)), bestval = max(procedures, na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(procedures) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( procedures, procedures = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , procedures, procedures) ) ) 



### Time_days
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(time_days)), bestval = mean(time_days, na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(time_days) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( time_days, time_days = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , time_days, time_days) ) ) 


### sexratio
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(sexratio)), bestval = mean(Xian$sexratio, na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(sexratio) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( sexratio, sexratio = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , natest$bestval[which(natest$Country==Country)], sexratio) ) ) 


### NB and convert to category
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(NB)), bestval = mean(NB,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(NB) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( NB, NB = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$NB,na.rm = TRUE), NB) ) )  %>%
    mutate(NB, NB = ifelse(NB>20000, "High",ifelse(NB>1000,"Medium","Low"))) 


### Lower Secondary
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(LowerSecondary)), bestval = max(LowerSecondary,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(LowerSecondary) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( LowerSecondary, LowerSecondary = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$LowerSecondary,na.rm = TRUE), LowerSecondary) ) ) 


### Percent Urban Population
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(Percent_Urban_Population)), bestval = max(Percent_Urban_Population,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(Percent_Urban_Population) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( Percent_Urban_Population, Percent_Urban_Population = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$Percent_Urban_Population,na.rm = TRUE), Percent_Urban_Population) ) ) 


### EODB_Index
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(EODB_index)), bestval = max(EODB_index,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(EODB_index) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( EODB_index, EODB_index = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$EODB_index,na.rm = TRUE), EODB_index) ) )

### Generate outcome variables
Xian$Y <- as.numeric(cut(Xian$EODB_index, 2))
Xian <- Xian  %>%
    select(-HasNA,-Method,-index) 


#Generate for 2019
Copy2019 <- Xian %>%
    filter(Year == "2018") %>%
    mutate(Year = "2019")

Xian <- bind_rows(Xian,Copy2019)

Xian <- Xian %>%
    arrange(desc(Year)) %>%
    arrange(Country) 

Xian <- cbind(index = seq(1,length(Xian$Year),1),Xian)

#str(Xian)


#Test and train data
train_data <- Xian %>%
    filter(Year != "2019") %>%
    mutate(Country, Country = as.character(Country)) %>%
    select(-index,-Code,-Year,-EODB_index)

test_data <- Xian %>%
    filter(Year == "2019") %>%
    mutate(Country, Country = as.character(Country)) %>%
    select(-index,-Code,-Year,-EODB_index)


#Recipe for baking
rec_obj <- recipe(Y ~ ., data = train_data) %>%
    step_dummy(Country, one_hot = TRUE) %>%
    step_dummy(Cost, one_hot = TRUE) %>%
    step_dummy(NB, one_hot = TRUE) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep(data = train_data)

#Bake both sets for predictors
Xtrain <- bake(rec_obj, new_data = train_data) %>% select(-Y) 
Xtest  <- bake(rec_obj, new_data = test_data) %>% select(-Y) 

#str(Xtrain)
#glimpse(Xtrain)
# Response variables both sets
Ytrain <- to_categorical(train_data$Y)
Ytest  <- to_categorical(test_data$Y)

#Note Length of target labels is 11 after OHE due to having novalue also as a column 



# Building our Artificial Neural Network
model <- keras_model_sequential()

model <- model %>% 
    # First hidden layer
    layer_dense(units= 80, activation= "relu", kernel_initializer = "uniform",input_shape = ncol(Xtrain)) %>% 
    #layer_dense(units= 16, activation= "relu", kernel_initializer = "uniform") %>% 
    # Output layer
    layer_dense(units= 3, kernel_initializer = "uniform",activation = "softmax") 


# Compile ANN
model <- model %>%  compile(optimizer = 'adam', loss  = 'binary_crossentropy', metrics   = 'accuracy' )


# Store the fitting history in `history` 
history <- model %>% fit(
    as.matrix(Xtrain), 
    as.matrix(Ytrain), 
    epochs = 20,
    batch_size = 32 , 
    validation_split = 0.2
)

# Plot the history
#plot(history)




# Predict the classes for the test data
pred <- model %>% predict_classes(as.matrix(Xtest))

# Confusion matrix
table(actual=test_data$Y, pred)


#Predict probabilities
YProb  <- predict_proba(object = model, x = as.matrix(Xtest))

colnames(YProb) <- c("none","easy","noteasy")

YProb <- as.data.frame(YProb)
YProb <- YProb %>%
    rowwise() %>%
    mutate(asdad =  max(none,easy,noteasy)) 

YProb$Predicted = as.vector(pred)
YProb$Country = test_data$Country


Top_1_cat <- YProb %>%
    filter(Predicted == 1) %>%
    arrange(desc(asdad)) %>%
    select(Country)

Top_2_cat <- YProb %>%
    filter(Predicted == 2) %>%
    arrange(asdad) %>%
    select(Country)

OutputRank <- as.data.frame(bind_rows(Top_1_cat, Top_2_cat))
OutputRank <- OutputRank %>%
    mutate(Rank = seq(1,length(OutputRank$Country),1))
OutputRank <- inner_join(OutputRank,latitude,by = "Country")


header <- dashboardHeader(
    title = span("Start-Up Global",style = "font-size:25px"),
    titleWidth = 300
)

sidebar <- dashboardSidebar(sidebarMenuOutput('menu'))


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
                    box(title = span('Feature Table for 2019',style="font-size:20px"),tableOutput("table1"), height = 150,solidHeader = TRUE,width = 8,tags$head(tags$style('#table1{font-size:9px;}'))),
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
                    box(title = span('Feature Table for 2019',style="font-size:20px"),tableOutput("table2"), height = 150,solidHeader = TRUE,width = 8,tags$head(tags$style('#table2{height:15px;font-size:9px;}')))),
                fluidRow( 
                    box(title = span('Map',style="font-size:20px"),leafletOutput("plot9"), height= 470,solidHeader = TRUE,width = 4,background = "black"),
                    box(title = span('Urban Population Percentage',style="font-size:20px"),plotOutput("plot7"), height= 470,solidHeader = TRUE,width = 4,background = "black"),
                    box(title = span('GDP Trend',style="font-size:20px"),plotOutput("plot8"), height= 470,solidHeader = TRUE,width = 4,background = "black"))
                
        )
    ))


ui <- dashboardPage(header,sidebar,body,title = "Start-Up Global")





server <- function(input, output){
    
    output$menu<-renderMenu({sidebarMenu(menuItem(text=span('Home',style="font-size:20px"),tabName = 'hp'),menuItem(text= span('Country Measure',style="font-size:20px"),tabName = "countryselect"),menuItem(text= span('Rank Measure',style="font-size:20px"),tabName = "rankselect")) })  
    
    
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















